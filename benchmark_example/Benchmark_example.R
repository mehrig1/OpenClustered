library(OpenClustered)
library(lme4)
library(parallel)
library(missForest)
library(randomForest)
library(pROC)
library(brms)


#Function to remove factors with more than 25 levels
remove_high_level_factors <- function(data, max_levels = 25, ignore_columns = NULL) {
  # Ensure input is a data frame
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }
  
  # Validate ignore_columns
  if (!is.null(ignore_columns)) {
    if (!all(ignore_columns %in% colnames(data))) {
      stop("Some columns in ignore_columns do not exist in the data frame.")
    }
  }
  
  # Identify columns to keep
  cols_to_keep <- sapply(names(data), function(col_name) {
    col <- data[[col_name]]
    # Skip the check for ignored columns
    if (!is.null(ignore_columns) && col_name %in% ignore_columns) {
      return(TRUE)
    }
    # Check if the column is a factor or character
    if (is.factor(col) || is.character(col)) {
      # Check the number of unique levels
      return(length(unique(col)) <= max_levels)
    }
    # Keep other column types
    TRUE
  })
  
  # Subset the data frame to keep only the desired columns
  data_filtered <- data[, cols_to_keep, drop = FALSE]
  
  return(data_filtered)
}


convert_char_to_factor <- function(df) {
  df[] <- lapply(df, function(x) if(is.character(x)) factor(x) else x)
  return(df)
}


#set vector of responses
results = list()
results$lmm = list()
results$blmm = list()
#results$auc_comparison = list()

options(mc.cores = parallel::detectCores())  # Detect available cores
options(mc.cores = 4)  # Use 4 cores for parallel processing


auc_results <- data.frame(
  Dataset = character(),
  GLMM_AUC = numeric(),
  GLMM_CI_Lower = numeric(),
  GLMM_CI_Upper = numeric(),
  BRMS_AUC = numeric(),
  BRMS_CI_Lower = numeric(),
  BRMS_CI_Upper = numeric(),
  RF_AUC = numeric(),
  RF_CI_Lower = numeric(),
  RF_CI_Upper = numeric(),
  stringsAsFactors = FALSE
)

for (i in 1:length(data_list)) {
  
  names(data_list)
  data_name <- names(data_list[i])
  temp_df <- data_list[[i]]
  
  # Make outcome variable a factor
  temp_df$target <- factor(temp_df$target)
  temp_df <- convert_char_to_factor(temp_df)
  temp_df <- remove_high_level_factors(temp_df, max_levels = 25, ignore_columns = "cluster_id")
  
  # Impute missing data if missing
  if (nrow(temp_df) != nrow(temp_df[complete.cases(temp_df), ])) {
    temp_df <- convert_char_to_factor(temp_df)
    imp_df <- missForest::missForest(temp_df)
    temp_df <- imp_df$ximp
  }
  
  # Dummy code factor variables
  # Convert all variables except 'cluster_id' to dummy variables
  temp_df_numeric <- as.data.frame(model.matrix(~ . - 1, data = temp_df %>% select(-cluster_id)))
  
  # Remove highly correlated predictors
  corr_matrix <- cor(temp_df_numeric, use = "complete.obs")
  high_corr <- which(abs(corr_matrix) > 0.8 & upper.tri(corr_matrix), arr.ind = TRUE)
  vars_to_drop <- unique(colnames(corr_matrix)[high_corr[, 2]])
  temp_df_numeric <- temp_df_numeric[, !names(temp_df_numeric) %in% vars_to_drop]
  
  # Add 'cluster_id' back to the dataframe
  temp_df_numeric$cluster_id <- temp_df$cluster_id
  
  # Rename target variable
  names(temp_df_numeric)[which(grepl("target", names(temp_df_numeric)))[1]] <- "target"
  
  # Remove "+" from variable names
  names(temp_df_numeric) <- gsub("\\+", "", names(temp_df_numeric))
  names(temp_df_numeric) <- gsub("\\-", "", names(temp_df_numeric))
  names(temp_df_numeric) <- gsub("\\ ", "", names(temp_df_numeric))
  names(temp_df_numeric) <- gsub("\\(", "_", names(temp_df_numeric))
  names(temp_df_numeric) <- gsub("\\,", "_", names(temp_df_numeric))
  names(temp_df_numeric) <- gsub("\\)", "", names(temp_df_numeric))
  names(temp_df_numeric) <- gsub("/", "_", names(temp_df_numeric))
  
  # Split dataset into training and testing datasets
  train_ids <- sample(1:nrow(temp_df_numeric), size = round(0.7 * nrow(temp_df_numeric)))
  train_data <- temp_df_numeric[train_ids, ]
  test_data <- temp_df_numeric[-train_ids, ]
  
  form <- as.formula(paste(
    "target ~", 
    paste(names(test_data[names(test_data) %in% c("cluster_id", "target") == FALSE]), collapse = "+"), 
    "+ (1 | cluster_id)"
  ))
  
  ## Fit GLMM
  glmm_auc <- glmm_ci_lower <- glmm_ci_upper <- NA
  tryCatch({
    results$lmm[[i]] <- glmer(
      form, 
      data = train_data,
      family = binomial(link = "logit")
    )
    glmm_preds <- predict(results$lmm[[i]], newdata = test_data, type = "response", allow.new.levels = TRUE)
    glmm_roc <- roc(test_data$target, glmm_preds)
    glmm_auc <- auc(glmm_roc)
    glmm_ci <- ci.auc(glmm_roc)
    glmm_ci_lower <- glmm_ci[1]
    glmm_ci_upper <- glmm_ci[3]
  }, error = function(e) {
    warning(paste("Error in glmer for dataset", data_name, ":", e$message))
    results$lmm[[i]] <- NULL
  })
  
  ## Fit Bayesian Model
  brms_auc <- brms_auc <- brms_ci_lower <- brms_ci_upper <- NA
  tryCatch({
    iter <- 3000
    adapt_delta <- 0.96
    max_attempts <- 3
    attempt <- 0
    
    repeat {
      attempt <- attempt + 1
      
      results$blmm[[i]] <- brm(
        form,
        data = train_data,
        family = bernoulli(),
        chains = 4, 
        cores = 4,
        iter = iter,
        control = list(adapt_delta = adapt_delta)
      )
      
      rhat_values <- summary(results$blmm[[i]])$fixed$Rhat
      
      if (all(rhat_values <= 1.2, na.rm = TRUE) || attempt >= max_attempts) {
        break
      } else {
        iter <- iter * 2
        adapt_delta <- adapt_delta + 0.01
      }
    }
    
    blmm_preds <- posterior_epred(results$blmm[[i]], newdata = test_data, allow_new_levels = TRUE)
    blmm_mean_preds <- colMeans(blmm_preds)
    blmm_roc <- roc(test_data$target, blmm_mean_preds)
    brms_auc <- auc(blmm_roc)
    brms_ci <- ci.auc(blmm_roc)
    brms_ci_lower <- brms_ci[1]
    brms_ci_upper <- brms_ci[3]
  }, error = function(e) {
    warning(paste("Error in brm for dataset", data_name, ":", e$message))
    results$blmm[[i]] <- NULL
  })
  
  ## Fit Random Forest Model (with ntree = 2000)
  rf_auc <- rf_ci_lower <- rf_ci_upper <- NA
  tryCatch({
    rf_model <- randomForest(form, data = train_data, importance = TRUE, ntree = 2000)
    rf_preds <- predict(rf_model, newdata = test_data, type = "response")
    rf_roc <- roc(test_data$target, rf_preds)
    rf_auc <- auc(rf_roc)
    rf_ci <- ci.auc(rf_roc)
    rf_ci_lower <- rf_ci[1]
    rf_ci_upper <- rf_ci[3]
  }, error = function(e) {
    warning(paste("Error in Random Forest for dataset", data_name, ":", e$message))
    rf_auc <- rf_ci_lower <- rf_ci_upper <- NA
  })
  
  # Append results to the dataframe
  auc_results <- rbind(
    auc_results,
    data.frame(
      Dataset = data_name,
      GLMM_AUC = glmm_auc,
      GLMM_CI_Lower = glmm_ci_lower,
      GLMM_CI_Upper = glmm_ci_upper,
      BRMS_AUC = brms_auc,
      BRMS_CI_Lower = brms_ci_lower,
      BRMS_CI_Upper = brms_ci_upper,
      RF_AUC = rf_auc,
      RF_CI_Lower = rf_ci_lower,
      RF_CI_Upper = rf_ci_upper,
      stringsAsFactors = FALSE
    )
  )
  
  print(paste("Finished AUC comparison for dataset:", data_name))
}


saveRDS(results, file="results.rds")
saveRDS(auc_results, file="auc_results.rds")
results
auc_results



# Load necessary libraries
library(ggplot2)
library(tidyr)
library(dplyr)

# Reshape data to long format
df_long <- auc_results %>% 
  mutate(
    GLMM_AUC = as.numeric(GLMM_AUC),
    BRMS_AUC = as.numeric(BRMS_AUC)
  ) %>%
  pivot_longer(
    cols = c(GLMM_AUC, BRMS_AUC),
    names_to = "Model",
    values_to = "AUC"
  ) %>%
  mutate(
    CI_Lower = ifelse(Model == "GLMM_AUC", GLMM_CI_Lower, BRMS_CI_Lower),
    CI_Upper = ifelse(Model == "GLMM_AUC", GLMM_CI_Upper, BRMS_CI_Upper),
    Model = gsub("_AUC", "", Model) # Clean model names
  )

# Plot
ggplot(df_long, aes(x = AUC, y = Dataset, color = Model)) +
  geom_point(position = position_dodge(width = .6), size = 3) +
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper),
                 position = position_dodge(width = .6), height = 0.2) +
  labs(
    title = "AUC Estimates with 95% Confidence Intervals",
    x = "AUC",
    y = "Dataset",
    color = "Model"
  ) +
  theme_minimal() + coord_flip()
