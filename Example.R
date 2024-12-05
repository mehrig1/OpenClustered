# Install package from github
devtools::install_github("https://github.com/NateOConnellPhD/OpenClustered")

# load package
library(OpenClustered)

# View Meta Data files
head(OpenClustered::meta_data)

# View meta data characteristics of all datasets in `data_list`
plot_meta_data(allplots=T)

# Summarize Meta Data (using r package "table1")
tab_meta_data(~n_obs + n_features + n_clusters + imbalance + missing_obs + 
                domain + sim_or_real)

# Subset data_list to datasets with >5000 observations and in the domain of 'linguistics'
ling_data = filter_data(n_obs >=1000, domain=="linguistics", subset=T)

# view characteristics of new data
plot_meta_data(df = ling_data)

# Summarize the list of linguistic datasets with >=1000 observations
tab_meta_data(~n_obs + n_features + n_clusters + imbalance + missing_obs + 
                domain + sim_or_real, df= ling_data)

### Develop a Logistic Prediction Model on one of the datasets (dat12) in Linguistics 
library(lme4)
library(pROC)
names(ling_data$dat12)
# Summarize dat12 using Table 1
table1::table1(~Modality + SemanticClass + LengthOfRecipient + 
                 AnimacyOfRec+ + DefinOfRec + PronomOfRec+LengthOfTheme+ AnimacyOfTheme+
                 DefinOfTheme+PronomOfTheme+AccessOfRec+AccessOfTheme, data=ling_data$dat12)

# Assess the cluster Variable
table(ling_data$dat12$cluster_id)

# relevel all clusters levels with less than `threshold' observations into their own category
relevel_to_other <- function(factor_var, threshold) {
  # Get frequency counts for each level
  level_counts <- table(factor_var)
  
  # Identify levels to be grouped into "other"
  levels_to_other <- names(level_counts[level_counts < threshold])
  
  # rename variables in levels_to_other to "other"
  factor_var = ifelse(factor_var %in% levels_to_other, "other", as.character(factor_var))
  return(factor_var)
}

ling_data$dat12$cluster_id <- relevel_to_other(as.factor(ling_data$dat12$cluster_id), threshold = 30)
table(ling_data$dat12$cluster_id)

# Split Dataset by single split into training and testing datasets
train_ids <- sample(1:nrow(ling_data$dat12), size = round(.7 * nrow(ling_data$dat12)))
train_data <- ling_data$dat12[train_ids, ]  # Training set
test_data <- ling_data$dat12[-train_ids, ]  # Testing set

#Fit Mixed Model
fit = glmer(target == "PP" ~ Modality + SemanticClass + LengthOfRecipient + 
             AnimacyOfRec+ + DefinOfRec + PronomOfRec+LengthOfTheme+ AnimacyOfTheme+
             DefinOfTheme+PronomOfTheme+AccessOfRec+AccessOfTheme+(1|cluster_id), data=train_data,
            family=binomial(link="logit"))

# Summarize Mixed Model
summary(fit)

# Predict 
test_data$predicted_prob <- predict(fit, newdata = test_data, type = "response")

# Compute AUC
auc_result <- pROC::roc(response =test_data$target, predictor = test_data$predicted_prob)
auc_result

