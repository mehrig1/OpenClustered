# Install package from github
devtools::install_github("https://github.com/NateOConnellPhD/OpenClustered")

# load package
library(OpenClustered)

# View Meta Data files
head(OpenClustered::meta_data)

# View meta data characteristics of all datasets in `data_list`
plot_meta_data(allplots=T)

# Summarize Meta Data (using r package "table1")
tab_meta_data(~n_obs +  n_features + n_clusters + imbalance + missing_percent)

data_long <- meta_data %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

# Calculate summary statistics
summary_table <- data_long %>%
  group_by(Variable) %>%
  summarise(
    Mean = mean(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE),
    Median = median(Value, na.rm = TRUE),
    Range = paste0(round(min(Value, na.rm = TRUE), 2), " - ", round(max(Value, na.rm = TRUE), 2))
  ) %>%
  ungroup()

# Convert the summary table to a grob
table_grob <- tableGrob(summary_table, rows = NULL)

# Example 4-panel plot (replace with your actual plot)
data_plot <- ggplot(data, aes(x = n_obs, y = n_features)) +
  geom_point() +
  facet_wrap(~n_clusters) +
  theme_minimal()

# Convert the ggplot object to a grob (if necessary)
plot_grob <- ggplotGrob(data_plot)

# Combine the plot and table side by side
combined <- grid.arrange(plot_grob, table_grob, ncol = 2, widths = c(2, 1))

# Display the combined figure
print(combined)






order_datasets <- function(dataset_names) {
  # Extract numeric part after "dat" and order based on that
  sorted_names <- dataset_names[order(as.numeric(sub("dat", "", dataset_names)))]
  return(sorted_names)
}
names(meta_data)
meta_data<- meta_data[order(as.numeric(sub("dat", "", meta_data[["dataset"]]))), ]
meta_data[,c(1:6)]  %>% kableExtra::kbl() %>% kableExtra::kable_styling()

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
test_data$predicted_prob <- predict(fit, newdata = test_data, type = "response",
                                    allow.new.levels=T)

# Compute AUC
auc_result <- pROC::roc(response =test_data$target, predictor = test_data$predicted_prob)
auc_result

