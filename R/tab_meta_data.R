#' Provides a table of descriptive statistics for the meta data of the datasets provided
#'
#' A wrapper function using the 'table1' package that provides summary data characteristics of a given list of data sets
#' @param formula A formula specifying which meta data characteristics to summarize. In the form of "~ x + y + z +..."
#' @param df List of data frames or vector of data frame names to summarize
#' @return An HTML table from the 'table1' package summarizing the meta data of the supplied data list
#' @examples 
#' plot_meta_data(allplots=T)
#' @export
#' 
#' 

tab_meta_data <- function(formula, df = data_list) {
  #get names of the data_list to be summarize 
  df_names <- names(df)
  
  # Subset data to summarize to those included
  temp_data <- OpenClustered::meta_data
  temp_data <- temp_data[temp_data$dataset %in% df_names, ]
  
  #Label meta data
  Hmisc::label(temp_data$n_obs) = "Number of Observations"
  Hmisc::label(temp_data$n_features) = "Number of Features"
  Hmisc::label(temp_data$missing_percent) = "Rows with Missing Data"
  Hmisc::label(temp_data$n_clusters) = "Number of Clusters"
  Hmisc::label(temp_data$imbalance) = "Imbalance"
  Hmisc::label(temp_data$missing_obs) = "Number of Missing Observations"
  
  # Generate the table using the provided formula
  table1::table1(data = temp_data, formula)
}


