#' Filter datasets based on criteria in the metadata file 
#'
#' Gives list of datasets meeting criteria specified in the function. 
#' @param ... dplyr `filter()' syntax for filtering data. Should be based on columns in the meta_data file
#' @param subset Logical (T or F). If T, then function returns list of dataframes matching filter criteria. If F, then function returns a vector of datasets that match filter criteria
#' @return Dependent on 'subset' - A list of datasets matching filter criteria or a vector of dataset names matching filter criteria 
#' @examples 
#' subset_data_list= filter_data(n_features>10, n_obs>2500, subset=T) #returns a list of datasets with >10 features and >2500 observations
#' subset_data_names = filter_data(n_features>10, n_obs>2500, subset=T) #returns a vector of dataset names matching the filter criteria
#' @export
#' 
filter_data <- function(..., subset=T) {
  subset_names = OpenClustered::meta_data %>%
                      filter(...) %>%
                      select(dataset) %>%
                      unlist()
  
  names(subset_names) = subset_names
  
  if(subset==T){
    return(OpenClustered::data_list[subset_names])
  } else {
    return(subset_names)
  }
}


