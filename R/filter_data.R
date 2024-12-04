#' Filter datasets based on criteria in the metadata file 
#'
#' Gives list of datasets meeting criteria specified in the function. 
#' @param ... dplyr `filter()' syntax for filtering data. Should be based on columns in the meta_data file
#' @return a vector of dataset names meeting the filtered criteria 
#' @examples 
#' subset_data = filter_data(n_features>10, n_obs>2500)
#' @export
#' 
filter_data <- function(...) {
  datasets$meta_data %>%
    filter(...) %>%
    select(dataset) %>%
    unlist()
}
