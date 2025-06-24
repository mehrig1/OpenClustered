
#' Information and meta data on the datasets within 'data_list'
#' 
#' @format A data frame with 19 rows for each dataset in 'data_list'
#' \describe{
#'   \item{dataset}{The general dataset name within the R package}
#'   \item{dataset_name}{Original dataset name from the original source} 
#'   \item{outcome}{The original outcome variable. Note: outcome variables within data_list are commonly renamed to 'target'}
#'   \item{domain}{The domain/area of work the dataset applies to}
#'   \item{sim_or_real}{Is the dataset simulated data or real data}
#'   \item{origin}{The origin of the dataset}
#'   \item{n_obs}{Number of observations in the dataset}
#'   \item{n_features}{Number of features in the dataset}
#'   \item{n_clusters}{Number of unique cluseters within the clustering variable}
#'   \item{n_classes}{Number of outcome classess in the outcome}
#'   \item{imbalance}{Outcome imbalance represented by proportion of the minoirty class}
#'   \item{task}{The task (i.e. classification) primarily for the dataset}
#'   \item{missing_obs}{The number of rows with at least one missing variable in the dataset}
#' }


"meta_data"

#Jaime was here

