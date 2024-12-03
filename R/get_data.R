
#' Dataset Extraction
#'
#' Extract specific dataset by name. 
#' @param x The dataset/filename as described in 'data_meta.csv" or "data_info.csv"
#' @return The datasset asked for 
#' @examples 
#' data_example <- get_data("dat1");
#' @export

get_data = function(x){
  read.csv(paste("data/", x,".csv", sep=""))
}

