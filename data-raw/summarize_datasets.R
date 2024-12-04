#' Metadata file creation
#'
#' Pull all datasets from "data/" file, summarize them, and write them to new file "data_meta.csv"
#' @return A new datafile saved in the "data/" folder containing up-to-date meta data on all data sets
#' @examples 
#' get_meta_data() 
#' @export
get_meta_data = function(){
  # extract file names from folder
  files = list.files("data/")
  files = files[grepl("info|meta", files)==F] #get rid of info file
  
  # create function to get info and fill into df_info 
  get_info = function(x){
    temp_df = read.csv(paste("data/", x, sep=""))
    dataset <- gsub(".csv", "",x)
    n_obs = nrow(temp_df)
    n_features = ncol(temp_df) - 2
    n_clusters = length(unique(temp_df$cluster_id))
    n_classes = length(unique(temp_df$target))
    imbalance = round(min(prop.table(table(temp_df$target))),2)
    task = "Classification"
    missing_obs = sum(complete.cases(temp_df))
    data.frame(dataset, n_obs, n_features, n_clusters,n_classes, imbalance, task, missing_obs)
  }
  
  data_meta = do.call("rbind",lapply(files, get_info))
  data_meta = data_meta[order(as.numeric(gsub("dat","",data_meta$dataset))),]
  
  write.csv(data_meta, "data/data_meta.csv", row.names=F)
}


