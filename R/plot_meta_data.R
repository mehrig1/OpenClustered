#' Provides plot of meta data for all datasets in the 'data_list'
#'
#' Creates plots summarizing the meta data (n obervations, n features, n clusters, and imbalance) of the 19 datasets in the 'data_list'
#' @param allplots Logical (T or F); If T, returns a 4-panel grid of each plot. If F, returns a list of each of the 4 plots as an individual element
#' @param df A list of data frames or vector of data frame names from the data-list. 
#' @return 4-panel figure of metadata summary 
#' @examples 
#' plot_meta_data(allplots=T)
#' @export
#' 
plot_meta_data <- function(allplots=T, df = OpenClustered::data_list){

  #get datasets to summarize
  df_names = names(df)
  
  #subset data to summarize to those included
  df = OpenClustered::meta_data
  df = df[df$dataset %in% df_names,]
  
  #Plot for N obs
  p1 <- ggplot(df, aes(x = n_obs)) +
    geom_histogram(color = "white",
                   fill = "black", boundary = 0) +
    ggtitle("Number of Observations") +
    ylab("Frequency") + xlab("N Observations") +
    theme_bw() +
    geom_hline(yintercept = 0)
  
  #N feature plot
  p2 <- ggplot(df, (aes(x = n_features))) +
    geom_histogram(
      color = "white",
      fill = "black",boundary = 0) +
    ggtitle("Number of Features") +
    ylab("Frequency") +
    xlab("Number of Features") +
    theme_bw() +
    geom_hline(yintercept = 0) +
    theme(
      axis.text.x.top = element_blank(),
      axis.ticks.x.top = element_blank(),
      axis.line.x.top = element_blank()
    )
  
  #N cluster Plot
  p3 <- ggplot(df, aes(x = n_clusters)) +
    geom_histogram(
      color = "white",
      fill = "black",boundary = 0) +
    ggtitle("Number of Cluster Units") +
    ylab("Frequency") + xlab("Clusters") +
    theme_bw() +
    geom_hline(yintercept = 0)
  
  #Imbalance Plot
  p4 <- ggplot(df, (aes(x = imbalance))) +
    geom_histogram(
      color = "white",
      fill = "black",boundary = 0) +
    ggtitle("Imbalance in Minority Class") +
    ylab("Frequency") +
    xlab("Imbalance") +
    theme_bw() +
    geom_hline(yintercept = 0) +
    scale_y_continuous(breaks=c(1,2))
  
  if(allplots==T){
    #Create combined plot
    grid.arrange(p1, p2, p3, p4,
                 layout_matrix = matrix(c(1, 2, 3, 4),
                                        ncol = 2,
                                        byrow = T))
  }
  else {
    return(
      list(n_obs_plot = p1, 
           n_feat_plot = p2,
           n_cluster_plot = p3,
           imb_plot = p4)
    )
  }
}
