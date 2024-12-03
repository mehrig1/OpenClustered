#' Visualize summary characteristics of all available datasets 
#'
#' Creates a 4-panel figure of summary statistics of the dataset meta data, summarizes the total number of observations, number of features, number of clusters and imbalance. 
#' @param df the meta data file one returned from get_data("meta_data.csv");
#' @return 4-panel figure of metadata summary 
#' @examples 
#' meta_data = get_data("meta_data")
#' vis_datasets_smry(meta_data)
#' @export
vis_datasets_smry <- function(df){
  
  #Create plot for Coef of Variation
  p2 <- ggplot(df, (aes(x = n_features))) +
    geom_histogram(binwidth = 1,
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
  
  #Plot for N:P
  p1 <- ggplot(df, aes(x = n_obs)) +
    geom_histogram(color = "white",
                   fill = "black", boundary = 0) +
    ggtitle("Number of Observations") +
    ylab("Frequency") + xlab("N Observations") +
    theme_bw() +
    geom_hline(yintercept = 0)
  
  #create plot for number of features
  p3 <- ggplot(df, aes(x = n_clusters)) +
    geom_histogram(
                   color = "white",
                   fill = "black",boundary = 0) +
    ggtitle("Number of Cluster Units") +
    ylab("Frequency") + xlab("Clusters") +
    theme_bw() +
    geom_hline(yintercept = 0)
  
  #Create plot for number of instances
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
  
  #Create combined plot
  grid.arrange(p1, p2, p3, p4,
               layout_matrix = matrix(c(1, 2, 3, 4),
                                      ncol = 2,
                                      byrow = T))
}
