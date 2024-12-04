devtools::install_github("https://github.com/NateOConnellPhD/OpenClustered")

library(OpenClustered)

vis_datasets_smry()

unlist(datasets$meta_data %>% filter(n_features>5) %>% select(dataset))
datasets[test]

names(datasets)

filter_data <- function(...) {
  datasets$meta_data %>%
    filter(...) %>%
    select(dataset) %>%
    unlist()
    
}

filter_data(n_features>10, n_obs>2500)
