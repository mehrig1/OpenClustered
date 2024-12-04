devtools::install_github("https://github.com/NateOConnellPhD/OpenClustered")

library(OpenClustered)

vis_datasets_smry()



datasets$meta_data %>% filter(n_features>5) %>% select(dataset)

datasets$
