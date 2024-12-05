#devtools::install_github("https://github.com/NateOConnellPhD/OpenClustered")

devtools::install()
library(OpenClustered)
vis_datasets_smry()

x <- filter_data(n_features>10, n_obs>2500, subset=T)
x


library(roxygen2); # Read in the roxygen2 R package
roxygenise();      # Builds the help files

#Create file for meta data descriptin
usethis::use_r("meta_data")
usethis::use_r("data_list")

