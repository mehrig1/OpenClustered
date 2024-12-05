#devtools::install_github("https://github.com/NateOConnellPhD/OpenClustered")

#Install package
devtools::install()
library(OpenClustered)

#Testing functions
x <- filter_data(n_features>10, n_obs>2500, subset=T)

plot_meta_data(allplots=T)


library(roxygen2); # Read in the roxygen2 R package
roxygenise();      # Builds the help files

## Create file for meta data description
# usethis::use_r("meta_data")
# usethis::use_r("data_list")

