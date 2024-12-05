devtools::install_github("https://github.com/NateOConnellPhD/OpenClustered")

library(OpenClustered)

devtools::install()

vis_datasets_smry()

filter_data(n_features>10, n_obs>2500)

library(roxygen2); # Read in the roxygen2 R package
roxygenise();      # Builds the help files

library(devtools);
load_all("."); # Working directory should be in the package SCC_R_package

