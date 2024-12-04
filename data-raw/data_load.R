library(usethis)
library(here)

files = list.files("data-raw/")
files = files[grepl("load", files)==F] #get rid of info filefile_names = gsub(".csv", "", files)

for(i in 1:length(files)){
  temp =  read.csv(paste("data-raw/", files[i], sep=""))
  usethis::use_data(temp, overwrite=T)
  file.rename("data/temp.rda", paste("data/",file_names[i], ".rda", sep=""))
}



