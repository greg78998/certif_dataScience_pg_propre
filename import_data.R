
path <- "/Users/gregoirehaniquaut/Desktop/bdf_projet/master"
# les données sont ici 

setwd(paste0(path,"/_data"))


data_ori <- read.table("adult.data",header = FALSE, sep = ",",na.strings = "?",stringsAsFactors = TRUE)

don_ori <- data_ori
