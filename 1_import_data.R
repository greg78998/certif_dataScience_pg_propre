matricule <- "N818398"

if (matricule == "N818398") {
  path_USER <- paste0("C:/Users/",matricule,"/Desktop/projetBdF", sep = "")
}

# 0 | chargement des libraries  -----

path_pg <- paste0(path_USER,"/pg_pipeline/", sep = "") 

source(paste0(path_pg,"//_before_chemins.R"))
source(paste0(path_pg,"//_before_libraries.R"))


# les données sont ici 


data_ori <- read.csv2(paste0(path_data,"attrition.csv", sep = ""),
                      header = TRUE, 
                      sep = ",",
                      na.strings = "NA",
                      stringsAsFactors = TRUE)



str(data_ori)
apply(data_ori, 2, function(x) sum(is.na(x)))
