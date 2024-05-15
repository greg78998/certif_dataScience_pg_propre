matricule <- "N818398"

if (matricule == "N818398") {
  path_USER <- paste0("C:/Users/",matricule,"/Desktop/projetBdF", sep = "")
}

# 0 | chargement des libraries  -----

path_pg <- paste0(path_USER,"/pg_pipeline/", sep = "") 

source(paste0(path_pg,"//_before_chemins.R"))
source(paste0(path_pg,"//_before_libraries.R"))

ls_pg <- list.files(path = path_pg, pattern = "X_")

for (prog in ls_pg){
  source(paste0(path_pg,"//", prog, sep = ""))
}
# Import de la table

forme_dt_ls <- c("simple", "poly")

db_ret <- data_ori

db_var_carre <- mise_au_carre(para_DB = data_ori, para_col = c("MonthlyIncome","Age"))



