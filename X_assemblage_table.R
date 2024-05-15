# Pour s'assurer de la cohérence de la table DB 

X1_construction_base <- function(para_file, para_forme_ls){
  
  if (any(grepl("simple", forme_dt_ls))){
    DB <- para_file
    print("La table contient les variables simples")
  }
  
    DB_poly <- var_carre
    DB <- DB %>% cbind(DB_poly)
    print("La table au carré")
  }
  
  return(DB)
}

