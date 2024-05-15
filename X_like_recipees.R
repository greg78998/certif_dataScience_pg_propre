fc_transform_col <- function(para_DB, para_col, para_seuil){
  
  # Fonction qui transforme une variable qualitative avec plusieurs modalités
  #
  # @ para_DB : DataFrame sur lequel on travaille
  # @ para_col : Nom de la colonne que l'on va traiter (comme chaîne de caractères)
  # @ para_seuil : Seuil de pourcentage pour garder les modalités fréquentes
  
  # Table de pourcentages des valeurs uniques de la colonne
  tab2 <- data.frame(
    Var1 = names(table(para_DB[[para_col]])), 
    Freq = round(table(para_DB[[para_col]]) / nrow(para_DB) * 100, 2)
  )
  
  # Filtrer les valeurs qui dépassent le seuil
  tab3 <- tab2 %>% dplyr::filter(Freq.Freq > para_seuil*100)
  
  # Créer une nouvelle colonne en catégorisant les valeurs
  para_DB[[paste0(para_col, "_ret")]] <- ifelse(
    para_DB[[para_col]] %in% tab3$Var1,
    para_DB[[para_col]],
    "Autres"
  )
  
  # Supprimer la colonne originale
  para_DB <- para_DB[, !(names(para_DB) %in% para_col)]
  
  return(para_DB)
}


mise_au_carre <- function(para_DB,
                          para_col){
  
  # @ para_DB : le dataframe qui sert d'input
  # @ para_col : les colonnes que l'on selection
  
  para_db_0 <- para_DB %>% select(para_col)
  para_db_0 <- para_db_0^2
  colnames(para_db_0) <- paste(colnames(para_db_0),"_sq",sep="")
  
  return(para_db_0)
}

