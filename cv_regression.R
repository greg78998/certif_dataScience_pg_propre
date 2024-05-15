# Creation des blocs 

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
DB_gen <- X1_construction_base(para_file = db_ret, para_forme_ls = forme_dt_ls)
nb <- 10
perc_training  <- 0.8

# Creation de la variable target 
DB <- DB_gen %>% 
  rename(Y = MonthlyIncome) 


need <- TRUE 

# pour assurer la reproductivité
if (need){
  set.seed(1234)
  
  # pour distinguer le training du test
  # blocs_training_test <- sample(rep(1:10, length = nrow(DB)))<=(perc_training*10)
  
  
  # pour distinguer le train de l'eval
  blocs <- sample(rep(1:nb,length=nrow(DB)))
}


PREV <- DB %>% 
  select(Y)

XX <- model.matrix(Y~., data = DB)
YY <- DB$Y

for(ii in 1:nb){
  
  print(paste0("Tour de piste : ",ii))
  
  DB_train <- DB[blocs!=ii,]
  DB_eval <- DB[blocs==ii,]
  
  XX_train <- XX[blocs!=ii,]
  XX_eval <- XX[blocs==ii,]
  YY_train <- YY[blocs!=ii]
  YY_eval <- YY[blocs==ii]
  
  ###methode1
  lm_mdl <- lm(Y~.,data=DB_train)
  PREV[blocs==ii,"lm"] <- predict(lm_mdl,DB_eval)
  
  # algo backward AIC
  #algo2 <- step(lm_mdl,trace=0)
  # PREV[blocs==ii,"aic"] <- predict(algo2,donT)
  ##### algo backward bis (BIC)
  # algo3 <- step(logit,k=log(nrow(donA)),trace=0)
  # PREV[blocs==ii,"bic"] <- predict(algo3,donT)
  
  #####methode3
  arbre_mdl <- rpart(Y~.,data=DB_train)
  PREV[blocs==ii,"arbre"] <- predict(arbre_mdl,DB_eval)
  rf_mdl <- randomForest(Y~.,data=DB_train)
  PREV[blocs==ii,"foret"] <- predict(rf_mdl,DB_eval)
  ranger_mdl <- ranger(Y~.,data=DB_train, probability = FALSE)
  PREV[blocs==ii,"foretRanger"] <- predict(ranger_mdl,DB_eval)$prediction
  
  #####ridge
  ridge <- cv.glmnet(XX_train,YY_train,alpha=0,family="gaussian")
  PREV[blocs==ii,"ridmin"] <- predict(ridge,XX_eval,s="lambda.min")
  # PREV[blocs==ii,"rid1se"] <- predict(ridge,XXT,s="lambda.1se")
  
  #####lasso
  lasso <- cv.glmnet(XX_train,YY_train,alpha=1,family="gaussian")
  PREV[blocs==ii,"lasmin"] <- predict(lasso,XX_eval,s="lambda.min")
  # PREV[blocs==ii,"las1se"] <- predict(lasso,XXT,s="lambda.1se")
  
  #####elas
  elas_net <- cv.glmnet(XX_train,YY_train,alpha=.5,family="gaussian")
  PREV[blocs==ii,"elamin"] <- predict(elas_net,XX_eval,s="lambda.min")
  #PREV[blocs==ii,"ela1se"] <- predict(elas,XXT,s="lambda.1se")
  
  ## Gradient boosting 
  xgb_train = xgb.DMatrix(data = XX_train, label = YY_train)
  xgb_test = xgb.DMatrix(data = XX_eval, label = YY_eval)
  
  cv <- xgb.cv(data=xgb_train,nrounds=300,max_depth=2,nfold=10,verbose=0,eta=0.1)
  iteropt <- which.min(cv$evaluation_log$test_rmse_mean)
  print(iteropt)
  model_xgboost = xgboost(data = xgb_train, max.depth = 2, 
                          nrounds = iteropt, eta=0.1,
                          verbose = 0)
  PREV[blocs==ii,"xgb"] <- predict(model_xgboost,xgb_test)
  
}

erreur <- function(X,Y){mean((X-Y)^2)}

apply(PREV,2,erreur,Y=PREV$Y)



EST <- data.frame(Y=don$maxO3)

for(ii in 1:3){
  tmp<-lm(maxO3~.,data=don)
  EST[,"mco"] <- tmp$fitted.values
  tmp2 <- step(tmp,trace=0)
  EST[,"aic"] <- tmp2$fitted.values
  tmp3 <- step(tmp,trace=0,k=log(nrow(don)))
  EST[,"bic"] <- tmp3$fitted.values
}

apply(EST,2,erreur,Y=PREV$Y)




