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
  mutate(Y = as.integer(Attrition)-1) %>% 
  select(-Attrition)


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
  
  # => regression logit
  logit <- glm(as.factor(Y)~.,data=DB_train,family="binomial")
  PREV[blocs==ii,"logit"] <- predict(logit,DB_eval,type="response")
  
  # => algo backward AIC
  #algo2 <- step(logit,trace=0)
  #PREV[blocs==ii,"aic"] <- predict(algo2,DB_eval,type="response")
  
  ##### algo backward bis (BIC)
  # algo3 <- step(logit,k=log(nrow(donA)),trace=0)
  # PREV[blocs==ii,"bic"] <- predict(algo3,donT,type="response")
  
  #####methode3
  arbre_mdl <- rpart(as.factor(Y)~.,data=DB_train)
  PREV[blocs==ii,"arbre"] <- predict(arbre_mdl,DB_eval,type="prob")[,2]
  
  rf_mdl <- randomForest(as.factor(Y)~.,data=DB_train)
  PREV[blocs==ii,"foret"] <- predict(rf_mdl,DB_eval,type="prob")[,2]
  
  ranger_mdl <- ranger(as.factor(Y)~.,data=DB_train, probability = TRUE)
  PREV[blocs==ii,"foretRanger"] <- predict(ranger_mdl,DB_eval)$prediction[,2]
  
  #####ridge
  ridge <- cv.glmnet(XXA,YYA,alpha=0,family="binomial")
  PREV[blocs==ii,"ridge_min"] <- predict(ridge,XXT, s="lambda.min",type="response")
  # PREV[blocs==ii,"rid1se"] <- predict(ridge,XXT,s="lambda.1se",type="response")
  
  #####lasso
  lasso <- cv.glmnet(XXA,YYA,alpha=1,family="binomial")
  # etape2 <- glmnet(XXA,YYA,alpha=1,family="binomial",lambda=lasso$lambda.min)
  PREV[blocs==ii,"lasso_min"] <- predict(lasso,XXT,s="lambda.min",type="response")
  # PREV[blocs==ii,"las1se"] <- predict(lasso,XXT,s="lambda.1se",type="response")
  
  #####elas
  elas_net <- cv.glmnet(XXA,YYA,alpha=.5,family="binomial")
  PREV[blocs==ii,"elas_net_min"] <- predict(elas_net,XXT,s="lambda.min",type="response")
  #PREV[blocs==ii,"ela1se"] <- predict(elas,XXT,s="lambda.1se",type="response")
  
  ## Gradient boosting 
  xgb_train = xgb.DMatrix(data = XX_train, label = YY_train)
  xgb_eval = xgb.DMatrix(data = XX_eval, label = YY_eval)
  
  cv <- xgb.cv(data=xgb_train,nrounds=100,
               max_depth=2,nfold=10,
               verbose=0,eta=0.2)
  
  iteropt <- which.min(cv$evaluation_log$test_rmse_mean)
  model_xgboost = xgboost(data = xgb_train, max.depth = 2, 
                          nrounds = iteropt, eta=0.2,
                          verbose = 0, objective = "binary:logistic")
  PREV[blocs==ii,"xgb"] <- predict(model_xgboost,xgb_eval)
  
}



accuracy <- function(X,Y,seuil=0.5){
  Xc=X*0
  Xc[X>seuil] <- 1
  sum(Xc==Y)/length(Y)*100
}



apply(PREV,2,accuracy,Y=PREV$Y)



logit_vf <- glm(as.factor(Y)~.,data=DB,family="binomial")
EST[,"logit"] <- predict(logit_vf,DB,type="response")


apply(EST,2,accuracy,Y=EST$Y)


auctout <- roc(Y~.,data=PREV)
plot(auctout$log)

for(ii in 2:length(auctout)){
  plot(auctout[[ii]],add=T,col=ii)
}
sapply(auctout,coords,x=0.5,ret=c("threshold","accuracy",
                                  "specificity","sensitivity"
))




