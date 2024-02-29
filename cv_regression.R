# Creation des blocs 

nb <- 4
set.seed(1234)
blocs <- sample(rep(1:nb,length=nrow(don)))
PREV <- data.frame(Y=don$Y)

XX <- model.matrix(Y~., data = don)
YY <- don$Y

for(ii in 1:nb){
  
  print(ii)
  
  donA <- don[blocs!=ii,]
  donT <- don[blocs==ii,]
  
  XXA <- XX[blocs!=ii,]
  XXT <- XX[blocs==ii,]
  YYA <- YY[blocs!=ii]
  YYT <- YY[blocs==ii]
  ### regression logit
  
  ###methode1
  lm_mdl <- lm(Y~.,data=donA)
  PREV[blocs==ii,"log"] <- predict(lm_mdl,donT)
  
  ### algo backward AIC
  # algo2 <- step(logit,trace=0)
  # PREV[blocs==ii,"aic"] <- predict(algo2,donT)
  ##### algo backward bis (BIC)
 bcdwsv  # algo3 <- step(logit,k=log(nrow(donA)),trace=0)
  # PREV[blocs==ii,"bic"] <- predict(algo3,donT)
  
  #####methode3
  arbre_mdl <- rpart(as.factor(Y)~.,data=donA)
  PREV[blocs==ii,"arbre"] <- predict(arbre_mdl,donT)[,2]
  rf_mdl <- randomForest(as.factor(Y)~.,data=donA)
  PREV[blocs==ii,"foret"] <- predict(rf_mdl,donT,type)[,2]
  ranger_mdl <- ranger(as.factor(Y)~.,data=donA, probability = FALSE)
  PREV[blocs==ii,"foretRanger"] <- predict(ranger_mdl,donT)$prediction[,2]
  
  #####ridge
  ridge <- cv.glmnet(XXA,YYA,alpha=0,family="gaussian")
  PREV[blocs==ii,"ridmin"] <- predict(ridge,XXT,s="lambda.min")
  # PREV[blocs==ii,"rid1se"] <- predict(ridge,XXT,s="lambda.1se")
  
  #####lasso
  lasso <- cv.glmnet(XXA,YYA,alpha=1,family="gaussian")
  etape2 <- glmnet(XXA,YYA,alpha=1,family="gausssian",lambda=lasso$lambda.min)
  PREV[blocs==ii,"lasmin"] <- predict(lasso,XXT,s="lambda.min")
  # PREV[blocs==ii,"las1se"] <- predict(lasso,XXT,s="lambda.1se")
  
  #####elas
  elas_net <- cv.glmnet(XXA,YYA,alpha=.5,family="gaussian")
  PREV[blocs==ii,"elamin"] <- predict(elas_net,XXT,s="lambda.min")
  #PREV[blocs==ii,"ela1se"] <- predict(elas,XXT,s="lambda.1se")
  
  #### arbre et forêt
  rpart_mdl <- rpart(Y~., data=donA)
  PREV[blocs==ii,"arbre"] <- predict(rpart_mdl,donT)[,2]
  rF_mdl <- randomForest(Y~., data=donA)
  PREV[blocs==ii,"random_forest"] <- predict(rF_mdl,donT)[,2]
  
  gbm_model <- gbm(Y~.,data=donA,distribution="gaussian",
                   cv.folds=10,n.trees = 100,shrinkage = 0.1,
                   interaction.depth = 1)
  PREV[blocs==ii,"gbm01"] <- predict(gbm_model,donT)
  
  ## Gradient boosting 
  xgb_train = xgb.DMatrix(data = XXA, label = YYA)
  xgb_test = xgb.DMatrix(data = XXT, label = YYT)
  
  cv <- xgb.cv(data=xgb_train,nrounds=300,max_depth=2,nfold=10,verbose=0,eta=0.1)
  iteropt <- which.min(cv$evaluation_log$test_rmse_mean)
  print(iteropt)
  model_xgboost = xgboost(data = xgb_train, max.depth = 2, 
                          nrounds = iteropt, eta=0.1,
                          verbose = 0)
  PREV[blocs==ii,"xgb11"] <- predict(model_xgboost,xgb_test)
  
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




