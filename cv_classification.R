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
  logit <- glm(Y~.,data=donA,family="binomial")
  PREV[blocs==ii,"logit"] <- predict(logit,donT,type="response")
  
  ### algo backward AIC
    # algo2 <- step(logit,trace=0)
    # PREV[blocs==ii,"aic"] <- predict(algo2,donT,type="response")
  ##### algo backward bis (BIC)
    # algo3 <- step(logit,k=log(nrow(donA)),trace=0)
    # PREV[blocs==ii,"bic"] <- predict(algo3,donT,type="response")
  
  #####methode3
  arbre_mdl <- rpart(as.factor(Y)~.,data=donA)
  PREV[blocs==ii,"arbre"] <- predict(arbre_mdl,donT,type="prob")[,2]
  rf_mdl <- randomForest(as.factor(Y)~.,data=donA)
  PREV[blocs==ii,"foret"] <- predict(rf_mdl,donT,type="prob")[,2]
  ranger_mdl <- ranger(as.factor(Y)~.,data=donA, probability = TRUE)
  PREV[blocs==ii,"foretRanger"] <- predict(ranger_mdl,donT)$prediction[,2]
  
  #####ridge
  ridge <- cv.glmnet(XXA,YYA,alpha=0,family="binomial")
  PREV[blocs==ii,"ridmin"] <- predict(ridge,XXT,s="lambda.min",type="response")
  # PREV[blocs==ii,"rid1se"] <- predict(ridge,XXT,s="lambda.1se",type="response")
  
  #####lasso
  lasso <- cv.glmnet(XXA,YYA,alpha=1,family="binomial")
  etape2 <- glmnet(XXA,YYA,alpha=1,family="binomial",lambda=lasso$lambda.min)
  PREV[blocs==ii,"lasmin"] <- predict(lasso,XXT,s="lambda.min",type="response")
  # PREV[blocs==ii,"las1se"] <- predict(lasso,XXT,s="lambda.1se",type="response")
  
  #####elas
  elas_net <- cv.glmnet(XXA,YYA,alpha=.5,family="binomial")
  PREV[blocs==ii,"elamin"] <- predict(elas_net,XXT,s="lambda.min",type="response")
  #PREV[blocs==ii,"ela1se"] <- predict(elas,XXT,s="lambda.1se",type="response")
  
  #### arbre et forêt
  rpart_mdl <- rpart(as.factor(Y)~., data=donA)
  PREV[blocs==ii,"arbre"] <- predict(rpart_mdl,donT,type="prob")[,2]
  rF_mdl <- randomForest(as.factor(Y)~., data=donA)
  PREV[blocs==ii,"random_forest"] <- predict(rF_mdl,donT,type="prob")[,2]
  
  gbm_model <- gbm(Y~.,data=donA,distribution="bernouilli",
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



apply(EST,2,erreur,Y=PREV$Y)


accuracy <- function(X,Y,seuil=0.5){
  Xc=X*0
  Xc[X>seuil] <- 1
  sum(Xc==Y)/length(Y)*100
}
apply(PREV,2,accuracy,Y=PREV$Y)
apply(EST,2,accuracy,Y=EST$Y)


auctout <- roc(Y~.,data=PREV)
plot(auctout$log)
for(ii in 2:length(auctout)){
  plot(auctout[[ii]],add=T,col=ii)
}
sapply(auctout,coords,x=0.5,ret=c("threshold","accuracy",
                                  "specificity","sensitivity"
))




