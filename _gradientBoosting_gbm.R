
tmp <- gbm(Y~., data=don, distribution="gaussian",n.trees=100, cv.folds=5, shrinkage=0.1) # on voit qu'avec 100 itérations on est pas en bout de grille, donc ça va  
gbm.perf(tmp) # 16
# le facteur de frein est lié au nombre d'itérations donc faire 2 ou 3 facteurs de frein dans gbm (est-ce que là les résultats sont franchement différents ?)
tmp <- gbm(Y~., data=don, distribution="gaussian",n.trees=100, cv.folds=5, shrinkage=0.5)
gbm.perf(tmp) # 5
names(tmp)
plot(tmp$train.error)
plot(tmp$valid.error)
plot(tmp$cv.error)
gbm.perf(tmp)
points(1:100,tmp$train.error,col=2)
points(1:100,tmp$test.error,col=2)
points(1:100,tmp$cv.error,col=3)
which.min(tmp$cv.error) # ça c'est quoi ?