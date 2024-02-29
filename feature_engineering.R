path_pj <- paste0(path,"/_projet1" )

setwd(path_pj)

don_0 <- don_ori %>% rename(Y=V15)

# Y renommé + X simple
saveRDS(don_0, file = "don.rds")



## Creaation de polynome de degré 2



XX <- model.matrix(Y~.,data=don_0,remove_1st_dummy =T,sparse = F)[,-1]

XXcar <- XX^2
XXcub <- XX^3
colnames(XXcar) <- paste(colnames(XX),"car",sep="")
colnames(XXcub) <- paste(colnames(XX),"cub",sep="")


# Interaction 
XI <- model.matrix(Y~.^2,data=don)[,-1]


saveRDS(don_power2, file = "don_power2.rds")
