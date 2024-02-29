



don <- readRDS(file = "don.rds")

dim(don)


don <- don %>% na.omit()



dim(don)


str(don)
which.max(table(don$V14))
don2 <- don %>% filter(V14 %in% (which.max(table(don$V14))[[2]]))
dim(don2)
table(don$V14)