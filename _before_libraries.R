para_installpackages <- FALSE 


if (para_installpackages == TRUE) {
  
  install.packages("corrplot", type = "binary")
  install.packages("DataExplorer", type = "binary")
  install.packages("dygraphs", type = "binary")
  install.packages("GGally", type = "binary")
  install.packages("naniar", type = "binary")
  install.packages("glmnet", type = "binary")
  
}


# Packages communs
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(data.table)
library(readr)
library(haven)
library(readxl)
library(tidyselect)

# Packages pour statistiques 
library(corrplot)
library(DataExplorer)
library(dygraphs)
library(GGally)
library(lubridate)
library(ggplot2)
library(naniar)

# Packages pour modélisation 
library(tidymodels)
library(tidyverse)
library(ranger)
library(xgboost)
library(glmnet)

library(pROC)
library(dplyr)
library(glmnet)

library(rpart.plot)

library(rpart)
library(randomForest)
library(ranger)
