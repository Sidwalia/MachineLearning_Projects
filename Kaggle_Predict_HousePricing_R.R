library(MASS)
library(ISLR)
library(tidyverse)
library(readxl)
library(dplyr)
library(janitor)
library(mice)
library(glmnet)
library(ggplot2)

Train <- read_csv("C:\\Users\\Siddharth\\Desktop\\Queens_Coursework\\867\\Kaggle\\train.csv")
Test <- read_csv("C:\\Users\\Siddharth\\Desktop\\Queens_Coursework\\867\\Kaggle\\test.csv")

Y.training <- Train$SalePrice  # TAKING OUT SALEPRICE FOR Y VARIABLE
Y.training <- log(corrr$SalePrice)  # TAKING LOG, AS IT IS NOT NORMALLY DISTRIBUTED


Train <- subset(Train, select = -c(SalePrice))

#concatinating 2 dataset so that we can do data exploration on both of them simulaneously

full_data <- rbind(Train,Test)
str(full_data)

#find sum of NA in different variables
missing_values <- colSums(is.na(full_data))


miss <- full_data %>% summarise_each(funs(100*mean(is.na(.))))
 miss <- transpose(miss)

 #removing variables with high missing percentages
 full_data <- subset(full_data, select = -c(Alley,FireplaceQu,MiscFeature, PoolQC, Fence))


 summary(full_data)
 names(full_data) <- make.names(names(full_data))  #this is used to clean the colunn names as MICE was giving error for the column errors

 imputed_full <- mice(full_data, m=5, maxit=30, meth='pmm', seed=1) 

 complete_full <- complete(imputed_full,1)
 
 complete_full$YearBuilt <- 2021-complete_full$YearBuilt   # CHANGING ALL YEAR VARIABLES TO NO. OF YEARS FOR BETTER USAGE
 complete_full$YearRemodAdd <- 2021-complete_full$YearRemodAdd
 complete_full$GarageYrBlt <- 2021-complete_full$GarageYrBlt
 complete_full$YrSold <- 2021-complete_full$YrSold
 

 
 # ##################################################################################
 previous_na_action <- options('na.action')   #  THESE GLOBAL VARIABLES HAD TO BE CHANGED SO THE MODEL.MATRIX DOES NOT DROP ROWS WITH NA VALUES
 options(na.action='na.pass')
 

#CREATE DUMMY VARIABLES AND INTERACTION
 complete_full_dummy<-model.matrix(~ . + LotArea*LotShape + LotArea*LotShape*LotConfig + Heating*CentralAir + BsmtFinType1*BsmtFinType2
                                    + Heating*HeatingQC + GarageCars*GarageArea*GarageQual 
                                     + MSZoning*LotArea +
                                     BldgType*HouseStyle + OverallQual*OverallCond, complete_full)[,-1]
 
 
 
 # IMPUTING NAs
 names(complete_full_dummy) <- make.names(names(complete_full_dummy))
 imputed_full2 <- mice(complete_full_dummy, m=5, maxit=30, meth='cart', seed=1) 
 
 complete_full <- complete(imputed_full,1)
 
 
 # SPLITTING THE COMBINED DATASET BACK TO TEST AND TRAIN
 X.training <- complete_full[1:1460,]
 X.testing <- complete_full[1461:2919,]
 
 # CHANGING TO MATRIX, AS GLNMET ACCEPTS MATRICES ONLY
 X.training <- as.matrix(X.training)
 
 
 #LASSO (alpha=1)
 lasso.fit<-glmnet(x = X.training, y = Y.training, alpha = 1)
 plot(lasso.fit, xvar = "lambda")
 
 X.training <- as.matrix(X.training)
 X.testing <- as.matrix(X.testing)
 
 
 #selecting the best penalty lambda
 crossval <-  cv.glmnet(x = X.training, y = Y.training, alpha = 1) #create cross-validation data. By default, the function performs ten-fold cross-validation, though this can be changed using the argument nfolds. 
 plot(crossval)
 penalty.lasso <- crossval$lambda.min 
 log(penalty.lasso) 
 plot(crossval,xlim=c(-6,-4),ylim=c(0.01,0.04)) 
 lasso.opt.fit <-glmnet(x = X.training, y = Y.training, alpha = 1, lambda = penalty.lasso) 
 coef(lasso.opt.fit) 
 
 # predicting the performance on the testing set
 lasso.testing <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =X.testing))
 
 lasso.testing <- as.data.frame(lasso.testing)
 
 # CREATING FINAL DATASET FOR SUBMISSION
 Test_combined <- cbind(Test,lasso.testing)
 final <- subset(Test_combined, select = c(Id, s1))
 names(final)[names(final) == "s1"] <- "SalePrice"
 
 write.csv(final, file = "C:\\Users\\Siddharth\\Desktop\\Queens_Coursework\\867\\Kaggle\\final.csv", row.names = TRUE)
 
 
 #ridge (alpha=0)
 ridge.fit<-glmnet(x = X.training, y = Y.training, alpha = 0)
 plot(ridge.fit, xvar = "lambda")
 
 #selecting the best penalty lambda
 crossval.ridge <-  cv.glmnet(x = X.training, y = Y.training, alpha = 0)
 plot(crossval.ridge)
 penalty.ridge <- crossval.ridge$lambda.min 
 log(penalty.ridge) 
 ridge.opt.fit <-glmnet(x = X.training, y = Y.training, alpha = 0, lambda = penalty.ridge) 
 coef(ridge.opt.fit)
 
 ridge.testing <- exp(predict(ridge.opt.fit, s = penalty.ridge, newx =X.testing))
 
 ridge.testing <- as.data.frame(ridge.testing)
 
 #CREATING FINAL DATASET FOR SUBMISSION
 
 Test_combined_ridge <- cbind(Test,ridge.testing)
 final_ridge <- subset(Test_combined_ridge, select = c(Id, s1))
 names(final_ridge)[names(final_ridge) == "s1"] <- "SalePrice"
 
 write.csv(final_ridge, file = "C:\\Users\\Siddharth\\Desktop\\Queens_Coursework\\867\\Kaggle\\final_ridge.csv", row.names = TRUE)
 

 
 ##########################################################################################################
 # IMPROVEMENTS TO LASSO MODEL
 
 # 
 previous_na_action <- options('na.action')
 options(na.action='na.pass')


 
 complete_full_dummy_1<-model.matrix(~ . + log(LotArea)*LotShape  + (Heating*CentralAir)^2
                                     + (Heating*HeatingQC)^2  + log(GarageCars*GarageArea) + log(BsmtFinType1*BsmtFinType2)
                                     (MSZoning*LotArea)^2 + sqrt(MasVnrArea)*MasVnrType + (OverallQual*OverallCond)^2
                                     +log(Fireplaces) + log(KitchenAbvGr)*KitchenQual +
                                      (Neighborhood*Condition1) + log(OverallQual*GrLivArea) + (SaleCondition*Condition1)^2 +
                                             log(TotRmsAbvGrd*FullBath), complete_full)[,-1]
 
 options(na.action=previous_na_action$na.action)
 
 # REPLACING  NAN AND INFINITE VALUES WITH NA
 complete_full_dummy_1[mapply(is.infinite, complete_full_dummy_1)] <- NA
 complete_full_dummy_1[mapply(is.nan, complete_full_dummy_1)] <- NA
 
 # CHANGING NAs TO MEAN OF THE COLUMN
 for(i in 1:ncol(complete_full_dummy_1)){
         complete_full_dummy_1[is.na(complete_full_dummy_1[,i]), i] <- mean(complete_full_dummy_1[,i], na.rm = TRUE)
 }
 
 X.training_1 <- complete_full_dummy_1[1:1460,]
 X.testing_1 <- complete_full_dummy_1[1461:2919,]
 
 
 #LASSO (alpha=1)
 lasso.fit_1<-glmnet(x = X.training_1, y = Y.training, alpha = 1)
 plot(lasso.fit_1, xvar = "lambda")
 
 
 #selecting the best penalty lambda
 crossval_1 <-  cv.glmnet(x = X.training_1, y = Y.training, alpha = 1)  
 plot(crossval_1)
 penalty.lasso_1 <- crossval_1$lambda.min 
 log(penalty.lasso_1) 
 plot(crossval_1,xlim=c(-7,-4),ylim=c(0,10))
 lasso.opt.fit_1 <-glmnet(x = X.training_1, y = Y.training, alpha = 1, lambda = penalty.lasso_1) 
 coef(lasso.opt.fit_1) 
 
 # predicting the performance on the testing set
 lasso.testing_1 <- exp(predict(lasso.opt.fit_1, s = penalty.lasso_1, newx =X.testing_1))
 
 lasso.testing_1 <- as.data.frame(lasso.testing_1)
 
 # CREATING FINAL DATASET FOR SUBMISSION
 Test_combined_1 <- cbind(Test,lasso.testing_1)
 final_1 <- subset(Test_combined_1, select = c(Id, s1))
 names(final_1)[names(final_1) == "s1"] <- "SalePrice"
 
 write.csv(final_1, file = "C:\\Users\\Siddharth\\Desktop\\Queens_Coursework\\867\\Kaggle\\final_improved.csv", row.names = TRUE)
 
 
