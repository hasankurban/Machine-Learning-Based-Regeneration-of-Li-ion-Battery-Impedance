#@ Dr. Kurban, Jan 2022, conctact hakurban@gmail.com for any questions.
#@IUB, Computer Science Department
#Training for Zphz
################################# PARTION DATA: TRAINING AND TESTING #########################################################
library(caret)
library(ggpubr)
set.seed(104)
#Spliting data as training and test set. 
indxTrain <- createDataPartition(y = finalData[,3],p = 0.75,list = FALSE)
training <- finalData[indxTrain,]
testing <-finalData[-indxTrain,]

#Checking distibution in original data and partitioned data
prop.table(table(training[,3])) * 100
prop.table(table(testing[,3])) * 100
prop.table(table(finalData[,3])) * 100
testing[,3] <- as.numeric(as.character(testing[,3]))
training[,3] <- as.numeric(as.character(training[,3]))
#####################################################################################
#Models:
#lm:  linear  regression,
#glm:  Generalized Linear Model, 
#lmStepAIC: Generalized Linear Model with Stepwise Feature Selection
#gcvEarth: Multivariate Adaptive Regression Splines
#ppr: Projection Pursuit Regression
#rf: Random Forest
#xgbDart,XgbTree, xgbLinear: Extreme Gradient  Boosting
#monmlp:Monotone Multi-Layer Perceptron Neural Network
#svmLinear, svmRadial: SVM
#knn: k-nearest neighbor
#rpart: CART
#gaussprLinear: Gaussian Process
#icr: Independent Component Regression
#############################################################
library(caretEnsemble)
library(caret)
set.seed(105)
my_control <- trainControl(method = "cv",
                           number =10,
                           savePredictions = "final",
                           allowParallel = TRUE)

models <- caretList(training[,1:3],
                        training[,4],
                        trControl = my_control,
                        methodList = c("lm","glm","rf","xgbDART","xgbLinear","xgbTree",
                                       "lmStepAIC","monmlp","gcvEarth","ppr",
                                       "svmLinear","svmRadial","knn","icr","gaussprLinear",
                                       "rpart"),
                        tuneList = NULL,
                        continue_on_fail = FALSE, 
                        preProcess = c("center","scale"))
######### Training Error: RMSE   ###########################################
training_results_RMSE <- data.frame(
  LM = min(models$lm$results$RMSE),
  RF = min(models$rf$results$RMSE),
  XGBDART= min(models$xgbDART$results$RMSE),
  GLM = min(models$glm$results$RMSE),
  LMSTEPAIC= min(models$lmStepAIC$results$RMSE),
  MONMLP= min(models$monmlp$results$RMSE),
  GVCEARTH = min(models$gcvEarth$results$RMSE),
  PPR= min(models$ppr$results$RMSE),
  SVMLINEAR= min(models$svmLinear$results$RMSE),
  SVMRADIAL= min(models$svmRadial$results$RMSE),
  KNN= min(models$knn$results$RMSE),
  XGBTREE= min(models$xgbTree$results$RMSE),
  XGBLINEAR = min(models$xgbLinear$results$RMSE),
  ICR = min(models$icr$results$RMSE),
  GAUSPRLINEAR= min(models$gaussprLinear$results$RMSE),
  RPART =min(models$rpart$results$RMSE)
)
print(training_results_RMSE)
####################################################################
resamples <- resamples(models)
#dotplot(resamples, metric = "RMSE")
#dotplot(resamples, metric = "MAE")
#dotplot(resamples, metric = "Rsquared")
#densityplot(resamples, metric = "RMSE",auto.key = TRUE, pch = "|")
#bwplot(difs,metric = "RMSE")
#bwplot(difs,metric = "MAE")
#bwplot(difs,metric = "Rsquared")



bw_RMSE <- bwplot(resamples,metric = "RMSE", col="black",par.settings=list(box.rectangle=list(col="black", fill="black",alpha=0.4),box.umbrella=list(col="black",alpha=0.4),plot.symbol=list(col="black",alpha=0.4)))
print(update(bw_RMSE),position=c(0,0,0.33,1))
bw_MAE <-  bwplot(resamples,metric = "MAE", col="black",par.settings=list(box.rectangle=list(col="black", fill="black",alpha=0.4),box.umbrella=list(col="black",alpha=0.4),plot.symbol=list(col="black",alpha=0.4)))
print(update(bw_MAE), newpage=FALSE, position=c(0.33,0,0.66,1))
bw_R2 <-  bwplot(resamples,metric = "Rsquared", col="black",par.settings=list(box.rectangle=list(col="black", fill="black",alpha=0.4),box.umbrella=list(col="black",alpha=0.4),plot.symbol=list(col="black",alpha=0.4)))
print(update(bw_R2),newpage=FALSE, position=c(0.66,0,0.99,1))


parallel_RMSE <- parallelplot(resamples , metric = "RMSE")
print(update(parallel_RMSE),position=c(0,0,0.33,1))
parallel_MAE <- parallelplot(resamples , metric = "MAE")
print(update(parallel_MAE), newpage=FALSE, position=c(0.33,0,0.66,1))
parallel_R2 <-parallelplot(resamples, metric = "Rsquared")
print(update(parallel_R2),newpage=FALSE, position=c(0.66,0,0.99,1))


ggarrange(bw_RMSE,parallel_RMSE, bw_MAE, parallel_MAE, bw_R2, parallel_R2, ncol = 2, nrow = 3)



difs <- diff(resamples)
difs_RMSE <- levelplot(difs, what = "differences", metric = "RMSE",col.regions=heat.colors(100))
print(update(difs_RMSE),position=c(0,0,1,0.36))
difs_MAE <- levelplot(difs, what = "differences", metric = "MAE",col.regions=heat.colors(100))
print(update(difs_MAE), newpage=FALSE, position=c(0,0.32,1,0.68))
difs_R2 <- levelplot(difs, what = "differences", metric = "Rsquared",col.regions=heat.colors(100))
print(update(difs_R2),newpage=FALSE, position=c(0,0.63,1,1))


library(corrplot)
library(xtable)
corTable <- modelCor(resamples)
print(xtable(corTable, type = "latex"), file = "corTable.tex")
corrplot(corTable, order="hclust", method = "color",tl.col = "black")
View(modelCor(resamples))


set.seed(222)

# Built the best ML model
ensemble1 <- caretEnsemble(models[c(5,3,4,6,13 )], 
                           metric = "RMSE", 
                           trControl = my_control)
summary(ensemble1)
plot(ensemble1)
varImp(ensemble1)




ensemble1.RMSE <- caretEnsemble(models[c(5,4,6 )], 
                                metric = "RMSE", 
                                trControl = my_control)

ensemble1.MAE <- caretEnsemble(models[c(5,4,6 )], 
                               metric = "MAE", 
                               trControl = my_control)


ensemble1.Rsquared <- caretEnsemble(models[c(5,4,6 )], 
                                    metric = "Rsquared", 
                                    trControl = my_control)


summary(ensemble1.RMSE)
plot1 <- plot(ensemble1.RMSE) + coord_flip()
varImp(ensemble1.RMSE)
ensemble1.RMSE


summary(ensemble1.MAE)
plot2 <- plot(ensemble1.MAE) + coord_flip()
varImp(ensemble1.MAE)
ensemble1.MAE

summary(ensemble1.Rsquared)
plot3 <- plot(ensemble1.Rsquared)+  coord_flip()
varImp(ensemble1.Rsquared)
ensemble1.Rsquared

ggarrange(plot1, plot2, plot3,ncol = 3, nrow = 1)



#set.seed(123)
#xgbLinear_model <- train( training[,1:3],
#                        training[,4],
#                        trControl = my_control,
#                        method = "xgbLinear",
#                        metric = "RMSE",
#                        preProcess = c("center","scale"),
#                        importance = TRUE)
#plot(varImp(xgbLinear_model))
#varImp(xgbLinear_model)
#xgbLinear_model$finalModel



#lm_model <- train( training[,1:3],
#                          training[,4],
#                          trControl = my_control,
#                          method = "lm",
#                          metric = "RMSE",
#                          preProcess = c("center","scale"),
#                          importance = TRUE)
#plot(varImp(lm_model))
#varImp(lm_model)
#lm_model$finalModel
