my_ensemble <- readRDS("ensemble1.rds") #best model
my_lm<- readRDS("lm.rds")
my_glm<-readRDS( "glm.rds")
my_rf<-readRDS("rf.rds")
my_xgbDART<-readRDS("xgbDART.rds")
my_xgbLinear<-readRDS("c.rds")
my_xgbTree<-readRDS("xgbTree.rds")
my_lmStepAIC<-readRDS("lmStepAIC.rds")
my_monmlp<-readRDS("monmlp.rds")
my_gcvEarth<-readRDS("gcvEarth.rds")
my_ppr<-readRDS("ppr.rds")
my_svmLinear<-readRDS( "svmLinear.rds")
my_svmRadial<-readRDS("svmRadial.rds")
my_knn<-readRDS("knn.rds")
my_icr<-readRDS("icr.rds")
my_gaussprLinear<-readRDS("gaussprLinear.rds")
my_rpart<-readRDS("rpart.rds")
###################################################
library(rpart.plot)
summary(my_rpart)
my_rpart$finalModel
my_rpart$resample
rpart.plot(my_rpart$finalModel)
###################################################
summary(my_gaussprLinear)
my_gaussprLinear$finalModel
my_gaussprLinear$resample
###################################################
summary(my_ensemble)
my_ensemble
