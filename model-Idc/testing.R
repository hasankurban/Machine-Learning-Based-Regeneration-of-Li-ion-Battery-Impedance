#@ Dr. Kurban, Oct 2021, conctact hakurban@gmail.com for any questions.
#@IUB, Computer Science Department
# Testing for Idc
# PREDICTIONS

pred_testing_LM <- predict.train(models$lm, newdata = testing[,1:3])
pred_testing_RF <- predict.train(models$rf, newdata = testing[,1:3])
pred_testing_XGBDART <- predict.train(models$xgbDART, newdata = testing[,1:3])
pred_testing_GLM <- predict.train(models$glm, newdata = testing[,1:3])
pred_testing_LMSTEPAIC <- predict.train(models$lmStepAIC, newdata = testing[,1:3])
pred_testing_MONMLP <- predict.train(models$monmlp, newdata = testing[,1:3])
pred_testing_GVCEARTH <- predict.train(models$gcvEarth, newdata = testing[,1:3])
pred_testing_PPR <- predict.train(models$ppr, newdata = testing[,1:3])
pred_testing_SVMLINEAR <- predict.train(models$svmLinear, newdata = testing[,1:3])
pred_testing_SVMRADIAL <- predict.train(models$svmRadial, newdata = testing[,1:3])
pred_testing_KNN <- predict.train(models$knn, newdata = testing[,1:3])
pred_testing_XGBTREE <- predict.train(models$xgbTree, newdata = testing[,1:3])
pred_testing_XGBLINEAR <- predict.train(models$xgbLinear, newdata = testing[,1:3])
pred_testing_ICR <- predict.train(models$icr, newdata = testing[,1:3])
pred_testing_GAUSSLINEAR <- predict.train(models$gaussprLinear, newdata = testing[,1:3])
pred_testing_RPART <- predict.train(models$rpart, newdata = testing[,1:3])
predict_testing_ens1 <- predict(ensemble1.RMSE, newdata = testing[,1:3])
#predict_ens2 <- predict(ensemble2, newdata = testing[,1:3])

#postResample(pred = pred_testing_LM, obs = testing[,5])

# RMSE
pred_testing_RMSE <- data.frame(ensemble_1 = RMSE(predict_testing_ens1,  testing[,5]),
                        #ensemble_2 = RMSE(predict_ens2,  testing[,5]),
                        LM = RMSE(pred_testing_LM,  testing[,5]),
                        RF = RMSE(pred_testing_RF,  testing[,5]),
                        XGBDART = RMSE(pred_testing_XGBDART,  testing[,5]),
                        GLM = RMSE(pred_testing_GLM ,  testing[,5]),
                        LMSTEPAIC = RMSE(pred_testing_LMSTEPAIC ,  testing[,5]),
                        MONMLP = RMSE(pred_testing_MONMLP ,  testing[,5]),
                        GVCEARTH = RMSE(pred_testing_GVCEARTH ,  testing[,5]),
                        PPR= RMSE(pred_testing_PPR,  testing[,5]),
                        SVMLINEAR= RMSE(pred_testing_SVMLINEAR,  testing[,5]),
                        SVMRADIAL= RMSE(pred_testing_SVMRADIAL,  testing[,5]),
                        KNN = RMSE(pred_testing_KNN ,  testing[,5]),
                        XGBTREE= RMSE(pred_testing_XGBTREE,  testing[,5]),
                        XGBLINEAR= RMSE(pred_testing_XGBLINEAR,  testing[,5]),
                        ICR = RMSE(pred_testing_ICR,  testing[,5]),
                        GAUSSLINEAR = RMSE(pred_testing_GAUSSLINEAR,  testing[,5]),
                        RPART = RMSE(pred_testing_RPART,  testing[,5])
                        )

#postResample(pred_testing_RMSE, obs = testing[,5])

print(pred_testing_RMSE)





# MAE
pred_testing_MAE <- data.frame(ensemble_1 =MAE(predict_testing_ens1,  testing[,5]),
                               #ensemble_2 = MAE(predict_ens2,  testing[,5]),
                                LM = MAE(pred_testing_LM,  testing[,5]),
                                RF = MAE(pred_testing_RF,  testing[,5]),
                                XGBDART = MAE(pred_testing_XGBDART,  testing[,5]),
                                GLM = MAE(pred_testing_GLM ,  testing[,5]),
                                LMSTEPAIC = MAE(pred_testing_LMSTEPAIC ,  testing[,5]),
                                MONMLP = MAE(pred_testing_MONMLP ,  testing[,5]),
                                GVCEARTH = MAE(pred_testing_GVCEARTH ,  testing[,5]),
                                PPR= MAE(pred_testing_PPR,  testing[,5]),
                                SVMLINEAR= MAE(pred_testing_SVMLINEAR,  testing[,5]),
                                SVMRADIAL= MAE(pred_testing_SVMRADIAL,  testing[,5]),
                                KNN = MAE(pred_testing_KNN ,  testing[,5]),
                                XGBTREE=MAE(pred_testing_XGBTREE,  testing[,5]),
                                XGBLINEAR= MAE(pred_testing_XGBLINEAR,  testing[,5]),
                                ICR = MAE(pred_testing_ICR,  testing[,5]),
                                GAUSSLINEAR = MAE(pred_testing_GAUSSLINEAR,  testing[,5]),
                                RPART =MAE(pred_testing_RPART,  testing[,5])
)
print(pred_testing_MAE)


#Take R^2 from here
pred_testing_postResample <- data.frame(ensemble_1 = postResample(predict_testing_ens1,  testing[,5]),
                                #ensemble_2 = postResample(predict_ens2,  testing[,5]),       
                                LM =  postResample(pred_testing_LM,  testing[,5]),
                                RF =  postResample(pred_testing_RF,  testing[,5]),
                                XGBDART =  postResample(pred_testing_XGBDART,  testing[,5]),
                                GLM =  postResample(pred_testing_GLM ,  testing[,5]),
                                LMSTEPAIC =  postResample(pred_testing_LMSTEPAIC ,  testing[,5]),
                                MONMLP =  postResample(pred_testing_MONMLP ,  testing[,5]),
                                GVCEARTH =  postResample(pred_testing_GVCEARTH ,  testing[,5]),
                                PPR=  postResample(pred_testing_PPR,  testing[,5]),
                                SVMLINEAR=  postResample(pred_testing_SVMLINEAR,  testing[,5]),
                                SVMRADIAL=  postResample(pred_testing_SVMRADIAL,  testing[,5]),
                                KNN =  postResample(pred_testing_KNN ,  testing[,5]),
                                XGBTREE=  postResample(pred_testing_XGBTREE,  testing[,5]),
                                XGBLINEAR=  postResample(pred_testing_XGBLINEAR,  testing[,5]),
                                ICR = postResample(pred_testing_ICR,  testing[,5]),
                                GAUSSLINEAR = postResample(pred_testing_GAUSSLINEAR,  testing[,5]),
                                RPART = postResample(pred_testing_RPART,  testing[,5]))
print(pred_testing_postResample)
View(pred_testing_postResample)
print(pred_testing_postResample)[2,]

#Understanding the best models and hybrid models
models$lm$finalModel
models$rf$finalModel
models$xgbLinear$finalModel
