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

#postResample(pred = pred_testing_LM, obs = testing[,4])

# RMSE
pred_testing_RMSE <- data.frame(ensemble_1 = RMSE(predict_testing_ens1,  testing[,4]),
                                #ensemble_2 = RMSE(predict_ens2,  testing[,4]),
                                LM = RMSE(pred_testing_LM,  testing[,4]),
                                RF = RMSE(pred_testing_RF,  testing[,4]),
                                XGBDART = RMSE(pred_testing_XGBDART,  testing[,4]),
                                GLM = RMSE(pred_testing_GLM ,  testing[,4]),
                                LMSTEPAIC = RMSE(pred_testing_LMSTEPAIC ,  testing[,4]),
                                MONMLP = RMSE(pred_testing_MONMLP ,  testing[,4]),
                                GVCEARTH = RMSE(pred_testing_GVCEARTH ,  testing[,4]),
                                PPR= RMSE(pred_testing_PPR,  testing[,4]),
                                SVMLINEAR= RMSE(pred_testing_SVMLINEAR,  testing[,4]),
                                SVMRADIAL= RMSE(pred_testing_SVMRADIAL,  testing[,4]),
                                KNN = RMSE(pred_testing_KNN ,  testing[,4]),
                                XGBTREE= RMSE(pred_testing_XGBTREE,  testing[,4]),
                                XGBLINEAR= RMSE(pred_testing_XGBLINEAR,  testing[,4]),
                                ICR = RMSE(pred_testing_ICR,  testing[,4]),
                                GAUSSLINEAR = RMSE(pred_testing_GAUSSLINEAR,  testing[,4]),
                                RPART = RMSE(pred_testing_RPART,  testing[,4])
)

#postResample(pred_testing_RMSE, obs = testing[,4])

print(pred_testing_RMSE)





# MAE
pred_testing_MAE <- data.frame(ensemble_1 =MAE(predict_testing_ens1,  testing[,4]),
                               #ensemble_2 = MAE(predict_ens2,  testing[,4]),
                               LM = MAE(pred_testing_LM,  testing[,4]),
                               RF = MAE(pred_testing_RF,  testing[,4]),
                               XGBDART = MAE(pred_testing_XGBDART,  testing[,4]),
                               GLM = MAE(pred_testing_GLM ,  testing[,4]),
                               LMSTEPAIC = MAE(pred_testing_LMSTEPAIC ,  testing[,4]),
                               MONMLP = MAE(pred_testing_MONMLP ,  testing[,4]),
                               GVCEARTH = MAE(pred_testing_GVCEARTH ,  testing[,4]),
                               PPR= MAE(pred_testing_PPR,  testing[,4]),
                               SVMLINEAR= MAE(pred_testing_SVMLINEAR,  testing[,4]),
                               SVMRADIAL= MAE(pred_testing_SVMRADIAL,  testing[,4]),
                               KNN = MAE(pred_testing_KNN ,  testing[,4]),
                               XGBTREE=MAE(pred_testing_XGBTREE,  testing[,4]),
                               XGBLINEAR= MAE(pred_testing_XGBLINEAR,  testing[,4]),
                               ICR = MAE(pred_testing_ICR,  testing[,4]),
                               GAUSSLINEAR = MAE(pred_testing_GAUSSLINEAR,  testing[,4]),
                               RPART =MAE(pred_testing_RPART,  testing[,4])
)
print(pred_testing_MAE)


#Take R^2 from here
pred_testing_postResample <- data.frame(ensemble_1 = postResample(predict_testing_ens1,  testing[,4]),
                                        #ensemble_2 = postResample(predict_ens2,  testing[,4]),       
                                        LM =  postResample(pred_testing_LM,  testing[,4]),
                                        RF =  postResample(pred_testing_RF,  testing[,4]),
                                        XGBDART =  postResample(pred_testing_XGBDART,  testing[,4]),
                                        GLM =  postResample(pred_testing_GLM ,  testing[,4]),
                                        LMSTEPAIC =  postResample(pred_testing_LMSTEPAIC ,  testing[,4]),
                                        MONMLP =  postResample(pred_testing_MONMLP ,  testing[,4]),
                                        GVCEARTH =  postResample(pred_testing_GVCEARTH ,  testing[,4]),
                                        PPR=  postResample(pred_testing_PPR,  testing[,4]),
                                        SVMLINEAR=  postResample(pred_testing_SVMLINEAR,  testing[,4]),
                                        SVMRADIAL=  postResample(pred_testing_SVMRADIAL,  testing[,4]),
                                        KNN =  postResample(pred_testing_KNN ,  testing[,4]),
                                        XGBTREE=  postResample(pred_testing_XGBTREE,  testing[,4]),
                                        XGBLINEAR=  postResample(pred_testing_XGBLINEAR,  testing[,4]),
                                        ICR = postResample(pred_testing_ICR,  testing[,4]),
                                        GAUSSLINEAR = postResample(pred_testing_GAUSSLINEAR,  testing[,4]),
                                        RPART = postResample(pred_testing_RPART,  testing[,4]))
print(pred_testing_postResample)
View(pred_testing_postResample)
print(pred_testing_postResample)[2,]

#Understanding the best models and hybrid models
models$lm$finalModel
models$rf$finalModel
models$xgbLinear$finalModel
