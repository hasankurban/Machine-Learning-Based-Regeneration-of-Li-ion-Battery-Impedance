finalData[,3] <- as.numeric(as.character(finalData[,3]))
Idc_predictions <- as.data.frame(predict(ensemble1.RMSE, newdata = finalData[,1:3]))
names(Idc_predictions)[1] <- "Idc"
write.table(Idc_predictions, file = "idc_predictions.txt", sep = "\t",
            row.names = FALSE, col.names = TRUE)
