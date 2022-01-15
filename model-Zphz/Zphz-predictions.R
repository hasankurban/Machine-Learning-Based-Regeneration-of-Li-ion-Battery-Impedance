finalData[,3] <- as.numeric(as.character(finalData[,3]))
Zphz_predictions <- as.data.frame(predict(ensemble1.RMSE, newdata = finalData[,1:3]))
names(Zphz_predictions)[1] <- "Zphz"
write.table(Zphz_predictions, file = "Zphz_predictions.txt", sep = "\t",
            row.names = FALSE, col.names = TRUE)
