#@ Dr. Kurban, Jan 2022, conctact hakurban@gmail.com for any questions.
#@IUB, Computer Science Department
############################## DATA PREPROCESSING #############################################
filelist = list.files(pattern = ".txt")
print(filelist)
datalist = lapply(filelist, function(x)read.table(x, header=TRUE, dec = ","))
finalData =  data.frame(matrix(vector(), 0, 8))
mV= c(3.2,3.4,3.6,3.8,4.0,4.2)
for (i in 1:length(datalist)){
  data <- as.data.frame(datalist[i])  
  print(dim(data))
  temp.data1 = rep(mV[i],dim(as.data.frame(datalist[1]))[1])
  temp.data2 = cbind(data,temp.data1)
  finalData <- rbind(finalData,temp.data2)
}
colnames(finalData)[8]  <- "mV"
finalData[,8] <- as.factor(finalData[,8])
s#ummary(finalData)
#dim(finalData)
#originalData is used for visualization purposes
originalData <-finalData
#picking the variables that will be used in training.
#finalData <- finalData[,c(1,7,8,5,6)]


idc_prediction <- read.table("../model-Idc/idc_predictions.txt", header = TRUE, sep= " ")
names(idc_prediction) [1] <- "predicted_Idc"
Zphz_prediction <- read.table("../model-Zphz/Zphz_predictions.txt", header = TRUE, sep= " ")
names(Zphz_prediction) [1] <- "predicted_Zphz"


newData <- cbind(finalData,idc_prediction,Zphz_prediction)
