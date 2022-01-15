#@ Dr. Kurban, Jan 2022, conctact hakurban@gmail.com for any questions.
#@IUB, Computer Science Department
#data visualization
library(ggplot2)
library(GGally)
require(reshape2)
# Visualizaiton of the data used in the experiments
temp.finalData <- finalData
#names(temp.finalData)[1] <- "log(Freq)"
temp.finalData[,3] <- as.factor(temp.finalData[,3])
#temp.finalData[,1] <- log(temp.finalData[,1])
#temp.finalData <- log(temp.finalData)
TO.fig.small <- ggpairs(temp.finalData, aes(color = temp.finalData[,3]))+ theme_bw()
for(i in 1:TO.fig.small$nrow) {
  for(j in 1:TO.fig.small$ncol){
    TO.fig.small[i,j] <- TO.fig.small[i,j] 
  }
}
TO.fig.small+  theme_bw() + theme_grey(base_size = 22) +
  theme(legend.title=element_blank(), panel.grid.major = element_blank(),
        axis.text=element_text(size=11),
        panel.grid.minor = element_blank()
  ) 


