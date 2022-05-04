#### ***********Comparative between two ROC AUC curves**********

setwd("/media/csb/NewVolume/susmita/Ujjaini_Animesh/ROC plots")	# set directory

## load packages
library("pROC")
library("ROCR")

## load the Data file
data1 = read.csv("Luminal_Grade2VsGrade3/Ceramide_C16.tsv", header = T, sep="\t")
data2 = read.csv("TNBC_Grade2VsGrade3/Ceramide_C16.tsv", header = T, sep="\t")

## fit a logistic regression to the data1	
glm.fit1=glm(data1$Type ~ data1$Ceramide_C16, family=binomial)
plot(data1$Type ~ data1$Ceramide_C16, data=data1)
lines(data1$Ceramide_C16, glm.fit1$fitted.values)

## fit a logistic regression to the data2	
glm.fit2=glm(data2$Type ~ data2$Ceramide_C16, family=binomial)
plot(data2$Type ~ data2$Ceramide_C16, data=data2)
lines(data2$Ceramide_C16, glm.fit2$fitted.values)

par(pty = "s") #par(pty = "s") ## pty sets the aspect ratio of the plot region. "s" - creates a square plotting region

## Saving ROC plot in PNG format
png(filename = "Comparative/png/Ceramide_C16.png", units="in", width=5, height=5, res=300)
ROC1 = roc(data1$Type, glm.fit1$fitted.values, plot=TRUE, legacy.axes=TRUE, col="#377eb8", lwd=1, main = "Ceramide_C16", type="l")
ROC2 = plot.roc(data2$Type, glm.fit2$fitted.values, col="#008600", lwd=1, type="l", add=TRUE)

testobj <- roc.test(ROC1, ROC2)
text(0.4, 0.4, labels=paste("p-value =", format.pval(testobj$p.value)), adj=c(0, .5))
legend("bottomright", legend=c("Luminal", "TNBC"), col=c("#377eb8", "#008600"), lwd=1)
dev.off()

## Saving ROC plot in PDF format
pdf(file = "Comparative/pdf/Ceramide_C16.pdf", width=5, height=5)
ROC1 = roc(data1$Type, glm.fit1$fitted.values, plot=TRUE, legacy.axes=TRUE, col="#377eb8", lwd=1, main = "Ceramide_C16", type="l")
ROC2 = plot.roc(data2$Type, glm.fit2$fitted.values, col="#008600", lwd=1, type="l", add=TRUE)

testobj <- roc.test(ROC1, ROC2)
text(0.4, 0.4, labels=paste("p-value =", format.pval(testobj$p.value)), adj=c(0, .5))
legend("bottomright", legend=c("Luminal", "TNBC"), col=c("#377eb8", "#008600"), lwd=1)
dev.off()
