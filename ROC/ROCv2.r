#### ***********Comparative between two ROC AUC curves**********

setwd("/media/csb/NewVolume/susmita/Ujjaini_Animesh/ROC plots")	# set directory

## load packages
library("pROC")
library("ROCR")
library("verification")

## load the Data file
data1 = read.csv("/media/csb/NewVolume/susmita/Ujjaini_Animesh/ROC plots/Comparative/Grade2/Ceramide_C16.tsv", header = T, sep="\t")
data2 = read.csv("/media/csb/NewVolume/susmita/Ujjaini_Animesh/ROC plots/Comparative/Grade3/Ceramide_C16.tsv", header = T, sep="\t")

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
legend("bottomright", legend=c("Grade 2", "Grade 3"), col=c("#377eb8", "#008600"), lwd=1)
dev.off()

## Saving ROC plot in PDF format
pdf(file = "Comparative/pdf/Ceramide_C16.pdf", width=5, height=5)
ROC1 = roc(data1$Type, glm.fit1$fitted.values, plot=TRUE, legacy.axes=TRUE, col="#377eb8", lwd=1, main = "Ceramide_C16", type="l")
ROC2 = plot.roc(data2$Type, glm.fit2$fitted.values, col="#008600", lwd=1, type="l", add=TRUE)

testobj <- roc.test(ROC1, ROC2)
text(0.4, 0.4, labels=paste("p-value =", format.pval(testobj$p.value)), adj=c(0, .5))
legend("bottomright", legend=c("Grade 2", "Grade 3"), col=c("#377eb8", "#008600"), lwd=1)
dev.off()

# Data 1
ROCobj1 = roc(data1$Type, glm.fit1$fitted.values, ci = T)	# ROC1 object
CI1 = ROCobj1$ci	# 95% CI

ROC1 = roc.area(data1$Type, glm.fit1$fitted.values)
AUC1 = ROC1$A	# AUC value
PosEve1 = ROC1$n.events	# No of positive events
NegEve1 = ROC1$n.noevents	# No of negative events


# Data 2
ROCobj2 = roc(data2$Type, glm.fit2$fitted.values, ci = T)	# ROC2 object
CI2 = ROCobj2$ci	# 95% CI

ROC2 = roc.area(data2$Type, glm.fit2$fitted.values)
AUC2 = ROC2$A	# AUC value
PosEve2 = ROC2$n.events	# No of positive events
NegEve2 = ROC2$n.noevents	# No of negative events


# Comparison between ROC1 and ROC2
AUCDiff = abs(AUC1-AUC2) # Difference between two AUCs
Pval = testobj$p.value	# p-value
TotalPos = PosEve1 + PosEve2
TotalNeg = NegEve1 + NegEve2

res=paste("Ceramide_C16", TotalNeg, TotalPos, PosEve1, NegEve1, AUC1, CI1[1], CI1[3], PosEve2, NegEve2, AUC2, CI2[1], CI2[3], AUCDiff, Pval, collapse = "\n")
cat(res, file = "Grade2VsGrade3_Table_ROC.tsv", sep = "\t", fill = FALSE, labels = NULL, append = T, "\n")
