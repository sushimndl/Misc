#### ***********ROC AUC analysis**********

setwd("/media/csb/NewVolume/susmita/Ujjaini_Animesh/ROC plots")	# set directory

## load packages
library("pROC")
library("ROCR")

## load the Data file
data = read.csv("Luminal_TumorVsNormal/Ceramide_C16.tsv", header = T, sep="\t")

## fit a logistic regression to the data	
glm.fit=glm(data$Type ~ data$Ceramide_C16, family=binomial)
plot(data$Type ~ data$Ceramide_C16, data=data)
lines(data$Ceramide_C16, glm.fit$fitted.values)
par(pty = "s") #par(pty = "s") ## pty sets the aspect ratio of the plot region. "s" - creates a square plotting region

## Saving ROC plot in PNG format
png(filename = "Luminal_TumorVsNormal/png/Ceramide_C16.png", units="in", width=5, height=5, res=300)
roc(data$Type, glm.fit$fitted.values, plot=TRUE, legacy.axes=TRUE, col="#377eb8", lwd=1, main = "Ceramide_C16", type="l")	# roc function in pROC package
dev.off()

## Saving ROC plots in PDF format
pdf(file = "Luminal_TumorVsNormal/pdf/Ceramide_C16.pdf", width=5, height=5)
roc(data$Type, glm.fit$fitted.values, plot=TRUE, legacy.axes=TRUE, col="#377eb8", lwd=1, main = "Ceramide_C16", type="l")	# roc function in pROC package
dev.off()

ROCobj = roc(data$Type, glm.fit$fitted.values, ci = T)	# ROC object
CI = ROCobj$ci	# 95% CI

ROC1 = roc.area(data$Type, glm.fit$fitted.values)
AUC = ROC1$A	# AUC value
Total = ROC1$n.total	# Total no of participants
PosEve = ROC1$n.events	# No of positive events
NegEve = ROC1$n.noevents	# No of negative events
Pval = ROC1$p.value	# p-value

## Calculate best cut-off value
pred = prediction(data$Ceramide_C16, data$Type)
ss = performance(pred, "sens", "spec")
CutOff = ss@alpha.values[[1]][which.max(ss@x.values[[1]]+ss@y.values[[1]])]

ROC2 = coords(ROCobj, x="best", input="threshold", best.method="youden", ret=c("specificity", "sensitivity", "accuracy","precision", "youden"), transpose = FALSE)	# coords function in pROC package
Specificity = ROC2$specificity
Sensitivity = ROC2$sensitivity
Accuracy = ROC2$accuracy
Precision = ROC2$precision
YoudenIndex = (ROC2$youden -1)

res=paste("Ceramide_C16", Total, NegEve, PosEve, AUC, CI[1], CI[3], Pval, CutOff, YoudenIndex, Specificity, Sensitivity, Accuracy, Precision, collapse = "\n")
cat(res, file = "Luminal_TumorVsNormal_Table_ROC.tsv", sep = "\t", fill = FALSE, labels = NULL, append = T, "\n")

