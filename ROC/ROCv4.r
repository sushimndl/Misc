library("pROC")
library("ggplot2")

data1 = read.csv("/media/csb/NewVolume/susmita/Ujjaini_Animesh/ROC plots/Comparative/Grade2/Ceramide_C16.tsv", header = T, sep="\t")
data2 = read.csv("/media/csb/NewVolume/susmita/Ujjaini_Animesh/ROC plots/Comparative/Grade3/Ceramide_C16.tsv", header = T, sep="\t")

glm.fit1=glm(data1$Type ~ data1$Ceramide_C16, family=binomial)	
glm.fit2=glm(data2$Type ~ data2$Ceramide_C16, family=binomial)

rocobj1 = roc(data1$Type, glm.fit1$fitted.values, ci = T)	# ROC object 1
rocobj2 = roc(data2$Type, glm.fit2$fitted.values, ci = T)	# ROC object 2

testobj <- roc.test(rocobj1, rocobj2)
pval = paste("p-value = ", format.pval(testobj$p.value))

g = ggroc(list(Grade2 = rocobj1, Grade3 = rocobj2), legacy.axes = T, size = 3) + scale_color_manual(values=c("darkblue", "darkgreen"))

g + annotate("text", x = 0.80, y = 0.25, label = pval, col = "black", size = 10, fontface ="bold") + theme_classic() + ggtitle("Ceramide_C16") + geom_abline(intercept = 0, slope = 1, color = "darkgrey", linetype = "solid", size = 2) + theme(panel.border = element_rect(color = "black", fill = NA, size = 5)) +  ggeasy::easy_center_title() + xlab("1 - Specificity") + ylab("Sensitivity") + theme(text =  element_text(color = "black", size = 30, face = "bold"), axis.text = element_text(color = "black"), axis.ticks = element_line(colour = "black", size = 3), axis.ticks.length=unit(.40, "cm"), legend.position=c(0.88,0.12), legend.title=element_blank(), legend.spacing.y = unit(0, "mm"), legend.background = element_blank(), legend.box.background = element_rect(colour = "black", size = 1)) 

ggsave("Plots/Comparative/Ceramide_C16.pdf", width = 10, height = 10, dpi = 3000)
