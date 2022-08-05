library("pROC")
library("ggplot2")

data = read.csv("TNBC_TumorVsNormal/Ceramide_C16.tsv", header = T, sep="\t")
glm.fit=glm(data$Type ~ data$Ceramide_C16, family=binomial)

rocobj = roc(data$Type, glm.fit$fitted.values, ci = T)	# ROC object

g = ggroc(rocobj, legacy.axes = T, colour = "darkred", size = 3)

g + theme_classic() + ggtitle("Ceramide_C16") + geom_abline(intercept = 0, slope = 1, color = "darkgrey", linetype = "solid", size = 2) + theme(panel.border = element_rect(color = "black", fill = NA, size = 5)) +  ggeasy::easy_center_title() + xlab("1 - Specificity") + ylab("Sensitivity") + theme(text =  element_text(color = "black", size = 30, face = "bold"), axis.text = element_text(color = "black"), axis.ticks = element_line(colour = "black", size = 3), axis.ticks.length=unit(.40, "cm"))

ggsave("Plots/TNBC_TumorVsNormal/Ceramide_C16.pdf", width = 8, height = 8, dpi = 3000)
