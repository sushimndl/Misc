library(corrplot)
library(RColorBrewer)
data <- read.table("tnbc_CorrelData.tsv", header = T, sep = "\t")
data_corr <- cor(data, method = c("pearson"))
res <- cor.mtest(data_corr, conf.level = .95)
corrplot(data_corr, method = "circle", tl.srt = 0, type = "lower", order = "hclust", tl.col = "black", tl.cex =0.3, cl.cex=1, col = rev(brewer.pal(n = 8, name = "RdBu")), p.mat = res$p, insig = "label_sig", sig.level = c(.001, .01, .05), pch.cex = 1.5, pch.col = "black")
