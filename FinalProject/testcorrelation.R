library(corrplot)

M <- cor(TedRaw)
plot.new()
corrplot(M, method = "circle")

ggcorr(TedRaw, nbreaks=8, palette='RdGy', label=TRUE, label_size=5, label_color='white')