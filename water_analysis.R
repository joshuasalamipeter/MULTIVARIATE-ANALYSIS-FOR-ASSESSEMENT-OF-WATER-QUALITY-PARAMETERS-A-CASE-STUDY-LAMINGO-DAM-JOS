install.packages("psych")
install.packages("ggplot2")
install.packages("factoextra")
install.packages("GPArotation")
install.packages("reshape2")
install.packages(corrplot)
library(ggplot2)
library(factoextra)
library(corrplot)
library(psych)
library(GPArotation)
library(reshape2)


water_quality <-read.csv("rawwater.csv")
water_quality <- data.frame(lapply(water_quality, as.numeric))
print(water_quality_scaled)
kmo_result <- KMO(water_quality)
print(kmo_result)
bartlett.test <- cortest.bartlett(cor(water_quality), n = nrow(water_quality) )
print(bartlett.test )
cor_matrix <- cor(water_quality)
print(cor_matrix)
water_quality_scaled <- scale(water_quality)
pca_result <- prcomp(water_quality_scaled, center = TRUE, scale. = TRUE)
summary(pca_result)
print(pca_result)
loadings <- pca_result$rotation[, 1:2]
print(loadings)
initial_communalities <-rep(1, ncol(water_quality_scaled))
extracted_communalities <- pca_result$communality
print(extracted_communalities)
communalities_df <- data.frame(Parameter = colnames(water_quality_scaled), Initial = initial_communalities, Extraction = extracted_communalities)
print(communalities_df)
fviz_eig(pca_result, addlabels = TRUE)
fviz_pca_var(pca_result,col.var = "black")



fa <- fa(water_quality_scaled , nfactors = 2, rotate = "varimax" , fm = "pa")
loadings <- fa$loadings
print(fa$loadings)
loadings_df <- as.data.frame(loadings[])
summary(fa)
print(fa)
fviz_eig(fa)
eigenvalues <- fa$values
scree_data <- data.frame(Fa)
