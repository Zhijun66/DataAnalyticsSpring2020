#In class work
#PCA
data("iris")
head(iris)
# creating another dataset from iris dataset that contains the columns from 1 to 4 
irisdata1 <- iris[,1:4]
irisdata1
head(irisdata1)
#Read the documentation for the princomp() function in RStudio.
help("princomp")
principal_components <- princomp(irisdata1, cor = TRUE, score = TRUE)
summary(principal_components)
plot(principal_components)
plot(principal_components, type = "l")
biplot(principal_components)

#PCA on Boston dataset
install.packages('MASS')
data(Boston, package="MASS")
Boston
help(prcomp)
pca_out <- prcomp(Boston,scale. = T)
pca_out
plot(pca_out)
help(biplot)
biplot(pca_out, scale = 0)
boston_pc <- pca_out$x
boston_pc

#PCA on Wine date
wine_data <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", sep = ",")
head(wine_data)
nrow(wine_data)
dim(wine_data)
colnames(wine_data) <- c("Cvs", "Alcohol", 
                         "Malic_Acid", "Ash", "Alkalinity_of_Ash", 
                         "Magnesium", "Total_Phenols", "Flavanoids", "NonFlavanoid_Phenols",
                         "Proanthocyanins", "Color_Intensity", "Hue", "OD280/OD315_of_Diluted_Wine", 
                         "Proline")
head(wine_data)
help("heatmap")
heatmap(cor(wine_data),Rowv = NA, Colv = NA) 
help("factor")
cultivar_classes <- factor(wine_data$Cvs) 
cultivar_classes
help("prcomp")
wine_data_PCA <- prcomp(scale(wine_data[,-1]))
summary(wine_data_PCA)
