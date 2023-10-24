library(readxl)
Soys <- read_excel("~/Downloads/TCC vitor/oRudell_Soybeans experiment.xlsx", 
                   sheet = "Data")
View(Soys)
require(FactoMineR)
require(factoextra)
require(ggplot2)
require(tidyr)
require(dplyr)
require(MASS)
require(reshape2)
require(cowplot)


# center and scale the data
grupo=read_excel("~/Downloads/TCC vitor/oRudell_Soybeans experiment.xlsx", 
                 sheet = "Sheet_parameters")
View(grupo)
it=grupo$Cultivar

pca1=PCA(Soys, scale.unit = FALSE, ncp = 3, graph = TRUE, axes = c(1,2))

plot.PCA(pca1)

fviz_pca_biplot(pca1,axes=c(1,2),geom = c("point"), pointshape=21,
                pointsize=3,fill.ind = it)+
  theme_classic()+
  labs(title = "PCA Soja",
       fill="Herbicide",
       x="Dimension 1 (66.81%)",
       y="Dimension 2 (20.34%)" )



###########

require(MultivariateAnalysis)
require(ggplot2)
require(FactoMineR)
require(factoextra)

Distancia1=Distancia(it, Metodo = 1); Distancia1

Dend= Dendrograma(Distancia1, corte=F,1); Dend


#Pacotes FactoMineR e factoextra
require(FactoMineR)
require(factoextra)

PCAgerado=PCA(Soys, graph = F)








library(readxl)
data1 <- read_excel("~/Downloads/TCC vitor/oRudell_Soybeans experiment.xlsx", 
                   sheet = "data1")
View(data1)

library(ggfortify)
library(ggplot2)
summary(data)
str(data)

install.packages("corrr")
yes
library('corrr')
install.packages("ggcorrplot")
library(ggcorrplot)
install.packages("FactoMineR")
library("FactoMineR")

colSums(is.na(data1))

data_normalized <- scale(data1)
head(data_normalized)

corr_matrix <- cor(data_normalized)
ggcorrplot(corr_matrix)


data.pca <- princomp(corr_matrix)
summary(data.pca)

data.pca$loadings[, 1:2]
install.packages("devtools")
install.packages("factoextra")
require("factoextra")
fviz_eig(data.pca, addlabels = TRUE)

# Graph of the variables
fviz_pca_var(data.pca, col.var = "blue")

fviz_cos2(data.pca, choice = "var", axes = 1:2)

fviz_pca_var(data.pca, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)




