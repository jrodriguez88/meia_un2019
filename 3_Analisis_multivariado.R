####  Scripts para curso de Metodos estadisticos en la investigacion agricola
####  Universidad Nacional de Colombia - Palmira
####  Jeferson Rodriguez-Espinoza 2019
####  https://github.com/jrodriguez88
####
#### 3. Analisis Multivariado


## Packages
library(tidyverse)
library(ade4)
library(FactoMineR)
library(skimr)
library(corrplot)
library(factoextra)

##  Load Data
data <- read.csv("data.csv", header=T, row.names = 1) %>%
    mutate_at(1:5, as.factor)

## Data Structure 
str(data)

## Summary 
skim(data)

moda <- function(data){ 
    unique(data)[which.max(tabulate(match(data, unique(data))))]
}

data %>% select(6:12) %>% as.list() %>%
    map(~.x %>% enframe(name = NULL) %>% 
            summarise_at(1, funs(n(), mean, median, moda, min, max, sum, sd, var)) %>%
    mutate(coef_var = 100*sd/mean,
           rango = max-min)) %>%
    bind_rows(.id="Var")

## Plot data
data %>% dplyr::select(-Genot) %>% gather("var", "value", -c(Cruza, Filial, Madurez, Parental)) %>%
    ggplot(aes(value)) + geom_histogram(color = "gray") +
    facet_wrap(var ~., scales = "free") +
    theme_bw()

data %>% dplyr::select(-Genot) %>% gather("var", "value", -c(Cruza, Filial, Madurez, Parental)) %>%
    ggplot(aes(var, value, color = Filial)) + geom_boxplot() +
    facet_grid(Cruza ~., scales = "free") +
    theme_bw()


# remove/change outlier
data[31,]$DFr <- 2.13

#Correlation matrix
sig_cor <- cor.mtest(data[, 6:12], conf.level = .95)
cor(data[, 6:12])
sig_cor$p
corrplot(cor(data[, 6:12]), p.mat = sig_cor$p,
         insig = "p-value", sig.level = .05)


#par(mfrow=c(1,2)) 
# Se escala paa que todos ingresen con varianza = 1
pca <- PCA(data[-1,], quali.sup = c(1:5), scale.unit = T, ncp = 7, graph = T)


# Extract the results for variables
var <- get_pca_var(pca)
var
var$contrib
pca$eig
var$cos2

# Graph of variables
fviz_screeplot(pca, addlabels = TRUE, ylim = c(0, 50))
fviz_pca_ind(pca, axes = c(1, 2), col.var = "black")#, habillage = "Cruza")
fviz_pca_var(pca, axes = c(1, 2), col.var = "black", repel = TRUE)

fviz_pca_var(pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)


# Contributions of variables to PC1
fviz_contrib(pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(pca, choice = "var", axes = 2, top = 10)
# Contributions of variables to PC3
fviz_contrib(pca, choice = "var", axes = 3, top = 10)


# Biplot of individuals and variables
fviz_pca_biplot(pca, repel = TRUE, col.var = "red")

plotellipses(pca)

get_eig(pca)
var$contrib

fviz_pca_ind(pca,
             label = "none", # hide individual labels
             habillage = "Cruza", # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE # Concentration ellipses
)


### Analisis de correspondencia multiple, incluir genotipo?
mca  <- MCA(data[,1:5], quali.sup = 1)

# Extract the results for variable categories
mca_var <- get_mca_var(mca)
mca_ind_ <- get_mca_ind(mca)

# Visualize variable categorie contributions on axes 1
fviz_contrib(mca, choice ="var", axes = 1)
fviz_contrib(mca, choice ="var", axes = 2)
fviz_contrib(mca, choice ="var", axes = 3)
# Visualize individual contributions on axes 1
# select the top 20
fviz_contrib(mca, choice ="ind", axes = 1, top = 10)
fviz_contrib(mca, choice ="ind", axes = 2, top = 10)

fviz_mca_ind(mca,  habillage = "Cruza",
             addEllipses = TRUE, repel = TRUE)

fviz_mca_var(mca, repel = TRUE)


fviz_mca_biplot(mca, repel = TRUE)

hcpc <- HCPC(mca, metric="euclidean", method="ward", graph = T) # grafico error vs numero de cluster

fviz_dend(hcpc, 
          cex = 0.7,                     # Label size
          palette = "jco",               # Color palette see ?ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
          rect_border = "jco",           # Rectangle color
          labels_track_height = 0.8      # Augment the room for labels
)


fviz_cluster(hcpc,
             repel = TRUE,            # Avoid label overlapping
             show.clust.cent = TRUE, # Show cluster centers
             palette = "jco",         # Color palette see ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Factor map"
)

hcpc$data.clust
# Principal components + tree
plot(hcpc, choice = "3D.map")
cluster_cl <- hcpc$data.clust

hcpc$desc.var$test.chi2
hcpc$desc.var$category
hcpc$desc.axes

cluster_cl %>% ggplot(aes(Cruza, clust, label=Genot)) + geom_jitter() + theme_bw()+geom_text(aes(label=Genot),hjust=0, vjust=0)
