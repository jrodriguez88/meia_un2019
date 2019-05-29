####  Scripts para curso de Metodos estadisticos en la investigacion agricola
####  Universidad Nacional de Colombia - Palmira
####  Jeferson Rodriguez-Espinoza 2019
####  https://github.com/jrodriguez88
####
#### 1. Estadistica Descriptiva

## Cargar paquetes!! se deben instalar ----> install.packages("tidyverse")
library(tidyverse)

## Leer datos
#data <- read.table("clipboard", header=F) 
data <- read_csv("data.csv")

## Definir titulos y labels
variable <- "Tamaño de finca (ha)"
titulo <- paste0("Analisis descriptivo y de frecuencias para ", variable)
var <- names(data)

## Tabla de frecuencias
tab_frecuencias <- data %>% group_by_at(1) %>% arrange_at(1) %>% 
    mutate(Frecuencia = n()) %>% distinct() %>% ungroup() %>%
    mutate(Porcentaje = 100*Frecuencia/nrow(data),
           Frecuencia_acumulada = cumsum(Frecuencia),
           Porcentaje_acumulado = cumsum(Porcentaje))

## Histograma de frecuencias
hist(data[[1]], xlab = variable, main = (paste0("Histograma de ", variable)))



## Estadisticos
moda <- function(data){ 
    unique(data)[which.max(tabulate(match(data, unique(data))))]
}
summary(data)

data %>%
    summarise_at(1, funs(n(), mean, median, moda, min, max, sum, sd, var)) %>%
    mutate(coef_var = sd/mean,
           rango = max-min)

## pueba T
t.test(data[[1]])

## Test Normalidad
shapiro.test(data[[1]])
nortest::lillie.test(data[[1]])


