library("xlsx") #Para leer archivos excel
library(FinCal)
library(e1071)
library(tidyverse)
library(psych)
library(car)
library(nortest)
library(modeest)

## ---- chunk-1 ----
datos <- read.xlsx("Libro1.xlsx", sheetIndex = 1)
pre_temprano <- datos %>% filter(Época.histórica == 1)
pre_tardio <- datos %>% filter(Época.histórica == 2)


tabla_resumen <- datos %>%
  group_by(Época.histórica) %>%  
  summarise( 
    N = n(),       
    Media = round(mean(Anchura.del.cráneo, na.rm = TRUE),digits = 1),
    Mediana = median(Anchura.del.cráneo, na.rm = TRUE),
    Moda = mlv(Anchura.del.cráneo, method = 'mfv'), 
    Rango =  diff(range(Anchura.del.cráneo)),
    Desviacion = round(sd(Anchura.del.cráneo), digits = 2),
    Pearson = round(coefficient.variation(sd=sd(Anchura.del.cráneo), 
                    avg = mean(Anchura.del.cráneo)), 
                    digits = 4),
    Fisher = round(skewness(Anchura.del.cráneo, na.rm = TRUE, type = 3), 
                   digits = 3),
    Curtosis = round(kurtosis(Anchura.del.cráneo, na.rm = TRUE), digits = 4)
  )



percentiles <- datos %>%
  group_by(Época.histórica,) %>%  
  summarise( 
    Casos = n(),       
    p00 = quantile(Anchura.del.cráneo, probs = 0.00, na.rm=T),
    p25 = quantile(Anchura.del.cráneo, probs = 0.25, na.rm=T),
    p50 = quantile(Anchura.del.cráneo, probs = 0.50, na.rm=T),
    p75 = quantile(Anchura.del.cráneo, probs = 0.75, na.rm=T),
    p100 = quantile(Anchura.del.cráneo, probs = 1.00, na.rm=T),
    
  )


knitr::kable(tabla_resumen[1:2, 1:10],
             col.names = c("Epoca", names(tabla_resumen)[-1]),
             caption = "Anchura de craneo periodo predinastico temprano",
             align = "cccccccccc",
             format = 'pipe')

knitr::kable(percentiles[1, 1:7],
             caption = "Cuartiles periodo predinástico temprano",
             col.names = c("Epoca",
                           "N",
                           "0%",
                           "25%",
                           "50%",
                           "75%",
                           "100%"),
             align = "ccccccc",
             format = 'pipe')


## ---- chunk-b ----


knitr::kable(tabla_resumen[3, 1:10],
             caption = "Anchura de cráneo periodo predinástico tardío",
             col.names = c("Epoca", names(tabla_resumen)[-1]),
             align = "cccccccccc",
             format = 'pipe')


knitr::kable(percentiles[2, 1:7],
             caption = "Cuartiles periodo predinástico tardío",
             col.names = c("N",
                           "Casos",
                           "0%",
                           "25%",
                           "50%",
                           "75%",
                           "100%"),
             align = "ccccccc",
             format = 'pipe')


## ---- chunk-2 ----
#Boxplot e histograma periodo predinastico temprano

par(mfcol = c(1, 2), cex=0.7) 
boxplot(pre_temprano$Anchura.del.cráneo, col = 'lightblue', 
        main = "Boxplot predinastico temprano")


hist(pre_temprano$Anchura.del.cráneo, 
     col = 'lightblue', 
     main = 'Histograma predinastico temprano',
     ylim=c(0,14),
     xlab = NULL,
     ylab=NULL)


## ---- chunk-3 ----
# Boxplot e histograma para data frame que representa el periodo predinastico
# tardio 


par(mfcol = c(1, 2), cex=0.7) 
boxplot(pre_tardio$Anchura.del.cráneo, col = 'pink', 
        main = "Epoca predinastica tardia")


hist(pre_tardio$Anchura.del.cráneo, 
     col = 'pink', 
     main = 'Anchura craneo predinastico tardio',
     ylim=c(0,14),
     xlab = NULL,
     ylab=NULL)

## ---- chunk-4 ----
#Estudio preliminar de la normalidad"

histDenNorm <- function (x, ...) {
  hist(x, col = 'lightblue', labels=seq(25,32, 1), xlab = NULL,...) 
  lines(density(x), col = "blue", lwd = 2) 
  x2 <- seq(min(x), max(x), length = 40)
  f <- dnorm(x2, mean(x), sd(x))
  lines(x2, f, col = "red", lwd = 2) 
  legend("topright", c("Sampled density", "Normal density"), box.lty = 0,
         lty = 1, col = c("blue", "red"), cex = 0.40)
}

temprana <- (pre_temprano$Anchura.del.cráneo)
tardia <- (pre_tardio$Anchura.del.cráneo)
par(mfcol = c(1, 2), cex = 1) 
histDenNorm(temprana, prob = TRUE, main = "Periodo temprano")
histDenNorm(tardia, prob = TRUE, main = "Periodo tardio")



par(mfcol = c(1, 2), cex = 1) 
qqnorm(pre_temprano$Anchura.del.cráneo, col = 'red', main = "Periodo temprano", 
       xlab = NULL, 
       ylab = NULL)
qqline(pre_temprano$Anchura.del.cráneo )

qqnorm(pre_tardio$Anchura.del.cráneo, col = 'darkgreen',
       main = "Periodo tardio", xlab = NULL, ylab = NULL)
qqline(pre_tardio$Anchura.del.cráneo)


## ---- kolmogorov-temprano ----
ks.test(pre_temprano$Anchura.del.cráneo, pnorm, mean(pre_temprano$Anchura.del.cráneo) , 
        sd(pre_temprano$Anchura.del.cráneo) , exact = FALSE)

## ---- kolmogorov-tardio ----
ks.test(pre_tardio$Anchura.del.cráneo, pnorm, mean(pre_tardio$Anchura.del.cráneo) , 
        sd(pre_tardio$Anchura.del.cráneo) , exact = FALSE)


## ---- chunk-shapiro1 ----
#Shapiro test
shapiro.test(pre_temprano$Anchura.del.cráneo)

## ---- chunk-shapiro2 ----
#Shapiro test
shapiro.test(pre_tardio$Anchura.del.cráneo)


## ---- chunk-igualvar ----

boxplot(datos$Anchura.del.cráneo ~ datos$Época.histórica, 
        col = c("lightblue", "pink"), 
        names=c("Temprano", "Tardio"),
        xlab = "Epoca historica",
        ylab = 'Anchura del craneo')



## ---- chunk-levene ----

#levene test para ver si la varianza es diferente para los datos
leveneTest(Anchura.del.cráneo  ~ as.factor(Época.histórica), 
                     data = datos)

## ---- chunk-t ----

t.test(
  x           = pre_temprano$Anchura.del.cráneo,
  y           = pre_tardio$Anchura.del.cráneo,
  alternative = "two.sided",
  var.equal   = TRUE,
  conf.level  = 0.95
)



## ---- chunk-wilcox ----
wilcox.test(pre_temprano$Anchura.del.cráneo, pre_tardio$Anchura.del.cráneo,
            paired = TRUE) 

## ---- chunk-8 ----
#Con var equal true se asume que las varianzas son iguales
t.test(
  x           = pre_temprano$Anchura.del.cráneo,
  y           = pre_tardio$Anchura.del.cráneo,
  alternative = "two.sided",
  var.equal   = TRUE,
  conf.level  = 0.90
)

t.test(
  x           = pre_temprano$Anchura.del.cráneo,
  y           = pre_tardio$Anchura.del.cráneo,
  alternative = "two.sided",
  var.equal   = TRUE,
  conf.level  = 0.95
)


t.test(
  x           = pre_temprano$Anchura.del.cráneo,
  y           = pre_tardio$Anchura.del.cráneo,
  alternative = "two.sided",
  var.equal   = TRUE,
  conf.level  = 0.99
)




