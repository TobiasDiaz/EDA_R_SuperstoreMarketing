#---Carga de librerias---
library(tidyverse)
library(ggplot2)
library(glmnet)
library(tidymodels)
library(caret)
library(rcompanion)
library(pROC)
library(rsample)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plyr)

#---Leer el Dataset---
df <- read.csv("C:/Users/alex_/kodigo/Proyecto de R/datasets/superstore_data.csv")

#---Analisis de datos---
# Estructura de los datos
str(df)
#Muestra las columnas del dataset y el tipo de valor de cada columna

# Primeras filas
head(df)
#mustra las primeras 6 filas del dataset

# Resumen estadístico
summary(df)   
#Muestra los valores max, min y media de cada columna

# Búsqueda de valores NA
colSums(is.na(df))

# Eliminar el valor NA
df = na.omit(df)
colSums(is.na(df))
str(df)


# Visualice los datos y obtenga información sin procesar

par(mfrow = c(2,1))
barplot(table(df$Education), col = c("lightgreen", "yellow","orange","blue"), 
        ylab = "Number of obsevation", 
        xlab = "Education")
# Muestra la relacion de la cantidad de nivel academico de los clientes.

barplot(table(df$Response), col = c("pink","lightblue"),
        ylab = "Number of observation", xlab = "Response")
# Muestra la relacion de la cantida de clientes que aceptaron la campaña de ofertas

# Vea visualmente la educación predeterminada

ggplot(df, aes(Response, fill = Education)) +
  geom_bar()
# Muestra 2 columnas la primera son quienes No aceptaron la campaña y la segunda son quienes SI aceptaron la campaña

# Construcción de modelos para predecir el uso de la campaña de oferta

set.seed(421)
split = initial_split(df, prop = 0.8, strata = Response)
train = split %>% 
  training()
val_test= split %>% 
  testing()

str(train)
str(val_test)

# Modelo-1
log_lr = glm(Response ~.,family = "binomial", train)
summary(log_lr)

# Este resultado proporciona información sobre los coeficientes, su importancia, el ajuste del modelo y la calidad. 
# Los puntos clave a tener en cuenta son la importancia de los coeficientes, la desviación del modelo y el valor AIC 
# para evaluar el rendimiento del modelo y la interpretación de los coeficientes para predecir las probabilidades de incumplimiento.
# Se puede observar del resultado que El modelo hace su coeficiente depende por el valor que este ingreasdo en cada columna por el cual 
# la fecha en que se registraron en la tienda se etiende con varios valores ya que varia mucho en la fehca de registro del cliente.


# Exactitud de mapeo para el modelo de predicción de la aceptacion de la campaña de ofertas
prd_Val = predict(log_lr, type='response')
prd_default = ifelse(prd_Val > 0.5, 1, 0)
cnf_m = table(prd=prd_default, act=train$Response)
confusionMatrix(cnf_m)

# Aquí obtenemos una buena precisión del modelo, de alrededor del 95 %.

# El modelo predijo un incumplimiento (1). Esto sugiere que, en función de los valores de las variables predictoras proporcionadas, 
# el modelo cree que es más probable que el cliente incumpla con sus obligaciones de deuda.


#Explorar la distribución de la variable objetivo
table(df$Complain)              # Muestra la cantidad de personas que Reclamaron (1) y los que NO Reclamaron (0)
prop.table(table(df$Complain))  # Muestra el porcentaje de los reclamos

#Crea el conjunto de personas que hicieron reclamo (1) y No reclamaron (0)
Complain <- df[df$Complain == 1,]
stay <- df[df$Complain == 0,]


# Atributos numéricos

#  ---Compras con Descuento---
hist(df$NumDealsPurchases,main=NULL,xlab="Compras con Descuento",ylab="Density",col=gray(seq(0.9,0.4,length=10)),cex.axis=1.2,cex.lab=1.5,font.lab=1,freq=FALSE,xlim=c(0,10),ylim=c(0,0.5))
hist(df$NumDealsPurchases,main=NULL,xlab="Compras con Descuento",ylab="Frequency",col=gray(seq(0.9,0.4,length=10)),cex.axis=1.2,cex.lab=1.5,font.lab=1,freq=TRUE,xlim=c(0,10),ylim=c(0,1000))
boxplot(NumDealsPurchases~Complain,df,horizontal=TRUE,xlab="Compras con Descuento",ylab="Reclamo",cex.lab=1.5,font.lab=1)
# Se muestra la relacion de las personas que hicieron reclamos por la cantidad de compras realizadas con "descuento", se puede observar que 2 es la media
# para ambos resultados, pero el cuadrante mas alto de reclamos son quienes compraron 3 productos con descuento.
boxplot.stats(Complain$NumDealsPurchases)
boxplot.stats(stay$NumDealsPurchases)

#---Compras de Catalogo---
hist(df$NumCatalogPurchases,main=NULL,xlab="Compras de Catalogo",ylab="Density",col=gray(seq(0.9,0.4,length=10)),cex.axis=1.2,cex.lab=1.5,font.lab=1,freq=FALSE,xlim=c(0,10),ylim=c(0,0.5))
hist(df$NumCatalogPurchases,main=NULL,xlab="Compras de Catalogo",ylab="Frequency",col=gray(seq(0.9,0.4,length=10)),cex.axis=1.2,cex.lab=1.5,font.lab=1,freq=TRUE,xlim=c(0,10),ylim=c(0,1000))
boxplot(NumCatalogPurchases~Complain,df,horizontal=TRUE,xlab="Compras de Catalogo",ylab="Reclamo",cex.lab=1.5,font.lab=1)
# Se muestra la relacion de las personas que hicieron reclamos por la cantidad de compras realizadas en "Catalogo" se puede observar que 1 es la media de reclamos
# y 2 No, pero el cuadrante mas alto de reclamos son quienes compraron 7 productos por el catalogo.
boxplot.stats(Complain$NumCatalogPurchases)
boxplot.stats(stay$NumCatalogPurchases)

#---Compras en la tienda---
hist(df$NumStorePurchases,main=NULL,xlab="Compras en la tienda",ylab="Density",col=gray(seq(0.9,0.4,length=10)),cex.axis=1.2,cex.lab=1.5,font.lab=1,freq=FALSE,xlim=c(0,20),ylim=c(0,0.3))
hist(df$NumStorePurchases,main=NULL,xlab="Compras en la tienda",ylab="Frequency",col=gray(seq(0.9,0.4,length=10)),cex.axis=1.2,cex.lab=1.5,font.lab=1,freq=TRUE,xlim=c(0,15),ylim=c(0,1000))
boxplot(NumStorePurchases~Complain,df,horizontal=TRUE,xlab="Compras en la tienda",ylab="Reclamo",cex.lab=1.5,font.lab=1)
# Se muestra la relacion de las personas que hicieron reclamos por la cantidad de compras realizadas en "tienda" se puede observar que 3 es la media de reclamos
# y 5 No, pero el cuadrante mas alto de reclamos son quienes compraron 13 productos por en la tienda, teniendo un punto importante a considerar por el tiempo que les lleva pasar en una caja.
boxplot.stats(Complain$NumStorePurchases)
boxplot.stats(stay$NumStorePurchases)

#---Compras en sitio Web---
hist(df$NumWebPurchases,main=NULL,xlab="Compras en sitio Web",ylab="Density",col=gray(seq(0.9,0.4,length=10)),cex.axis=1.2,cex.lab=1.5,font.lab=1,freq=FALSE,xlim=c(0,20),ylim=c(0,0.2))
hist(df$NumWebPurchases,main=NULL,xlab="Compras en sitio Web",ylab="Frequency",col=gray(seq(0.9,0.4,length=10)),cex.axis=1.2,cex.lab=1.5,font.lab=1,freq=TRUE,xlim=c(0,15),ylim=c(0,1000))
boxplot(NumWebPurchases~Complain,df,horizontal=TRUE,xlab="Compras en sitio Web",ylab="Reclamo",cex.lab=1.5,font.lab=1)
# Se muestra la relacion de las personas que hicieron reclamos por la cantidad de compras realizadas en "Sitio Web" se puede observar que 3 es la media de reclamos
# y 4 No, pero el cuadrante mas alto de reclamos son quienes compraron 9 productos por el sitio web.
boxplot.stats(Complain$NumWebPurchases)
boxplot.stats(stay$NumWebPurchases)

# Diagrama de dispersión entre dos variables numéricas (muestra de datos)
df.sample <- df[1:200,]
plot(df.sample$NumDealsPurchases,df.sample$NumCatalogPurchases,xlab="Cant.  oferta",ylab="Cant. Catalogo",cex.lab=1.5,font.lab=1,pch=19)
plot(df.sample$NumDealsPurchases,df.sample$NumStorePurchases,xlab="Cant. Productos en ofert",ylab="Cant.  tienda",cex.lab=1.5,font.lab=1,pch=19)
plot(df.sample$NumDealsPurchases,df.sample$NumWebPurchases,xlab="Cant. Productos en ofert",ylab="Cant.Sitio Web",cex.lab=1.5,font.lab=1,pch=19)
# Se puede observa que la cantidad de productos comprado ya sea en "catalogo", "tienda" y "sitio web", 4 de esos producto eran con descuento 

# Atributos categóricos/binarios

#Evaluar la distribución de las variables categóricas hacia el atributo de clase
#  ---NumDealsPurchases---
barplot(table(df$NumDealsPurchases),xlab="Compras con Descuento",ylab="Frequency",col=gray(seq(0.9,0.4,length=4)),cex.names=1.2,cex.axis=1.2,cex.lab=1.5,font.lab=1,space=0.1,xlim=c(0,6),ylim=c(0,5000))
sum.NumDealsPurchases <- count(df,c("Complain","NumDealsPurchases"))
names(sum.NumDealsPurchases)[3] <- "Num"
ggplot(sum.NumDealsPurchases,aes(factor(NumDealsPurchases),Num,fill = Complain)) + geom_bar(stat="identity", position = "dodge") + scale_color_distiller(palette = "Set1")
# Muestra todas las cantidad de productos comprados y en cuales se hicieron los reclamos segun la cantidad de productos comprados en descuento

#  ---NumCatalogPurchases---
barplot(table(df$NumCatalogPurchases),xlab="Compras de Catalogo",ylab="Frequency",col=gray(seq(0.9,0.4,length=4)),cex.names=1.2,cex.axis=1.2,cex.lab=1.5,font.lab=1,space=0.1,xlim=c(0,6),ylim=c(0,5000))
sum.NumCatalogPurchases <- count(df,c("Complain","NumCatalogPurchases"))
names(sum.NumCatalogPurchases)[3] <- "Num"
ggplot(sum.NumCatalogPurchases,aes(factor(NumCatalogPurchases),Num,fill = Complain)) + geom_bar(stat="identity", position = "dodge") + scale_color_distiller(palette = "Set1")
# Muestra todas las cantidad de productos comprados y en cuales se hicieron los reclamos segun la cantidad de productos comprados por catalogo

#  ---NumStorePurchases---
barplot(table(df$NumStorePurchases),xlab="Compras con Descuento",ylab="Frequency",col=gray(seq(0.9,0.4,length=4)),cex.names=1.2,cex.axis=1.2,cex.lab=1.5,font.lab=1,space=0.1,xlim=c(0,6),ylim=c(0,5000))
sum.NumStorePurchases <- count(df,c("Complain","NumStorePurchases"))
names(sum.NumStorePurchases)[3] <- "Num"
ggplot(sum.NumStorePurchases,aes(factor(NumStorePurchases),Num,fill = Complain)) + geom_bar(stat="identity", position = "dodge") + scale_color_distiller(palette = "Set1")
# Muestra todas las cantidad de productos comprados y en cuales se hicieron los reclamos segun la cantidad de productos comprados en la tienda

#  ---NumWebPurchases---
barplot(table(df$NumWebPurchases),xlab="Compras con Descuento",ylab="Frequency",col=gray(seq(0.9,0.4,length=4)),cex.names=1.2,cex.axis=1.2,cex.lab=1.5,font.lab=1,space=0.1,xlim=c(0,6),ylim=c(0,5000))
sum.NumWebPurchases <- count(df,c("Complain","NumWebPurchases"))
names(sum.NumWebPurchases)[3] <- "Num"
ggplot(sum.NumWebPurchases,aes(factor(NumWebPurchases),Num,fill = Complain)) + geom_bar(stat="identity", position = "dodge") + scale_color_distiller(palette = "Set1")
# Muestra todas las cantidad de productos comprados y en cuales se hicieron los reclamos segun la cantidad de productos comprados en sitio web

#---Income---
prop.table(table(df$Income))
sum.Income <- count(df,c("Complain","Income"))
names(sum.Income)[3] <- "Num"
ggplot(sum.Income,aes(factor(Income),Num,fill = Complain)) + geom_bar(stat="identity", position = "dodge") + scale_color_distiller(palette = "Set1")
prop.table(table(Complain$Income))
prop.table(table(stay$Income))
# Muestra las cantidades de ingreso que realiza al año por familia.
