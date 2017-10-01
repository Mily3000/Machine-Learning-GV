################################################################################
##---------------- INTRODUCCIÓN A DATA MINING -------------------------------###
################################################################################

# Capacitador: Ebson David Allende Quintana
# email: david.allende@outlook.com
# Tema: Clustering
# versión: 1.0

################################################################################
# Performing Cluster analysis using k-means clustering
################################################################################

getwd()
setwd('D:/Cursos/18.PEA_ML/0.Data') # Establecer el área de trabajo
consumo_tc <- read.csv("Data_clustering.csv") # Leer un archivo csv


summary(consumo_tc) #Estadísticas Descriptivas
head(consumo_tc) # Revisar los primeros registros
colnames(consumo_tc)

#Análisis de valores extremos
#Número de Transacciones
outlier_values_trx <- boxplot.stats(consumo_tc$TRXS_6M)$out  # outlier values.

dim(consumo_tc) # Dimensión de la dataset
dim(as.data.frame(outlier_values_trx)) # Dimensión de outliers

boxplot(consumo_tc$TRXS_6M, main="Número de Transacciones", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values_trx, collapse=", ")), cex=0.6)

quantile(consumo_tc$TRXS_6M,c(0.01,0.05,0.10,0.25,0.50,0.90,0.95,0.97,0.99)) #Número de transacciones


#Consumo en 6 meses
outlier_values_mts <- boxplot.stats(consumo_tc$MTO_TRXS_6M)$out  # outlier values.
boxplot(consumo_tc$MTO_TRXS_6M, main="Monto Consumido", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values_mts, collapse=", ")), cex=0.6)

quantile(consumo_tc$MTO_TRXS_6M,c(0.01,0.05,0.10,0.25,0.50,0.90,0.95,0.97,0.99)) #Monto por las transacciones 6meses

#Se excluyen a los Low Users / High Users
#Se excluyen a los que consumen menos de S/300 y más de S/25000
consumo_tc_f1<-consumo_tc[consumo_tc$MTO_TRXS_6M>300,] 
consumo_tc_f2<-consumo_tc_f1[consumo_tc_f1$MTO_TRXS_6M<=25000,]

dim(consumo_tc)
dim(consumo_tc_f1)
dim(consumo_tc_f2)

quantile(consumo_tc_f2$MTO_TRXS_6M,c(0.01,0.05,0.95,0.99)) #Monto por las transacciones 6meses


#Se excluyen a los que realizan más de 150 transacciones en 6 meses
consumo_tc_f3<-consumo_tc_f2[consumo_tc_f2$TRXS_6M<=150,]

quantile(consumo_tc_f3$TRXS_6M,c(0.01,0.05,0.95,0.99)) #Número de transacciones

dim(consumo_tc)
dim(consumo_tc_f1)
dim(consumo_tc_f2)
dim(consumo_tc_f3)

# Se renombra
consumo_tc<-consumo_tc_f3

#Función para estandarizar las variables y añadirlas a la data original
rdacb.scale.many <- function (dat, column_nos) {
  nms <- names(dat)
  for (col in column_nos) {
    name <- paste0(nms[col], "_z")
    dat[name] <- scale(dat[, col])
  }
  cat(paste("Scaled", length(column_nos), "variable(s)\n"))
  dat
}


#Aplicamos la función de estandarización
colnames(consumo_tc)
consumo_tc <- rdacb.scale.many(consumo_tc, 2:33)
colnames(consumo_tc)
head(consumo_tc)

#Aplicamos el algoritmo k-means
set.seed(1020) # establecer una semilla aleatoria
colnames(consumo_tc)
fit <- kmeans(consumo_tc[, 35:36], 5) # Le pedimos 5 clusters iniciales
fit # El modelo no supervisado

#Within cluster sum of squares by cluster:
# Is a measure of the total variance in your data set that is explained by the clustering. 
#k-means minimize the within group dispersion and maximize the between-group dispersion.


# Visualizar los clusters en base a las variables usadas
pairs(consumo_tc[,2:3], col=c(1:5)[fit$cluster]) 

# Visualizar el traslape de los clusters en base a dos componentes principales
library(cluster)
clusplot(consumo_tc[,35:36], fit$cluster, color = TRUE, shade = TRUE, labels=0, lines=0)

# Función que permite seleccionar el número de clusters óptimo
rdacb.kmeans.plot <- function (data, num_clust = 50, seed = 9876) 
{
  set.seed(seed)
  ss <- numeric(num_clust)
  ss[1] <- (nrow(data) - 1) * sum(apply(data, 2, var))
  for (i in 2:num_clust) {
    ss[i] <- sum(kmeans(data, centers = i)$withinss)
  }
  plot(1:num_clust, ss, type = "b", pch = 18, xlab = "# Clusters", 
       ylab = "Total within_ss across clusters")
}


# Aplicamos la función de selección de clusters óptimos
rdacb.kmeans.plot(consumo_tc[,35:66])

# Analizamos el grado de poblamiento de las variables
nrow(consumo_tc)
X<-rep(0,30)
X[1]<-round(1-nrow(consumo_tc[consumo_tc$POR_TRXS_RUBRO_ALIM==0,])/nrow(consumo_tc),2)
X[2]<-round(1-nrow(consumo_tc[consumo_tc$POR_TRXS_RUBRO_SERVI==0,])/nrow(consumo_tc),2)
X[3]<-round(1-nrow(consumo_tc[consumo_tc$POR_TRXS_RUBRO_TIENDAS==0,])/nrow(consumo_tc),2)
X[4]<-round(1-nrow(consumo_tc[consumo_tc$POR_TRXS_RUBRO_SALUD==0,])/nrow(consumo_tc),2)
X[5]<-round(1-nrow(consumo_tc[consumo_tc$POR_TRXS_RUBRO_BOT_FAR==0,])/nrow(consumo_tc),2)
X[6]<-round(1-nrow(consumo_tc[consumo_tc$POR_TRXS_RUBRO_DIVER==0,])/nrow(consumo_tc),2)
X[7]<-round(1-nrow(consumo_tc[consumo_tc$POR_TRXS_RUBRO_ESTET==0,])/nrow(consumo_tc),2)
X[8]<-round(1-nrow(consumo_tc[consumo_tc$POR_TRXS_RUBRO_DISPO==0,])/nrow(consumo_tc),2)
X[9]<-round(1-nrow(consumo_tc[consumo_tc$POR_TRXS_RUBRO_INTER==0,])/nrow(consumo_tc),2)
X[10]<-round(1-nrow(consumo_tc[consumo_tc$POR_TRXS_RUBRO_VIAJE_HOTEL==0,])/nrow(consumo_tc),2)
X[11]<-round(1-nrow(consumo_tc[consumo_tc$POR_TRXS_RUBRO_AUTOS_ESTAC==0,])/nrow(consumo_tc),2)
X[12]<-round(1-nrow(consumo_tc[consumo_tc$POR_TRXS_RUBRO_TD_VEST==0,])/nrow(consumo_tc),2)
X[13]<-round(1-nrow(consumo_tc[consumo_tc$POR_TRXS_RUBRO_FAST_RESTA==0,])/nrow(consumo_tc),2)
X[14]<-round(1-nrow(consumo_tc[consumo_tc$POR_TRXS_RUBRO_PAGOD==0,])/nrow(consumo_tc),2)
X[15]<-round(1-nrow(consumo_tc[consumo_tc$POR_TRXS_RUBRO_OTROS==0,])/nrow(consumo_tc),2)
X[16]<-round(1-nrow(consumo_tc[consumo_tc$POR_MTO_RUBRO_ALIM==0,])/nrow(consumo_tc),2)
X[17]<-round(1-nrow(consumo_tc[consumo_tc$POR_MTO_RUBRO_SERVI==0,])/nrow(consumo_tc),2)
X[18]<-round(1-nrow(consumo_tc[consumo_tc$POR_MTO_RUBRO_TIENDAS==0,])/nrow(consumo_tc),2)
X[19]<-round(1-nrow(consumo_tc[consumo_tc$POR_MTO_RUBRO_SALUD==0,])/nrow(consumo_tc),2)
X[20]<-round(1-nrow(consumo_tc[consumo_tc$POR_MTO_RUBRO_BOT_FAR==0,])/nrow(consumo_tc),2)
X[21]<-round(1-nrow(consumo_tc[consumo_tc$POR_MTO_RUBRO_DIVER==0,])/nrow(consumo_tc),2)
X[22]<-round(1-nrow(consumo_tc[consumo_tc$POR_MTO_RUBRO_ESTET==0,])/nrow(consumo_tc),2)
X[23]<-round(1-nrow(consumo_tc[consumo_tc$POR_MTO_RUBRO_DISPO==0,])/nrow(consumo_tc),2)
X[24]<-round(1-nrow(consumo_tc[consumo_tc$POR_MTO_RUBRO_INTER==0,])/nrow(consumo_tc),2)
X[25]<-round(1-nrow(consumo_tc[consumo_tc$POR_MTO_RUBRO_VIAJE_HOTEL==0,])/nrow(consumo_tc),2)
X[26]<-round(1-nrow(consumo_tc[consumo_tc$POR_MTO_RUBRO_AUTOS_ESTAC==0,])/nrow(consumo_tc),2)
X[27]<-round(1-nrow(consumo_tc[consumo_tc$POR_MTO_RUBRO_TD_VEST==0,])/nrow(consumo_tc),2)
X[28]<-round(1-nrow(consumo_tc[consumo_tc$POR_MTO_RUBRO_FAST_RESTA==0,])/nrow(consumo_tc),2)
X[29]<-round(1-nrow(consumo_tc[consumo_tc$POR_MTO_RUBRO_PAGOD==0,])/nrow(consumo_tc),2)
X[30]<-round(1-nrow(consumo_tc[consumo_tc$POR_MTO_RUBRO_OTROS==0,])/nrow(consumo_tc),2)

Y<-names(consumo_tc[,4:33])

Z<-paste0(Y,"-",X)
Z

#Seleccionamos las variables más pobladas
colnames(consumo_tc)
consumo_tc2<-consumo_tc[,c("CODMES","TRXS_6M","MTO_TRXS_6M","POR_TRXS_RUBRO_ALIM",
  "POR_TRXS_RUBRO_TD_VEST","POR_TRXS_RUBRO_FAST_RESTA",
  "POR_MTO_RUBRO_ALIM","POR_MTO_RUBRO_TD_VEST","POR_MTO_RUBRO_FAST_RESTA",
  "TRXS_6M_z","MTO_TRXS_6M_z","POR_TRXS_RUBRO_ALIM_z",
  "POR_TRXS_RUBRO_TD_VEST_z","POR_TRXS_RUBRO_FAST_RESTA_z",
  "POR_MTO_RUBRO_ALIM_z","POR_MTO_RUBRO_TD_VEST_z","POR_MTO_RUBRO_FAST_RESTA_z")]

dim(consumo_tc2)
colnames(consumo_tc2)
#Óptimo de clusters
rdacb.kmeans.plot(consumo_tc2[,10:17])

# Buscaremos 10 clusters
fit <- kmeans(consumo_tc2[, 10:17],5) # Le pedimos 5 clusters
fit # El modelo no supervisado

cluster_tc<-fit$cluster # Objeto que almacena el cluster obtenido
consumo_tc3<-cbind(consumo_tc2,cluster_tc) # Nuevo Data Frame
head(consumo_tc3)
dim(consumo_tc3)

#Sacamos una muestra aleatoria
consumo_tc3_sam<-consumo_tc3[sample(1:nrow(consumo_tc3), 1000,replace=FALSE),]

# Visualizar los clusters en base a las variables usadas
pairs(consumo_tc3_sam[,2:9], col=c(1:5)[fit$cluster]) 

# Visualizar el traslape de los clusters en base a dos componentes principales
library(cluster)
colnames(consumo_tc3_sam)
clusplot(consumo_tc3_sam[,2:9], consumo_tc3_sam$cluster_tc, color = TRUE, shade = TRUE, labels=0, lines=0)


# Analizamos las variables usadas por cada cluster vs. general
head(consumo_tc3)
colnames(consumo_tc3)
# Analizamos la variable MTO_TRXS_6M

head(consumo_tc3[which(consumo_tc3$cluster_tc==1),c(2,18)])

mt_tc_c1<-mean(consumo_tc3[which(consumo_tc3$cluster_tc==1),2])
mt_tc_c2<-mean(consumo_tc3[which(consumo_tc3$cluster_tc==2),2])
mt_tc_c3<-mean(consumo_tc3[which(consumo_tc3$cluster_tc==3),2])
mt_tc_c4<-mean(consumo_tc3[which(consumo_tc3$cluster_tc==4),2])
mt_tc_c5<-mean(consumo_tc3[which(consumo_tc3$cluster_tc==5),2])
mt_total<-mean(consumo_tc3[,2])

mt<-cbind(mt_tc_c1,mt_tc_c2,mt_tc_c3,mt_tc_c4,mt_tc_c5,mt_total)

# Analizamos la variable Cylinders
cyl_c1<-mean(auto_cluster[which(auto_cluster$cluster_autos==1),3])
cyl_c2<-mean(auto_cluster[which(auto_cluster$cluster_autos==2),3])
cyl_c3<-mean(auto_cluster[which(auto_cluster$cluster_autos==3),3])
cyl_c4<-mean(auto_cluster[which(auto_cluster$cluster_autos==4),3])
cyl_c5<-mean(auto_cluster[which(auto_cluster$cluster_autos==5),3])
cyl_total<-mean(auto_cluster[,3])

cyl<-cbind(cyl_c1,cyl_c2,cyl_c3,cyl_c4,cyl_c5,cyl_total)

# Analizamos la variable Displacement
dis_c1<-mean(auto_cluster[which(auto_cluster$cluster_autos==1),4])
dis_c2<-mean(auto_cluster[which(auto_cluster$cluster_autos==2),4])
dis_c3<-mean(auto_cluster[which(auto_cluster$cluster_autos==3),4])
dis_c4<-mean(auto_cluster[which(auto_cluster$cluster_autos==4),4])
dis_c5<-mean(auto_cluster[which(auto_cluster$cluster_autos==5),4])
dis_total<-mean(auto_cluster[,4])

dis<-cbind(dis_c1,dis_c2,dis_c3,dis_c4,dis_c5,dis_total)

# Analizamos la variable Horsepower
hp_c1<-mean(auto_cluster[which(auto_cluster$cluster_autos==1),5])
hp_c2<-mean(auto_cluster[which(auto_cluster$cluster_autos==2),5])
hp_c3<-mean(auto_cluster[which(auto_cluster$cluster_autos==3),5])
hp_c4<-mean(auto_cluster[which(auto_cluster$cluster_autos==4),5])
hp_c5<-mean(auto_cluster[which(auto_cluster$cluster_autos==5),5])
hp_total<-mean(auto_cluster[,5])

hp<-cbind(hp_c1,hp_c2,hp_c3,hp_c5,hp_c5,hp_total)

# Analizamos la variable Weight
wt_c1<-mean(auto_cluster[which(auto_cluster$cluster_autos==1),6])
wt_c2<-mean(auto_cluster[which(auto_cluster$cluster_autos==2),6])
wt_c3<-mean(auto_cluster[which(auto_cluster$cluster_autos==3),6])
wt_c4<-mean(auto_cluster[which(auto_cluster$cluster_autos==4),6])
wt_c5<-mean(auto_cluster[which(auto_cluster$cluster_autos==5),6])
wt_total<-mean(auto_cluster[,6])

hp<-cbind(wt_c1,wt_c2,wt_c3,wt_c4,wt_c5,wt_total)

# Analizamos la variable Acceleration
ac_c1<-mean(auto_cluster[which(auto_cluster$cluster_autos==1),7])
ac_c2<-mean(auto_cluster[which(auto_cluster$cluster_autos==2),7])
ac_c3<-mean(auto_cluster[which(auto_cluster$cluster_autos==3),7])
ac_c4<-mean(auto_cluster[which(auto_cluster$cluster_autos==4),7])
ac_c5<-mean(auto_cluster[which(auto_cluster$cluster_autos==5),7])
ac_total<-mean(auto_cluster[,7])

ac<-cbind(ac_c1,ac_c2,ac_c3,ac_c4,ac_c5,ac_total)

# Matriz General

comparativo_var<-rbind(mpg,cyl,dis,hp,ac)
comparativo_var

################################################################################
# Performing Cluster analysis using hierical clustering
################################################################################

# Lectura data en formato csv
auto <- read.csv("Clustering/auto-mpg.csv")

# Función para estandarizar las variables a usar
rdacb.scale.many <- function (dat, column_nos) {
  nms <- names(dat)
  for (col in column_nos) {
    name <- paste0(nms[col], "_z")
    dat[name] <- scale(dat[, col])
  }
  cat(paste("Scaled", length(column_nos), "variable(s)\n"))
  dat
}

# Aplicamos la función para estandarizar las variables
auto <- rdacb.scale.many(auto, 2:7)

names(auto)

# Aplicamos el cluster jerárquico
dis <- dist(auto[,10:15], method = "euclidean")

fit <- hclust(dis, method = "ward")

plot(fit, labels = FALSE, hang = 0)

rect.hclust(fit, k=4, border="blue")

cluster <- cutree(fit, k=4)

cluster

# Analizamos las variables usadas por cada cluster vs. general
cluster_autos<-cluster #Renombramos el cluster
auto_cluster<-cbind(auto,cluster_autos) # Añadimos la variable cluster al dataset inicial
head(auto_cluster)

# Analizamos la variable MPG
mpg_c1<-mean(auto_cluster[which(auto_cluster$cluster_autos==1),2])
mpg_c2<-mean(auto_cluster[which(auto_cluster$cluster_autos==2),2])
mpg_c3<-mean(auto_cluster[which(auto_cluster$cluster_autos==3),2])
mpg_c4<-mean(auto_cluster[which(auto_cluster$cluster_autos==4),2])
mpg_c5<-mean(auto_cluster[which(auto_cluster$cluster_autos==5),2])
mpg_total<-mean(auto_cluster[,2])

mpg<-cbind(mpg_c1,mpg_c2,mpg_c3,mpg_c4,mpg_c5,mpg_total)

# Analizamos la variable Cylinders
cyl_c1<-mean(auto_cluster[which(auto_cluster$cluster_autos==1),3])
cyl_c2<-mean(auto_cluster[which(auto_cluster$cluster_autos==2),3])
cyl_c3<-mean(auto_cluster[which(auto_cluster$cluster_autos==3),3])
cyl_c4<-mean(auto_cluster[which(auto_cluster$cluster_autos==4),3])
cyl_c5<-mean(auto_cluster[which(auto_cluster$cluster_autos==5),3])
cyl_total<-mean(auto_cluster[,3])

cyl<-cbind(cyl_c1,cyl_c2,cyl_c3,cyl_c4,cyl_c5,cyl_total)

# Analizamos la variable Displacement
dis_c1<-mean(auto_cluster[which(auto_cluster$cluster_autos==1),4])
dis_c2<-mean(auto_cluster[which(auto_cluster$cluster_autos==2),4])
dis_c3<-mean(auto_cluster[which(auto_cluster$cluster_autos==3),4])
dis_c4<-mean(auto_cluster[which(auto_cluster$cluster_autos==4),4])
dis_c5<-mean(auto_cluster[which(auto_cluster$cluster_autos==5),4])
dis_total<-mean(auto_cluster[,4])

dis<-cbind(dis_c1,dis_c2,dis_c3,dis_c4,dis_c5,dis_total)

# Analizamos la variable Horsepower
hp_c1<-mean(auto_cluster[which(auto_cluster$cluster_autos==1),5])
hp_c2<-mean(auto_cluster[which(auto_cluster$cluster_autos==2),5])
hp_c3<-mean(auto_cluster[which(auto_cluster$cluster_autos==3),5])
hp_c4<-mean(auto_cluster[which(auto_cluster$cluster_autos==4),5])
hp_c5<-mean(auto_cluster[which(auto_cluster$cluster_autos==5),5])
hp_total<-mean(auto_cluster[,5])

hp<-cbind(hp_c1,hp_c2,hp_c3,hp_c5,hp_c5,hp_total)

# Analizamos la variable Weight
wt_c1<-mean(auto_cluster[which(auto_cluster$cluster_autos==1),6])
wt_c2<-mean(auto_cluster[which(auto_cluster$cluster_autos==2),6])
wt_c3<-mean(auto_cluster[which(auto_cluster$cluster_autos==3),6])
wt_c4<-mean(auto_cluster[which(auto_cluster$cluster_autos==4),6])
wt_c5<-mean(auto_cluster[which(auto_cluster$cluster_autos==5),6])
wt_total<-mean(auto_cluster[,6])

hp<-cbind(wt_c1,wt_c2,wt_c3,wt_c4,wt_c5,wt_total)

# Analizamos la variable Acceleration
ac_c1<-mean(auto_cluster[which(auto_cluster$cluster_autos==1),7])
ac_c2<-mean(auto_cluster[which(auto_cluster$cluster_autos==2),7])
ac_c3<-mean(auto_cluster[which(auto_cluster$cluster_autos==3),7])
ac_c4<-mean(auto_cluster[which(auto_cluster$cluster_autos==4),7])
ac_c5<-mean(auto_cluster[which(auto_cluster$cluster_autos==5),7])
ac_total<-mean(auto_cluster[,7])

ac<-cbind(ac_c1,ac_c2,ac_c3,ac_c4,ac_c5,ac_total)

# Matriz General

comparativo_var<-rbind(mpg,cyl,dis,hp,ac)
comparativo_var



