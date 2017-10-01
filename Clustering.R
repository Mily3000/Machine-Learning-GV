################################################################################
##---------------- INTRODUCCIÓN A DATA MINING & ML --------------------------###
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
auto <- read.csv("Clustering_R/auto-mpg.csv") # Leer un archivo csv
head(auto) # Revisar los primeros registros
colnames(auto) #Leer el nombre de las variables

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
auto <- rdacb.scale.many(auto, 2:7)
names(auto)
head(auto)

#Aplicamos el algoritmo k-means
set.seed(1020) # establecer una semilla aleatoria
colnames(auto)
fit <- kmeans(auto[, 10:15], 5) # Le pedimos 5 clusters iniciales
fit # El modelo no supervisado

# Visualizar los clusters en base a las variables usadas
pairs(auto[,2:7], col=c(1:5)[fit$cluster]) 

# Visualizar el traslape de los clusters en base a dos componentes principales
library(cluster)
clusplot(auto[,10:15], fit$cluster, color = TRUE, shade = TRUE, labels=0, lines=0)

# Función que permite seleccionar el número de clusters óptimo
rdacb.kmeans.plot <- function (data, num_clust = 15, seed = 9876) 
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
rdacb.kmeans.plot(auto[,10:15])


# Analizamos las variables usadas por cada cluster vs. general
cluster_autos<-fit$cluster #Renombramos el cluster
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

################################################################################
# Performing Cluster analysis using hierical clustering
################################################################################

# Lectura data en formato csv
auto <- read.csv("Clustering_R/auto-mpg.csv")

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

fit <- hclust(dis, method = "ward.D")

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



