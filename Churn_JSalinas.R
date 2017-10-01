################################
#Tomado de:                     #
#   ÁRBOLES DE CLASIFICACIÓN   #
#        EJEMPLO CHURN         #
#   MG. Jesús Salinas Flores   #
#   jsalinas@lamolina.edu.pe   #
#                              #
################################


###############################
# 1. Instalación de librerías #
###############################

# Instalación de Paquetes
install.packages(c("foreing","gmodels","rpart","rpart.plot","RColorBrewer",
                   "partykit","rattle","C50","party","adabag", "ROCR", "pROC", "gplots",
                   "foreach","randomForest","ISLR","tree"),
                   dependencies = c("Depends", "Suggests"))

###############################
# 2. Descripción de los datos #
###############################

# Para predecir clientes propensos a desafilarse (también llamado churn o attrition) 
# en una empresa de telecomunicaciones, podría usarse el algoritmo de 
# árbol de decision (decision tree) como clasificador.
#
# Se cuenta con una data de 1345 clientes de una empresa de telecomunicaciones
# donde algunos siguen siendo clientes (ACtual) y otros han fugado 
# de manera voluntaria.
#
# Variable Dependiente:  
#           CHURN   (0=Cliente Actual, 1=Fuga Voluntaria)
#
# Variables Independentes:
#           EDAD    (Edad del cliente en años)  
#           SEXO    (Sexo del cliente, 1=Fememino 2=Masculino)
#           CIVIL   (Estado civil del cliente, 1=Casado 2=Soltero) 
#           HIJOS   (Número de hijos del cliente)
#           INGRESO (Ingresos anuales del cliente)
#           AUTO    (Si es Cliente es dueño de un auto, 1=Si 2=No)  
#
# Variable de identificación: 
#           ID      (Código del cliente)

#######################
# 3. Lectura de datos #
#######################
getwd()
setwd("D:/Cursos/18.PEA_ML/0.Data")
library(foreign)
datos<-read.spss("DT_R/Churn-arboles.sav",use.value.labels=TRUE, max.value.labels=TRUE,to.data.frame=TRUE)
str(datos)
head(datos)

# No considerar la variable de identificación ID
datos<-datos[,-1]
str(datos)

# Declarar Variables categóricas como un factor
datos[,2] <- as.factor(datos[,2])
datos[,3] <- as.factor(datos[,3])
datos[,6] <- as.factor(datos[,6])
datos[,7] <- as.factor(datos[,7])
str(datos)

# Etiquetando las opciones de las variables categóricas
levels(datos$SEXO)  <- c("Fem","Masc")
levels(datos$CIVIL) <- c("Casado","Soltero")
levels(datos$AUTO)  <- c("Si","No")
levels(datos$CHURN) <- c("Actual","Fuga")
str(datos)

#########################################
# 4. Análisis Exploratorio de los datos #
#########################################

#----------------------------------------
# Análisis descriptivo univariado

summary(datos)
boxplot(datos$EDAD,    ylab="Edad",            col="blue")
boxplot(datos$HIJOS,   ylab="Número de hijos", col="green")
boxplot(datos$INGRESO, ylab="Ingreso anual",   col="yellow")

#------------------------------------------------------------
# Análisis descriptivo bivariado de las variables categóricas

library(gmodels)
CrossTable(datos$SEXO,datos$CHURN,prop.t=FALSE,prop.r=TRUE,prop.c=FALSE,prop.chisq=FALSE)
CrossTable(datos$CIVIL,datos$CHURN,prop.t=FALSE,prop.r=TRUE,prop.c=FALSE,prop.chisq=FALSE)
CrossTable(datos$AUTO,datos$CHURN,prop.t=FALSE,prop.r=TRUE,prop.c=FALSE,prop.chisq=FALSE)

#------------------------------------------------------------------------
#  Visualización de una Tabla de Contingencia usando una Matriz Gráfica  


Tabla1=table(datos$SEXO,datos$CHURN)
Tabla2=table(datos$CIVIL,datos$CHURN)
Tabla3=table(datos$AUTO,datos$CHURN)

library(gplots)

balloonplot(t(Tabla1), main ="Tabla de Contingencia",xlab ="Cliente", ylab="Sexo",label = FALSE, show.margins = FALSE)
balloonplot(t(Tabla2), main ="Tabla de Contingencia",xlab ="Cliente", ylab="Estado Civil",label = FALSE, show.margins = FALSE)
balloonplot(t(Tabla3), main ="Tabla de Contingencia",xlab ="Cliente", ylab="Tenencia de Auto",label = FALSE, show.margins = FALSE)



#--------------------------------------------------------------
# Análisis descriptivo bivariado de las variables cuantitativas

tapply(datos$EDAD,datos$CHURN,mean) 
tapply(datos$HIJOS,datos$CHURN,mean) 
tapply(datos$INGRESO,datos$CHURN,mean) 



#############################################
#  5. Arboles por Inferencia Condicional    # 
#############################################

library(party)
arbolch<-ctree(CHURN ~ . ,data=datos,controls = ctree_control(minsplit=50,minbucket=25))

plot(arbolch)
arbolch

plot(arbolch,type="simple")   # y= (0=Cliente, 1=Fuga)
plot(arbolch,type="extended")


#-----------------------------
# Predicción usando el árbol 

# Calcular los valores predichos
PRED <-predict(arbolch,datos[,c(1:6)],type="response")
PRED


# Calculando las probabilidades 
PROBA.CHAID <- predict(arbolch, datos, type = "prob")
head(PROBA.CHAID)

PROBA.CHAID <- data.frame(matrix(unlist(PROBA.CHAID), nrow=1345, byrow=T))
head(PROBA.CHAID)

PROBA.CHAID=PROBA.CHAID[,2]
head(PROBA.CHAID)

# Junta el archivo de datos con la columna de predicción y de probabilidad
datoschaid=cbind(datos,PRED,PROBA.CHAID)
datoschaid
str(datoschaid)
write.csv(datoschaid,"Churn con valor y probabilidad predecida de FUGA-CHAID.csv")


#---------------------------------------------
# Calcular el error de mala clasificación
error=mean(PRED!=datos$CHURN)
error

#---------------------------------------------
# Calcular la matriz de confusión
table(datos$CHURN,PRED)

library(gmodels)
CrossTable(datos$CHURN,PRED,prop.t=FALSE,prop.r=TRUE,prop.c=FALSE,prop.chisq=FALSE)


####################################################
# 6. Árbol de Clasificación con el algoritmo RPART #
####################################################

# Cargar la libreria rpart
library(rpart)

#--------------------------------------------------
# Ejemplo 1: Árbol con los parámetros por defecto
head(datos)
# Estimar el arbol
arbol1=rpart(CHURN ~ . , data=datos, method="class")
str(arbol1)
arbol1
arbol1$variable.importance


# Graficando el arbol
plot(arbol1,margin=.25)
text(arbol1,use.n=T,digits=3)

library(rpart.plot)
rpart.plot(arbol1, type=0, extra=101,cex = .7, nn=TRUE)
rpart.plot(arbol1, type=1, extra=101,cex = .7, nn=TRUE)
rpart.plot(arbol1, type=2, extra=101,cex = .7, nn=TRUE)
rpart.plot(arbol1, type=3, extra=101,cex = .7, nn=TRUE)
rpart.plot(arbol1, type=4, extra=101,cex = .7, nn=TRUE)

prp(arbol1, faclen = 0, type=0, cex = 0.8, extra = 1)
prp(arbol1, faclen = 0, type=0, cex = 0.8, extra = 4)
prp(arbol1, faclen = 0, type=2, cex = 0.8, extra = 1)

# Mejorando los Gráficos
library(partykit)
plot(as.party(arbol1), tp_args = list(id = FALSE))

# Resumen del árbol 
summary(arbol1)


#-----------------------------------------
# Ejemplo 2: Arbol controlando parametros

# Parámetros 
# minsplit:   Indica el número mínimo de observaciones en un nodo para que este sea dividido. 
#             Mínimo para que un nodo sea padre. Esta opción por defecto es 20.
# minbucket:  Indica el número mínimo de observaciones en cualquier nodo terminal. 
#             Por defecto esta opción es el valor redondeado de minsplit/3.
# cp:         Parámetro de complejidad. Indica que si el criterio de impureza no es 
#             reducido en mas de cp*100% entonces se para.
#             Por defecto cp=.01. Es decir, la reducción en la impureza del
#             nodo terminal debe ser de al menos 1% de la impureza inicial.
# maxdepth:   condiciona la profundidad máxima del arbol. 
#             Por defecto está establecida como 30.


arbol1$control
# rpart.control(minsplit = 20, minbucket = round(minsplit/3), cp = 0.01, 
# maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, xval = 10,
# surrogatestyle = 0, maxdepth = 30, ...)

arbol2=rpart(CHURN ~ . , data=datos,control=rpart.control(minsplit=90, minbucket=30),method="class")
rpart.plot(arbol2, type=2, extra=101,cex = .7, nn=TRUE)


printcp(arbol2)	   # Muestra la tabla cp
plotcp(arbol2)	   # Resultados del ploteo de validación cruzada  
print(arbol2)	     # Imprime resultados 
summary(arbol2)	   # Resultados detallados incluyendo sustitutos (surrogate)


#-----------------------------------------------------
# Ejemplo 3: Controlando el crecimiento del árbol
# con el parámetro de complejidad (cp=0.05)

arbol3=rpart(CHURN ~ . , data=datos,control=rpart.control(minsplit=90, minbucket=30,cp=0.05),method="class")
rpart.plot(arbol3, type=2, extra=101,cex = .7, nn=TRUE)

printcp(arbol3)

#-----------------------------------------------------------------
# Ejemplo 4: cp=0.001 para obtener un árbol con más ramas

arbol4=rpart(CHURN ~ . ,data=datos, method="class",cp=0.001)
rpart.plot(arbol4, type=2, extra=101,cex = .7, nn=TRUE)

printcp(arbol4)

#-------------------------------------------------------
# Ejemplo 5: Controlando el crecimiento del árbol
#           por número máximo de niveles (maxdepth=3)

arbol5=rpart(CHURN~. ,data=datos,method="class",maxdepth=3)
rpart.plot(arbol5, type=2, extra=101,cex = .7, nn=TRUE)

printcp(arbol5)

#------------------------------------
# Ejemplo 6: Recortar el árbol

arbol6=prune(arbol4,cp=0.1)
rpart.plot(arbol6, type=2, extra=101,cex = .7, nn=TRUE)
printcp(arbol6)

arbol7=prune(arbol4,cp=0.01)
rpart.plot(arbol7, type=2, extra=101,cex = .7, nn=TRUE)
printcp(arbol5)

#----------------------------------------------
# Valor óptimo de CP
arbol.completo <- rpart(CHURN ~ . ,data=datos,method="class",cp=0, minbucket=0)
arbol.completo
printcp(arbol.completo)

rpart.plot(arbol.completo, type=2, extra=101,cex = .7, nn=TRUE)

xerr <- arbol.completo$cptable[,"xerror"]
xerr

minxerr <- which.min(xerr)
minxerr

mincp <- arbol.completo$cptable[minxerr, "CP"]
mincp

arbol.pruned <- prune(arbol.completo,cp=mincp)
arbol.pruned
printcp(arbol.pruned)

rpart.plot(arbol.pruned, type=2, extra=101,cex = .7, nn=TRUE)


#------------------------------------
# Predicción usando el árbol Podado

# Calcular los valores predichos
PRED <-predict(arbol.pruned,datos[,c(1:6)],type="class")
PRED


# Calculando las probabilidades 
PROBA.CART <- predict(arbol.pruned, datos, type = "prob")
str(PROBA.CART)
head(PROBA.CART)

PROBA.CART=PROBA.CART[,2]
head(PROBA.CART)

# Junta el archivo de datos con la columna de predicción y de probabilidad
datoscart=cbind(datos,PRED,PROBA.CART)
datoscart
str(datoscart)
write.csv(datoscart,"Churn con valor y probabilidad predecida de FUGA-CART.csv")

