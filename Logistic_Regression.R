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


#########################################
# Regresión Logística #
#########################################

fit <- glm(datos$CHURN ~ ., data = datos, 
           family=binomial(link="logit"))

summary(fit)

colnames(datos)
datos2<-datos[,-3]

fit_2 <- glm(datos2$CHURN ~ . , 
             data = datos2, family=binomial(link="logit"))
summary(fit_2)

colnames(datos2)
datos3<-datos2[,-5]

fit_3 <- glm(datos3$CHURN ~ . , data = datos3, family=binomial(link="logit"))
summary(fit_3)


colnames(datos3)
datos4<-datos3[,-3]

fit_4 <- glm(datos4$CHURN ~ . , data = datos4, 
             family=binomial(link="logit"))
summary(fit_4)

#############################################
#  Testeo del modelo    # 
#############################################

churn_model1<-predict(fit,datos,type="response")
churn_model2<-predict(fit_2,datos,type="response")
churn_model3<-predict(fit_3,datos,type="response")
churn_model4<-predict(fit_4,datos,type="response")

quantile(churn_model)

class_churn1<-churn_model1 >=.38
class_churn2<-churn_model2 >=.38
class_churn3<-churn_model3 >=.38
class_churn4<-churn_model4 >=.38

datos_score <- cbind(datos,churn_model1,churn_model2,
                     churn_model3,churn_model4,
                     class_churn1,class_churn2,
                     class_churn3,class_churn4)
head(datos_score)

table(datos_score$CHURN,datos_score$class_churn4)

table(datos_score$CHURN)
513/(513+832)

write.csv(datos_score,"datos_score_RL.csv")

