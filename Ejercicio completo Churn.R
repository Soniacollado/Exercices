# Alumno: Sonia Collado


#Instalación de paquetes


install.packages("readr")
library(readr)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("GGally")
library(GGally)
install.packages("modeest")
library(modeest)
install.packages("psych")
library(psych)
install.packages("gapminder")
library(gapminder)
install.packages("binr")
library(binr)
install.packages("dummies")
library(dummies)
install.packages("ROCR")
library(ROCR)
install.packages("randomForest")
library(randomForest)
install.packages("tidyverse")
library(tidyverse)
install.packages('purrr')
library(purrr)
install.packages("smbinning")
library(smbinning)
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)



#Carga de datos
ejercicio<-read.table("~/EAE/Data Visualization/ivan/churn_examen.csv",sep=";",dec=".",header=TRUE)
ejercicio



#Cargados los datos, optamos por visualizar en una tabla ordenada los datos con los que trabajamos.
View(ejercicio)



#Lo primero que tenemos que hacer es ver con qué tipo de datos vamos a trabajar. Para ello utilizamos la función str
#Observamos que tenemos datos cuantitativos y cualitativos

str(ejercicio)
glimpse(ejercicio) #otra forma de visualizar las variables a través de la libreria tidyverse



# Summary. A través de es función obtenemos medidas como cuantiles, medianas, medias, que nos ayudan a tener una visión sobre qué
#datos estan las variables. Estos datos, nos pueden ayudar a tomar decisiones a la hora de comparar datos finales con los generales.

summary(ejercicio)



#observamos que hay na y los loccalizamos 

install.packages("mice")
library(mice)
md.pattern(ejercicio)

install.packages("VIM")
library(VIM)

aggr(ejercicio, 
     col=c('green','red'),
     numbers=TRUE)


#como los valores omitidos están en la variable age, los sustituimos por la media

ejercicio$age[is.na(ejercicio$age)]<-round(mean(ejercicio$age,na.rm=TRUE),0)

#comprobación
summary(ejercicio) 
aggr(ejercicio,
     col=c('green','red'),
     numbers=TRUE)


#comenzamos a realizar visualizar el tipo de variables que tenemos:

#Univariante discretas
#Empezamos con la variable state donde lo agruparemos, contrastaremos frente al Churn y lo ponderaremos
datosstate=table(ejercicio$state, ejercicio$churn)
datosstate
churn_state=datosstate[,2]/(datosstate[,1]+datosstate[,2])
churns_state_account = cbind(churn_state,table(ejercicio$state))
churns_state_account   #ordenar por arrange/group_by pero da error.

### Observamos que los estados con un mayor churn son: California, Carolina del Sur e Iowa 



#Realizamos un histograma para ver la distribución del resto de variables numericas y estudiar su distribución

ejercicio$age <- as.numeric(ejercicio$age)
ejercicio$annualincome <- as.numeric(ejercicio$annualincome)
numeric_churn<-ejercicio[,c(2,3,11,12,13,14,16,18,19,23,24)]
str(ejercicio)
cor(numeric_churn)

datoschurn_hist = ejercicio[,c(2,3,11,12,13,14,16,18,19,23,24)]
multi.hist(x = datoschurn_hist, 
           density = TRUE, 
           freq = FALSE, 
           dcol = c("green", "blue"), 
           dlty = c("dashed", "solid"), 
           main = "")



#Valorando la gráfica trabajaremos a través del multihistograma anterior trabajaremos con distribución por distancias
#las variables 12,13,19 y el resto (2,3,11,14,16,18,23 y 24) por frecuencias


####Distancia

#12.number of complaints
ejercicio$numberofcomplaints <-   cut(ejercicio$numberofcomplaints,seq(min(ejercicio$numberofcomplaints), max(ejercicio$numberofcomplaints), by = 1))
ejercicio$numberofcomplaints
numquejas<-ejercicio$numberofcomplaints

datoscomplaints=table(ejercicio$numberofcomplaints,ejercicio$churn)
datoscomplaints


churncomplaints = datoscomplaints[,2]/(datoscomplaints[,1]+datoscomplaints[,2])
churncomplaints
churncomplaintscount = cbind(churncomplaints,table(ejercicio$numberofcomplaints))
churncomplaints
barplot(churncomplaints)
barplot(datoscomplaints)
#Observamos que segun las frecuencias del numero de quejas, tienen una mayor tasa de bajas las que 
#tienen de 3 quejas que el resto. Frente a los que tienen 1 o 2, la proporción es semejante.


#13. number of months unpaid
ejercicio$numberofmonthunpaid <-   cut(ejercicio$numberofmonthunpaid,seq(min(ejercicio$numberofmonthunpaid), max(ejercicio$numberofmonthunpaid), by = 1))
ejercicio$numberofmonthunpaid
mesesimpago<-ejercicio$numberofmonthunpaid

datosimpago=table(ejercicio$numberofmonthunpaid,ejercicio$churn)
datosimpago

view(ejercicio)

churnimpago = datosimpago[,2]/(datosimpago[,1]+datosimpago[,2])
churnimpago
churnimpagocount = cbind(churnimpago,table(ejercicio$numberofmonthunpaid))
churnimpagocount
barplot(datosimpago)
barplot(churnimpago)
#Observamos que en esta variable no encontramos gran diferencias en las frecuencias.
#Podemos adelantarnos a que esta variable no es realmente relevante para el modelo.


#19.Unpaid balance

ejercicio$unpaidbalance <-   cut(ejercicio$unpaidbalance,seq(min(ejercicio$unpaidbalance), max(ejercicio$unpaidbalance), by = 20))
ejercicio$unpaidbalance
impago_balance<-ejercicio$unpaidbalance

impago_balance=table(ejercicio$unpaidbalance,ejercicio$churn)
impago_balance


churnimpago_balance = impago_balance[,2]/(impago_balance[,1]+impago_balance[,2])
churnimpago_balance
churnimpagobalancecount = cbind(churnimpago_balance,table(ejercicio$unpaidbalance))
churnimpagobalancecount
barplot(impago_balance)
barplot(churnimpago_balance)
#De lo anterior observamos que existe una mayor probabilidad de baja en aquellos que se encuentren
#entre el rango de (200-220] seguido del rango (220-240]



#### FRECUENCIAS

cutsage <- bins(ejercicio$age, target.bins = 10, minpts = 100)
#lo que hacemos es crear intervalos

#division por frecuencia de la variable edad

cutsage$breaks <- bins.getvals(cutsage)
cutsage$binct

ejercicio$ageBin <-   cut(ejercicio$age,cutsage$breaks)
ejercicio$ageBin
#son datos que tenemos que analizar como intervalos

datosage=table(ejercicio$ageBin,ejercicio$churn)
datosage

churnsage = datosage[,2]/(datosage[,1]+datosage[,2])
churnsage
churnagecount = cbind(churnsage,table(ejercicio$ageBin))
churnagecount


prior<-sum(ejercicio$churn==1)/length(ejercicio$churn)

churnageplot = cbind(churnsage,table(ejercicio$ageBin)/length(ejercicio$churn))
churnageplot
plot=barplot(100*t(churnageplot),ylim=c(0,50),col=c('red','blue'),cex.names=0.6,main='Churn por edades', beside=TRUE)


abline(h=100*prior,col="green")
hist(churnageplot)
##Observamos como la mayor probabilidad la encontramos entre las edades de 25 y 38 años donde se dan de baja
## los que menos, a partir de 52 años.

view(ejercicio)



#### CUANTIA DE LA FACTURA MENSUAL

cutsbill <- bins(ejercicio$monthlybilledamount, target.bins = 10, minpts = 100)

cutsbill$breaks <- bins.getvals(cutsbill)
cutsbill$binct

ejercicio$monthlybilledamount <-   cut(ejercicio$monthlybilledamount,cutsbill$breaks)
ejercicio$monthlybilledamount

datosbill=table(ejercicio$monthlybilledamount,ejercicio$churn)
datosbill

churnbill = datosbill[,2]/(datosbill[,1]+datosbill[,2])
churnbill
churbillcount = cbind(churnbill,table(ejercicio$monthlybilledamount))
churbillcount


prior<-sum(ejercicio$churn==1)/length(ejercicio$churn)

churnbillplot = cbind(churnbill,table(ejercicio$monthlybilledamount)/length(ejercicio$churn))
hist(churnbillplot)
plot=barplot(100*t(churnbillplot),ylim=c(0,20),col=c('red','blue'),cex.names=0.6,main='Churn por fra. mensual', beside=TRUE)

abline(h=100*prior,col="green")
# Apenas hay diferencia, entendemos que no será influyente esta variable.

view(ejercicio)

#### NUMERO DIAS DE CONTRATO

cutsnumofdays <- bins(ejercicio$numdayscontractequipmentplanexpiring, target.bins = 10, minpts = 100)

cutsnumofdays$breaks <- bins.getvals(cutsnumofdays)
cutsnumofdays$binct

ejercicio$numdayscontractequipmentplanexpiring <-   cut(ejercicio$numdayscontractequipmentplanexpiring,cutsnumofdays$breaks)
ejercicio$numdayscontractequipmentplanexpiring

datosnumofdays=table(ejercicio$numdayscontractequipmentplanexpiring,ejercicio$churn)
datosnumofdays

churnnumofdays = datosnumofdays[,2]/(datosnumofdays[,1]+datosnumofdays[,2])
hist(churnnumofdays)
churnumofdayscount = cbind(churnnumofdays,table(ejercicio$numdayscontractequipmentplanexpiring))
churnumofdayscount


prior<-sum(ejercicio$churn==1)/length(ejercicio$churn)

churnnumofdaysplot = cbind(churnnumofdays,table(ejercicio$numdayscontractequipmentplanexpiring)/length(ejercicio$churn))
churnnumofdaysplot
plot=barplot(100*t(churnnumofdaysplot),ylim=c(0,20),col=c('red','blue'),cex.names=0.6,main='Churn por num days contract', beside=TRUE)

abline(h=100*prior,col="green")
# No encontramos algun dato que verdaderamente destaque exceptuando las del rango (49.5,60.5) y más de 89,5



#### PENALTY

cutspenalty <- bins(ejercicio$penaltytoswitch, target.bins = 50, minpts = 100)

cutspenalty$breaks <- bins.getvals(cutspenalty)
cutspenalty$binct

ejercicio$penaltytoswitch <-   cut(ejercicio$penaltytoswitch,cutspenalty$breaks)
ejercicio$penaltytoswitch

datospenalty=table(ejercicio$penaltytoswitch,ejercicio$churn)
datospenalty

churnpenalty = datospenalty[,2]/(datospenalty[,1]+datospenalty[,2])
churnpenalty
churnpenaltycount = cbind(churnpenalty,table(ejercicio$upenaltytoswitch))
churnpenaltycount


prior<-sum(ejercicio$churn==1)/length(ejercicio$churn)

churnpenaltyplot = cbind(churnpenalty,table(ejercicio$penaltytoswitch)/length(ejercicio$churn))
hist(churnpenaltyplot)
plot=barplot(100*t(churnpenaltyplot),ylim=c(0,20),col=c('red','blue'),cex.names=0.6,main='Churn penalty', beside=TRUE)

abline(h=100*prior,col="green")

#las bajas se mantienen constantes.


#### TOTAL MINUTES  

cutsminutes <- bins(ejercicio$totalminsusedinlastmonth, target.bins = 50, minpts = 100)

cutsminutes$breaks <- bins.getvals(cutsminutes)
cutsminutes$binct

ejercicio$totalminsusedinlastmonth <-   cut(ejercicio$totalminsusedinlastmonth,cutsminutes$breaks)
ejercicio$totalminsusedinlastmonth

datosminutes=table(ejercicio$totalminsusedinlastmonth,ejercicio$churn)
datosminutes

churnminutes = datosminutes[,2]/(datosminutes[,1]+datosminutes[,2])
churnminutes
churnminutes = cbind(churnminutes,table(ejercicio$totalminsusedinlastmonth))
churnminutes


prior<-sum(ejercicio$churn==1)/length(ejercicio$churn)

churnminutes = cbind(churnminutes,table(ejercicio$totalminsusedinlastmonth)/length(ejercicio$churn))
churnminutes
plot=barplot(10*t(churnminutes),ylim=c(0,5000),col=c('red','blue'),cex.names=0.6,main='Churn Total minutes last month', beside=TRUE)

abline(h=100*prior,col="green")

#Aunque existe un valor en el que si existe diferencia entre el numero de bajas y no bajas en el intervalo (102-112]
#No se ven diferencias muy significativas



#### TOTAL CALL

cutscall <- bins(ejercicio$totalcallduration , target.bins = 100, minpts = 100)

cutscall$breaks <- bins.getvals(cutscall)
cutscall$binct

ejercicio$totalcallduration <-   cut(ejercicio$totalcallduration,cutscall$breaks)
ejercicio$totalcallduration

datoscall=table(ejercicio$totalcallduration,ejercicio$churn)
datoscall

churncall = datoscall[,2]/(datoscall[,1]+datoscall[,2])
churncall
churncall = cbind(churncall,table(ejercicio$totalcallduration))
churncall


prior<-sum(ejercicio$churn==1)/length(ejercicio$churn)

churncall = cbind(churncall,table(ejercicio$totalcallduration)/length(ejercicio$churn))
churncall
plot=barplot(10*t(churncall),ylim=c(0,3000),col=c('red','blue'),cex.names=0.6,main='Churn Total call duration', beside=TRUE)

abline(h=100*prior,col="green")
#los valores son parecidos entre todos los tramos


#### ANNUAL INCOME

annual_income <- bins(ejercicio$annualincome , target.bins = 100, minpts = 100)

annual_income$breaks <- bins.getvals(annual_income)
annual_income$binct

ejercicio$annualincome <-   cut(ejercicio$annualincome,annual_income$breaks)
ejercicio$annualincome

datosann_income=table(ejercicio$annualincome,ejercicio$churn)
datosann_income

churnannual_inco = datosann_income[,2]/(datosann_income[,1]+datosann_income[,2])
churnannual_inco
churnannual_inco = cbind(churnannual_inco,table(ejercicio$annualincome))
churnannual_inco


prior<-sum(ejercicio$churn==1)/length(ejercicio$churn)
churnannual_inco = cbind(churnannual_inco,table(ejercicio$annualincome)/length(ejercicio$churn))
churnannual_inco
plot=barplot(10*t(churnannual_inco),ylim=c(0,3000),col=c('red','blue'),cex.names=0.6,main='Churn Anual Income', beside=TRUE)

abline(h=100*prior,col="green")

#se observan datos muy similares en toda la representaicón


#### AVG CALL DURATION

cutsavg <- bins(ejercicio$avgcallduration , target.bins = 100, minpts = 100)

cutsavg$breaks <- bins.getvals(cutsavg)
cutsavg$binct

ejercicio$avgcallduration <-   cut(ejercicio$avgcallduration,cutsavg$breaks)
ejercicio$avgcallduration

datosavg=table(ejercicio$avgcallduration,ejercicio$churn)
datosavg

churnavg = datosavg[,2]/(datosavg[,1]+datosavg[,2])
churnavg
churnavg = cbind(churnavg,table(ejercicio$avgcallduration))
churnavg


prior<-sum(ejercicio$churn==1)/length(ejercicio$churn)

churnavg = cbind(churnavg,table(ejercicio$avgcallduration)/length(ejercicio$churn))
churnavg
plot=barplot(10*t(churnavg  ),ylim=c(0,3000),col=c('red','blue'),cex.names=0.6,main='Churn AVG Call Duration', beside=TRUE)

abline(h=100*prior,col="green")

#Observamos que en este gráfico si encontramos diferencias por lo que puede que sea una variable relevantes.


view(ejercicio)





# Ahora trabajaremos con aquellas variables cualitativas y las enfrentaremos contra el churn para sacar las primeras conclusiones


####CUSTOMER SUSPENDED

ejercicio$customersuspended <- as.numeric(ejercicio$customersuspended)
customer <- table( ejercicio$customersuspended, ejercicio$churn )
cutomer_proporc <-prop.table(customer)*100
cutomer_proporc
barplot(cutomer_proporc)    
# se observa que aquellos que si han suspendio tienen una mayor baja frente a los que no
view(ejercicio)


#### EDUCATION 

ejercicio$education <- as.numeric(ejercicio$education)
education <- table( ejercicio$education, ejercicio$churn )
education_proporc <-prop.table(education)*100
education_proporc
barplot (education_proporc)
#Se observa que dentro de los que se han dado de baja encontramos en mayor medida los que tienen una educación
#de High School or below seguido de Bachelor or equivalent. Por ello deducimos las camapañas deberán ir dirigidas
#a personas con estudios más bajos pues tienen mayor probabilidad para darse de baja



#### GENDER

ejercicio$gender <- as.numeric(ejercicio$gender)
gender <- table( ejercicio$gender, ejercicio$churn )
gender_proporc <-prop.table(gender)*100
gender_proporc
barplot (gender_proporc)
# en este caso observamos que aunque la diferencia es mínima y, que la distribución de clientes es más o menos
#mixta, las mujeres se dan más de baja que los hombres, por lo que es el sector que en todo caso habría que tratar


#### HOMEOWNER

ejercicio$homeowner <- as.numeric(ejercicio$homeowner)
homeowner <- table( ejercicio$homeowner, ejercicio$churn )
homeowner_proporc <-prop.table(homeowner)*100
homeowner_proporc
barplot (homeowner_proporc)
#comprobamos que hay una mayor posibilidad de que se den de baja los que son propietarios del hogar frente a los que no


#### MARITAL STATUS
ejercicio$maritalstatus <- as.numeric(ejercicio$maritalstatus)
maritalstatus <- table( ejercicio$maritalstatus, ejercicio$churn )
maritalstatus_proporc <-prop.table(maritalstatus)*100
maritalstatus_proporc
barplot (maritalstatus_proporc)
#Al igual que pasa con el genero, apenas existen diferencias entre casado y  soltero, donde existe una
#muy pequeña variación que se decanta por los solteros



#### OCCUPATION
ejercicio$occupation <- as.numeric(ejercicio$occupation)
occupation <- table( ejercicio$occupation, ejercicio$churn )
occupation_proporc <-prop.table(occupation)*100
occupation_proporc
barplot (occupation_proporc)
#la mayor tasa de baja la encontramos en ocupación Others pero no hay mucha diferencia frente al resto



#### USES INTERNET SERVICES
ejercicio$usesinternetservice <- as.numeric(ejercicio$usesinternetservice)
usesinternet <- table( ejercicio$usesinternetservice, ejercicio$churn )
usesinternet_proporc <-prop.table(usesinternet)*100
usesinternet_proporc
barplot (usesinternet_proporc)
#encontramos que hay diferencia entre ambos usos donde del porcentaje que se han dado de baja, 8 puntos por encima
#no usaban el servicio de internet


#### USES VOICE SERVICES
ejercicio$usesvoiceservice <- as.numeric(ejercicio$usesvoiceservice)
voicesserv<- table( ejercicio$usesvoiceservice, ejercicio$churn )
voicesserv_proporc <-prop.table(voicesserv)*100
voicesserv_proporc
barplot (voicesserv_proporc)
# coincide el uso del servicio de voz con internet frente a los datos comparados con el churn.

View(ejercicio)
str(ejercicio)



############ WOE Weight of Evidence.  Mide la diferencia entre la proporción de buenos y malos en cada grupo.


install.packages("woe");
library(woe);

ejercicio$age <- as.numeric(ejercicio$age)
ejercicio$monthlybilledamount <- as.numeric(ejercicio$monthlybilledamount)
ejercicio$numberofcomplaints <- as.numeric(ejercicio$numberofcomplaints)
ejercicio$numberofmonthunpaid <- as.numeric(ejercicio$numberofmonthunpaid)
ejercicio$numdayscontractequipmentplanexpiring <- as.numeric(ejercicio$numdayscontractequipmentplanexpiring)
ejercicio$education <- as.numeric(ejercicio$education)
ejercicio$penaltytoswitch <- as.numeric(ejercicio$penaltytoswitch)
ejercicio$totalcallduration <- as.numeric(ejercicio$totalcallduration)
ejercicio$totalminsusedinlastmonth <- as.numeric(ejercicio$totalminsusedinlastmonth)
ejercicio$unpaidbalance <- as.numeric(ejercicio$unpaidbalance)
ejercicio$avgcallduration <- as.numeric(ejercicio$avgcallduration)
ejercicio$annualincome <- as.numeric(ejercicio$annualincome)
ejercicio$calldroprate <- as.numeric(ejercicio$calldroprate)
ejercicio$callfailurerate <- as.numeric(ejercicio$callfailurerate)
ejercicio$state <- as.numeric(ejercicio$state)
ejercicio$percentagecalloutsidenetwork <- as.numeric(ejercicio$percentagecalloutsidenetwork)
ejercicio$ageBin <- as.numeric(ejercicio  $ageBin)


str(ejercicio)
view(ejercicio)

woe(ejercicio,"education",TRUE,"churn",5,Bad=0,Good=1);
woe(ejercicio,"age",TRUE,"churn",5,Bad=0,Good=1);
woe(ejercicio,"customersuspended",TRUE,"churn",5,Bad=0,Good=1);
woe(ejercicio,"gender",TRUE,"churn",5,Bad=0,Good=1);
woe(ejercicio,"homeowner",TRUE,"churn",5,Bad=0,Good=1);
woe(ejercicio,"maritalstatus",TRUE,"churn",5,Bad=0,Good=1);
woe(ejercicio,"occupation",TRUE,"churn",5,Bad=0,Good=1);
woe(ejercicio,"usesinternetservice",TRUE,"churn",5,Bad=0,Good=1);
woe(ejercicio,"usesvoiceservice",TRUE,"churn",5,Bad=0,Good=1);
woe(ejercicio,"ageBin",TRUE,"churn",5,Bad=0,Good=1);

woe(ejercicio,"percentagecalloutsidenetwork",TRUE,"churn",5,Bad=0,Good=1);
woe(ejercicio,"callfailurerate",TRUE,"churn",10,Bad=0,Good=1);
woe(ejercicio,"calldroprate",TRUE,"churn",10,Bad=0,Good=1);
woe(ejercicio,"annualincome",TRUE,"churn",10,Bad=0,Good=1);
woe(ejercicio,"avgcallduration",TRUE,"churn",10,Bad=0,Good=1);
woe(ejercicio,"unpaidbalance",TRUE,"churn",10,Bad=0,Good=1);
woe(ejercicio,"totalminsusedinlastmonth",TRUE,"churn",10,Bad=0,Good=1);
woe(ejercicio,"totalcallduration",TRUE,"churn",10,Bad=0,Good=1);
woe(ejercicio,"penaltytoswitch",TRUE,"churn",10,Bad=0,Good=1);
woe(ejercicio,"numdayscontractequipmentplanexpiring",TRUE,"churn",10,Bad=0,Good=1);

ejercicio$numberofmonthunpaid[is.na(ejercicio$numberofmonthunpaid)]<-round(mean(ejercicio$numberofmonthunpaid,na.rm=TRUE),0)
woe(ejercicio,"numberofmonthunpaid",TRUE,"churn",10,Bad=0,Good=1);

ejercicio$numberofcomplaints[is.na(ejercicio$numberofcomplaints)]<-round(mean(ejercicio$numberofcomplaints,na.rm=TRUE),0)
woe(ejercicio,"numberofcomplaints",TRUE,"churn",10,Bad=0,Good=1);

woe(ejercicio,"monthlybilledamount",TRUE,"churn",10,Bad=0,Good=1);




install.packages("woeBinning")
library(woeBinning)


# Bin a single numeric variable

binning <- woe.binning(ejercicio,'churn','age',
                       min.perc.total=0.05, min.perc.class=0.01,
                       stop.limit=0.1, event.class= 1
)

woe.binning.plot(binning)
datoschurnsinphon =ejercicio[,-25]
datoschurnsinphone =ejercicio[,-24]

binning <- woe.binning(datoschurnsinphone,'churn',datoschurnsinphone)
par(mar = rep(2, 4))

woe.binning.plot(binning)

#### de lo anterior, de donde sacamosel Information Value observamos las 5 variables más importantes que afctan al churn.
# en este caso son: age, percentagecalloutsidenetwork, unpaidbalance, numberofcomplaints y state. 
#Pasado este primer filtro, observamos que del resto disminuyen pero están más próximas unas que otras.



install.packages("smbinning")
library(smbinning)
result1= smbinning(ejercicio  , 'churn', 'age', p = 0.05)
result2=smbinning.factor(ejercicio, 'churn', 'state', maxcat = 100)

smbinning.plot(result1,option="dist",sub="churn")
smbinning.plot(result1,option="badrate",sub="churn")
smbinning.plot(result1,option="woe",sub="churn")

smbinning.plot(result2,option="dist",sub="churn")
smbinning.plot(result2,option="badrate",sub="churn")
smbinning.plot(result2,option="woe",sub="churn")

#observamos que el mayor indice de churn se encuentra en aquellos que entre el intervalo (24,39]


install.packages('arules')
library(arules)
nbins <- nclass.scott(ejercicio$churn)
churn_arules <- discretize(ejercicio$churn, method = "interval", categories = nbins)
table(churn_arules)

#observamos entre qué parametros se encuentra nuestra variable churn. Situándose
#esta entre los valores [0,0.357) y [0.964,1]


#### COMENZAMOS CON EL MODELO (REGRESIÓN)

#TRAIN
smp_size <- floor(0.75 * nrow(ejercicio))
smp_size

#observamos que se escoge una muestra de 15355
#genera una semilla para asegurarnos de que siempre sean los mismos datos

set.seed(2018)
train_ind <- sample(seq_len(nrow(ejercicio)), size = smp_size)
train_ind
#i observamos la parte de consola vemos que del parametro (1:15355) ha cogido la mas 14343, 18896, 10704, 9050,...

train <- ejercicio[train_ind, ]
test <- ejercicio[-train_ind, ]
train <- ejercicio[1:15000,]
test <- ejercicio [15001:20474,]

view(ejercicio)
#Escogemos el modelo y añadimos las variables que nos ha dado el Information Value

modelo <- glm(churn ~ageBin+age+percentagecalloutsidenetwork+homeowner+numberofcomplaints+unpaidbalance, family = binomial(link='logit'),data=train);
coefficients(modelo)


#Comprobamos el modelo y observamos que variables son las que más afectan al modelo
summary(modelo)
#Observamos las variables que son más importantes para el modelo dentro de los datos tramificados.


modelo$coefficients



## R cuadrado de un modelo logit


library(pscl);
pR2(modelo);

library(gplots)

library(ROCR);

p<- predict(modelo, test, type="response");
pr <- prediction(p, test$churn);
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
lines(par()$usr[1:2],par()$usr[3:4])

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc


p <- predict(modelo, train, type="response");
pr <- prediction(p, train$churn);
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
lines(par()$usr[1:2],par()$usr[3:4])
#Se observan las probabilidades de que te de un falso positivo frente a un verdadero positivo 


auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

#se observa una pequeña mejoría del test frente al train.


######## probamos con más variables

ejercicio$nuevavariable<-ejercicio$state=='WA' | ejercicio$state=='UT';
table(ejercicio$nuevavariable,ejercicio$state)
nuevavariable <- class(ejercicio$nuevavariable)
class(ejercicio$nuevavariable)
ejercicio$nuevavariable[ejercicio$state=='WA' | ejercicio$state=='UT]<-1;']
table(ejercicio$nuevavariable,ejercicio$state)


modelo2 <- glm(churn ~ageBin+age+annualincome+percentagecalloutsidenetwork++unpaidbalance+numberofcomplaints+state+customersuspended+education+usesinternetservice+usesvoiceservice+totalminsusedinlastmonth+monthlybilledamount+occupation+callfailurerate, family = binomial(link='logit'),data=train);
coefficients(modelo2)


#Comprobamos el modelo y observamos que variables son las que más afectan al modelo
summary(modelo2)


modelo$coefficients

view(ejercicio)
summary(modelo2)


pR2(modelo2);

p<- predict(modelo2, test, type="response");
pr <- prediction(p, test$churn);
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
lines(par()$usr[1:2],par()$usr[3:4])

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

p <- predict(modelo2, train, type="response");
pr <- prediction(p, train$churn);
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
lines(par()$usr[1:2],par()$usr[3:4])

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc


#Observando que en nuestro segundo intento no hemos conseguido aumentar nuestro AUC, nos quedaremos con el primero
#observamos del modelo que, incluyo cogiendo el primer modelo donde el AUC es mayor, es un modelo con un ratio de falso positivo importante


#Por ello, decidimos trabajar con la curva ROC en el primer modelo con los datos del test 
# (datos recuperados de lineas más arriba)

library(ROCR);
p<- predict(modelo2, test, type="response");
pr <- prediction(p, test$churn);
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
lines(par()$usr[1:2],par()$usr[3:4])

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc


p <- predict(modelo2, train, type="response");
pr <- prediction(p, train$churn);
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, colorize="TRUE")
lines(par()$usr[1:2],par()$usr[3:4])
#Se observan las probabilidades de que te de un falso positivo frente a un verdadero positivo 

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc


#procedemos a realizar un corte para analizar la cabeza y la cola
prob.cuts1 <-data.frame(cut=prf@alpha.values[[1]],
                        fpr=prf@x.values[[1]],
                        tpr=prf@y.values[[1]])

head(prob.cuts1)
tail(prob.cuts1)


prob.cuts1[prob.cuts1$tpr>=0.70,]

#Observamos que a partir del dato 3857 obtenemos un acierto positivo del 70%
#también observamos su alto índice de falso positivo que asciende a la mitad aprox.
#su dato corte está en 0.0847...

view(prob.cuts1)





##### procedemos a realizar distintos modelos con el objetivo de obtener otra perspectivas

install.packages("randomForest","caret")
install.packages("rpart")
library(randomForest)
library(caret)
library(ROCR)
library(rpart.plot)
library(rpart)

# ARBOLES DE DECISIÓN


set.seed(2018)

ejercicio_arboles <-read.table("~/EAE/Data Visualization/ivan/churn_examen.csv",sep=";",dec=".",header=TRUE)
tr.id <- createDataPartition(ejercicio$churn, p=0,7, list=FALSE)

x=ejercicio_arboles[tr.id,c(2:24)]
Y =as.factor(ejercicio_arboles[tr.id,25])

mod <-rpart(ejercicio_arboles$churn ~ . , data = ejercicio_arboles[tr.id,],
            method = "class",
            control = rpart.control(minsplit = 20,cp=0.01)) #me da error está función y no se como arreglarlo 

mod

prp(mod,type=2, extra = 104, nn=TRUE,
    fallen.leaves = TRUE, faclen = 4, varlen=8,
    shadow.col="blue")

      ## PARA EVITAR EL OVERFITTING REALIZARÍASOS UNA PODA PERO NO ME EJECUTA EL EJERCICIO
mod$

mod.pruned <- pruned(mod,mod$X[8,"X"])

prp(mod.pruned, type=2, extra=104, nn=TRUE,
    fallen.leaves = TRUE, faclen = 4, varlen = 8,
    shadow.col="red")

pred.pruned <- predict(mod, ejercicio_arboles[-tr.id,],type="class")

table(ejercicio_arboles[-tr.id,]$churn,pred.pruned,dnn=c("Actual","Predicho"))

pred.pruned2 <- predict(mod.pruned, ejercicio[-tr.id], type="prob")
pred.pruned <- predic(mod.proned, ejercicio [-tr.id,], type="class")

head(pred.pruned)
head(pred.pruned2)

pred <- prediction(pred.pruned2[,2], ejercicio[tr.id,"class"])
perf <- performance(pred,"tpr","fpr")
plot(perf)






########## OTRO ARBOL DE DECISIÓN


library(tidyverse)
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
install.packages("caret")
library(caret)
install.packages("party")
library(party)

tree <- ctree(churn~ageBin+age+annualincome+percentagecalloutsidenetwork++unpaidbalance+numberofcomplaints+state+customersuspended+education+usesinternetservice, train)
plot(tree)

pred_tree <- predict(tree, test)
print("Confusion Matrix for Decision Tree"); table(Predicted = pred_tree, Actual = test$churn)

p1 <- predict(tree, train)
tab1 <- table(Predicted = p1, Actual = train$churn)
tab2 <- table(Predicted = pred_tree, Actual = test$churn)
print(paste('Decision Tree Accuracy',sum(diag(tab2))/sum(tab2)))


#Tenemos un accuracy de 0,386919.....

#RANDOM FOREST


ejercicio_random<-read.table("~/EAE/Data Visualization/ivan/churn_examen.csv",sep=";",dec=".",header=TRUE)

sapply(ejercicio_random, function(x) sum(is.na(x)))

ejercicio_random$age[is.na(ejercicio_random$age)]<-round(mean(ejercicio_random$age,na.rm=TRUE),0)

set.seed(2018)
tr.id <- createDataPartition(ejercicio_random$churn, p=0.7, list = FALSE)


x=ejercicio_random[tr.id,c(2:24)]
y =as.factor(ejercicio_random[tr.id,25])

mod <- randomForest(x=ejercicio_random[tr.id,c(2:24)], y =as.factor(ejercicio_random[tr.id,25]),
                    ntree = 100,
                    xtest = ejercicio_random [-tr.id,c(2:24) ], ytest=as.factor(ejercicio_random[-tr.id,25]),
                    importance =TRUE, keep.forest=TRUE)

pred <- predict(mod,ejercicio_random[-tr.id,])

ejercicio_random[-tr.id,"churn"]

table(ejercicio_random[-tr.id,"churn"],pred,dnn=c("Actual","PREDICCIÓN"))

plot(mod)
probs <- predict(mod,ejercicio_random[-tr.id,], type = "prob")

pred <- prediction(probs[,2], ejercicio_random[-tr.id,"churn"])
perf <- performance(pred,"tpr","fpr")
plot(perf)
head(probs)

#Observamos que el modelo cuando preve que será un 0 coincide en gran numero ocn el actual, es decir la predicción de la
#no baja esta asentada; la diferencia la encontramos en que cuando actualmente encontramos un uno, se predice un cero en un numero que
# se asemeja en que que los el valor de predición con el actual coinciden.
#No observamos que sea un mal modelo pero tampoco con un muy alto indice de fiabilidad.