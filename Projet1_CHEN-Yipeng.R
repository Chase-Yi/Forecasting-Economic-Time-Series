# Projet 1
# M1 IREF FQA
# CHEN Yipeng
# Le but est de prévoir pour l'année 2020 la valeur de FDI pour le Japon
# FDI = les flux nets d'investissements directs étrangers en % du PIB
# On fixe le risque de première espèce alpha à 5% (alpha = 0.05)

# pour nettoyer l'environnement
rm(list = ls())

# pour changer le repertoire courant
setwd("C:/Users/c4780/Desktop/Projet1")

# pour lire des données en xls
library(rJava)
library(xlsxjars)
library(xlsx)

options(max.print = 1000000) # pour augmenter la limite de max.print dans R

#On ne garde que le pays qui nous intéresse.(Le Japon)
FDI <- read.xlsx2("NIofFDI.xls",sheetIndex = 1,header = TRUE,startRow = 1,endRow = 23,colClasses = "numeric",as.data.frame = TRUE)
View(FDI)

# Japon : jp
#la valeur de FDI du Japon se situe à la 22ème ligne
jp_FDI <- FDI[22,]
View(jp_FDI)

jp_FDI = jp_FDI[,2:ncol(jp_FDI)]
View(jp_FDI)

jp_FDI = as.numeric(jp_FDI)
View(jp_FDI)

jp_FDI = na.omit(jp_FDI)
View(jp_FDI)

jpFDI = ts(jp_FDI,start = 1970,freq = 1) # Une donnée par an => freq = (1/1) = 1

TjpFDI<-length(jpFDI)
TjpFDI #le nombre d'observations dans la série temporelle "jpFDI"

#chronogramme
par(bg="snow1") #pour changer couleur du fond
plot.ts(jpFDI,xlab = "Années", ylab = "FDI (en % du PIB)", main = "FDI du Japon de 1970 à 2019",col = 6,type = "o",lwd = 2)

abline(v = 1973,col = "blue",lwd = 2)
text(1973,0.8, "choc pétrolier",font = 4,pos = 1) #Le premier choc pétrolier mondial en 1973
text(1973,0.8, "1973",font = 4,pos = 3)

abline(v = 1986,col = 2,lwd = 2)
text(1986,0.8, "bulle spéculative",font = 4,pos = 1) #La bulle spéculative japonaise est une bulle économique survenue au Japon en 1986
text(1986,0.8, "1986",font = 4,pos = 3)

abline(v = 1997,col = 3,lwd = 2)
text(1997,0.8, "crise bancaire",font = 4,pos = 1) #Le Japon subit une crise bancaire en 1997
text(1997,0.8, "1997",font = 4,pos = 3)

abline(v = 2008,col = 7,lwd = 2)
text(2008,0.8, "crise financière",font = 4,pos = 1) #La crise financière mondiale de 2008
text(2008,0.8, "2008",font = 4,pos = 3)


################################ Tests de RU #############################################

###Avant de faire le Test de DF
###ACF et PACF pour détecter s'il y a l'autocorrélation ou pas dans notre série temporelle
Acf(jpFDI)
Pacf(jpFDI)


###Test de DF
library(forecast)
library(caschrono)
library(lmtest)
library(urca)

summary(ur.df(jpFDI,type="trend",lag=0))
#bêta(1) significatif et |rhô| < 1 => PGD TS

#ACF et PACF pour détecter s'il y a l'autocorrélation ou pas dans les aléas 
plot(ur.df(jpFDI,type="trend",lag=0))

#Pas de l'autocorrélation dans les aléas de la régression DF
#Validité de la conclusion sur le Test de DF 


###Test de ADF
pmax<-as.integer(12*(TjpFDI/100)^(0.25));library(CADFtest)
pmax
summary(CADFtest(jpFDI,criterion="MAIC",type="trend",max.lag.y=pmax))

summary(ur.df(jpFDI,type="trend",lag=2))#lag donné par le MAIC = 2
#gamma 1 et 2 ne sont pas significatifs

#On enlève d'abord le gamma 2 en mettant la valeur de lag = 1
summary(ur.df(jpFDI,type="trend",lag=1))
#gamma 1 non significatif

# => Procédure "Top-Down"
summary(ur.df(jpFDI,type="trend",lag=pmax))
#gamma 10 significatif
# - 1.07 > - 3.5 => On accepte H0 : rhô -1 = 0 => présence de racine unitaire => PGD DS


###Test de ZA
summary(ur.za(jpFDI, model="both",lag=2))#lag donné par le MAIC = 2
#gamma 1 et 2 ne sont pas significatifs

#On enlève d'abord le gamma 2 en mettant la valeur de lag = 1
summary(ur.za(jpFDI, model="both",lag=1))
#gamma 1 non significatif

#Sans prendre en compte l'autocorrélation dans notre série temporelle
summary(ur.za(jpFDI, model="both",lag=0))
#bêta(1) significatif et |rhô| < 1 => PGD TS avec un unique changement structurel en 2010

TjpFDI
###lambda = 41/50 = 0.82
plot(ur.za(jpFDI, model="both",lag=0))


###Test de LS
source("C:\\Users\\c4780\\Desktop\\Projet1\\LeeStrazicichUnitRoot-master\\LeeStrazicichUnitRootTestParallelization.R")
library(foreach)
library(doSNOW)
library(parallel)
cl <- makeCluster(max(1, detectCores() - 1))
registerDoSNOW(cl)

#Avec une seule date de rupture || lag = 5
myBreaks <- 1
myModel <- "break"
myLags <- 5 
myParallel_LS <- ur.ls.bootstrap(y=jpFDI , model = myModel, breaks = myBreaks, lags = myLags, method = "Fixed",pn = 0.1, critval = "bootstrap", print.results = "print")
#Error in { : task 1 failed

#Avec une seule date de rupture || lag = 2 #lag donné par le MAIC = 2 
myBreaks <- 1
myModel <- "break"
myLags <- 2 #lag donné par le MAIC = 2
myParallel_LS <- ur.ls.bootstrap(y=jpFDI , model = myModel, breaks = myBreaks, lags = myLags, method = "Fixed",pn = 0.1, critval = "bootstrap", print.results = "print")
#PGD DS avec 1 changement structurel en 2014

#Avec deux dates de rupture || lag = 5
myBreaks <- 2
myModel <- "break"
myLags <- 5 
myParallel_LS <- ur.ls.bootstrap(y=jpFDI , model = myModel, breaks = myBreaks, lags = myLags, method = "Fixed",pn = 0.1, critval = "bootstrap", print.results = "print")
#PGD TS avec deux changements structurels en 1997 et en 2010

#Avec deux dates de rupture || lag = 2 #lag donné par le MAIC = 2 
myBreaks <- 2
myModel <- "break"
myLags <- 2 #lag donné par le MAIC = 2 
myParallel_LS <- ur.ls.bootstrap(y=jpFDI , model = myModel, breaks = myBreaks, lags = myLags, method = "Fixed",pn = 0.1, critval = "bootstrap", print.results = "print")
#PGD TS avec deux changements structurels en 1997 et en 2010

#=> PGD DS avec 1 changement structurel en 2014 !!!


################################ Transformation de la série #############################################

#Ici, on a un PGD DS avec 1 changement structurel en 2014 !!!
#On doit différencier notre série à l'ordre 1 
djpFDI = diff(jpFDI)

TdjpFDI<-length(djpFDI)
TdjpFDI

#Comparaison de 2 chronogrammes
op <- par(mfrow = c(2,1))
par(bg="snow1") #pour changer couleur du fond
plot.ts(jpFDI,xlab = "Années", ylab = "FDI (en % du PIB)", main = "Série jpFDI : FDI du Japon de 1970 à 2019",col = 6,type = "o",lwd = 2)
plot.ts(djpFDI,xlab = "Années", ylab = "FDI (en % du PIB)", main = "Série djpFDI : FDI du Japon de 1971 à 2019",col = 6,type = "o",lwd = 2)
par(op)
#Avec notre série temporelle transformée “djpFDI”, il n’y en a plus la Tendance !!!
#Notre série temporelle transformée “djpFDI” est issue d’un PGD stationnaire !!! 


################################ Avant la Prévision #############################################

###(1)ACF,PACF et LB sur la série “djpFDI” pour voir si elle est auto-corrélée
library(forecast)

Acf(djpFDI)
Pacf(djpFDI)
Box.test(djpFDI,lag=1,type="Ljung-Box")
#Présence d’autocorrélation dans la série “djpFDI”

###(2)EACF pour déterminer les valeurs de p et q dans ARMA(p,q)
library(TSA)
eacf(djpFDI)
#=> ARMA(5,3) !!!

###(3)Estimation du ARMA(5,3)
library(forecast)
reg = Arima(djpFDI,order=c(5,0,3))
library(lmtest)
coeftest(reg)
#les coefficients ne sont pas tous significatifs !!!

###(4)On doit trouver un modèle où tous les coefficients sont significatifs !!!
#Ici,on enlève d'abord le ar5
reg = Arima(djpFDI,order=c(4,0,3))
library(lmtest)
coeftest(reg)

#On enlève ensuite le ma1
reg = Arima(djpFDI,order=c(4,0,3),fixed=c(NA,NA,NA,NA,0,NA,NA,NA))
#NA:estimation par R {ar1,ar2,ar3,ar4,ma2,ma3,intercept}
library(lmtest)
coeftest(reg)

#On enlève ensuite le ar3
reg = Arima(djpFDI,order=c(4,0,3),fixed=c(NA,NA,0,NA,0,NA,NA,NA))
#NA:estimation par R {ar1,ar2,ar4,ma2,ma3,intercept}
library(lmtest)
coeftest(reg)

#Finalement,on enlève intercept
reg = Arima(djpFDI,order=c(4,0,3),fixed=c(NA,NA,0,NA,0,NA,NA),include.mean=F)
#NA:estimation par R {ar1,ar2,ar4,ma2,ma3}
library(lmtest)
coeftest(reg)

AIC(reg) #Bonne critère d'information pour faire la prévision 

###(5)Vérification sur les aléas de la régression
#On vérifie que les aléas de notre régression sont bien des BB !!!

##(5a)Test de Jarque-Bera sur les résidus (non obligatoire)
#Est-ce que les aléas sont normalement distribuées ?
library(tseries)
jarque.bera.test(reg$res)
#p-value < 0.05 => On rejette H0 => Les aléas ne sont pas normalement distribuées !!!

##(5b)Test de Student sur les résidus {t.test}
#Est-ce que l’espérance des aléas est nulle ?
t.test(reg$res)

##(5c)Standardisation des résidus
#on va standardiser (centrer et réduire) les résidus
Sres = (reg$res - mean(reg$res)) / sd(reg$res)
Sres

##(5d)Test de Ljung-Box sur les résidus centrés-réduits
#H0 : absence d'autocorrélation jusqu'à l'ordre K(lag)
Box.test(Sres,lag=1,type="Ljung-Box") #Test de LB à l'ordre 1

Box.test(Sres,lag=10,type="Ljung-Box") #Test de LB à l'ordre 10

Box.test(Sres,lag=20,type="Ljung-Box") #Test de LB à l'ordre 20

Box.test(Sres,lag=30,type="Ljung-Box") #Test de LB à l'ordre 30

Box.test(Sres,lag=40,type="Ljung-Box") #Test de LB à l'ordre 40
#=> Absence d’autocorrélation dans les aléas !!!

##(5e)Test d’Engle sur les résidus centrés-réduits
#H0 : pas d'Effets ARCH = pas de clusters de volatilité = homoscédasticité conditionnelle
library(FinTS)
ArchTest(Sres,lag=1)

ArchTest(Sres,lag=10)


#On va supposer qu'il n’existe pas de clusters de volatilité !!! (Hypothèse Forte)


################################ Prévision #############################################

###Prévision pour la valeur de FDI du Japon en 2020
fc <- forecast(djpFDI,h = 3,model = reg) 
# h: périodes de prévision ; Ici,h = 3 ans car les données sont annuelles

fc

fc2020 <- -0.5527874
fc2020

jpFDI2019 <-jpFDI[50]
jpFDI2019

jpFDI2020 <- jpFDI2019 + fc2020
jpFDI2020

# jpFDI2020 = 0.1787852
# Sans modéliser des clusters de volatilité dans notre modèle, 
# nous avons obtenu la valeur prévue des 
# Flux nets d'Investissements Directs étrangers en % du PIB
# pour le Japon en 2020 qui est proche de 0.18 !!!

plot(fc)


################################ Annexe : C1 #############################################
################################ Test de ZA avec lag = 10 sur la série initiale "jpFDI" ####################

###Test de ZA
#lag = 10, car on a introduit 10 variables explicatives additionnelles afin de modéliser l'autocorrélation dans le Test de ADF  
summary(ur.za(jpFDI, model="both",lag=10)) 
#du et dt ne sont pas significatifs !!!
# => Le modèle "both" n'est pas la bonne spécification !!!
#On passe au modèle "crash" {"intercept"}
summary(ur.za(jpFDI, model="intercept",lag=10)) 
#du n'est pas significatif !!!
# => Le modèle "crash" n'est pas la bonne spécification !!!


################################ Annexe : C2 #############################################
################################ Test de LS avec lag = 4 sur la série initiale "jpFDI" {en supposant une seule date de rupture}####################

###Test de LS
source("C:\\Users\\c4780\\Desktop\\Projet1\\LeeStrazicichUnitRoot-master\\LeeStrazicichUnitRootTestParallelization.R")
library(foreach)
library(doSNOW)
library(parallel)
cl <- makeCluster(max(1, detectCores() - 1))
registerDoSNOW(cl)

#Avec une seule date de rupture || lag = 4
myBreaks <- 1
myModel <- "break"
myLags <- 4 
myParallel_LS <- ur.ls.bootstrap(y=jpFDI , model = myModel, breaks = myBreaks, lags = myLags, method = "Fixed",pn = 0.1, critval = "bootstrap", print.results = "print")
#PGD DS avec 1 changement structurel en 2006


################################ Annexe : D #############################################
################################ Tests de RU sur la série temporelle transformée “djpFDI” ####################

###Avant de faire le Test de DF
###ACF et PACF pour détecter s'il y a l'autocorrélation ou pas dans notre série temporelle “djpFDI”
Acf(djpFDI)
Pacf(djpFDI)


###Test de DF
library(forecast)
library(caschrono)
library(lmtest)
library(urca)

summary(ur.df(djpFDI,type="trend",lag=0))
#bêta(1) non significatif

summary(ur.df(djpFDI,type="drift",lag=0))
#bêta(0) non significatif

summary(ur.df(djpFDI,type="none",lag=0))
#=> Pas de Racine Unitaire !!!

#ACF et PACF pour détecter s'il y a l'autocorrélation ou pas dans les aléas 
plot(ur.df(djpFDI,type="none",lag=0))
#Présence de l’autocorrélation dans les aléas de la régression DF
#=> Non Validité de la conclusion sur le Test de DF


###Test de ADF
TdjpFDI
pmax<-as.integer(12*(TdjpFDI/100)^(0.25));library(CADFtest)
pmax
summary(CADFtest(djpFDI,criterion="MAIC",type="none",max.lag.y=pmax))
#lag donné par le MAIC = 0

summary(ur.df(djpFDI,type="none",lag=pmax,selectlag="BIC"))
#gamma 1 non significatif

# => Procédure "Top-Down"
summary(ur.df(djpFDI,type="none",lag=pmax))
#gamma 10 non significatif
summary(ur.df(djpFDI,type="none",lag=pmax-1))
#gamma 9 non significatif

summary(ur.df(djpFDI,type="none",lag=pmax-2))
#gamma 8 significatif
# -0.572 > -1.95 => On accepte H0 : rhô -1 = 0 
#=> Présence de Racine Unitaire !!!


###Test de ZA
#lag utilisé pour modéliser l'autocorrélation dans le Test de ADF = 8
summary(ur.za(djpFDI, model="both",lag=8))
#le modèle "both" est la bonne spécification
#gamma 4,5,6,7,8 ne sont pas significatifs !!!
#Ici, notre modèle “both” n’arrive pas à prendre en compte l’autocorrélation

#On enlève d'abord le gamma 8 en mettant la valeur de lag = 8-1 = 7
summary(ur.za(djpFDI, model="both",lag=7))
#le modèle "both" est la bonne spécification
#gamma 1,2,3,4,5,6,7 sont significatifs !!!
#-8.0641 < -5.08 => On rejette H0 : rhô = 1 
# => Pas de Racine Unitaire !!!

#lag donné par le BIC = 1
summary(ur.za(djpFDI, model="both",lag=1))
#le modèle "both" est la bonne spécification
#gamma 1 est significatif !!!
#-8.0377 < -5.08 => On rejette H0 : rhô = 1
# => Pas de Racine Unitaire !!!


###Test de LS
source("C:\\Users\\c4780\\Desktop\\Projet1\\LeeStrazicichUnitRoot-master\\LeeStrazicichUnitRootTestParallelization.R")
library(foreach)
library(doSNOW)
library(parallel)
cl <- makeCluster(max(1, detectCores() - 1))
registerDoSNOW(cl)

#Avec une seule date de rupture || lag = 1 #car lag donné par le BIC = 1 
myBreaks <- 1
myModel <- "break"
myLags <- 1 
myParallel_LS <- ur.ls.bootstrap(y=djpFDI, model = myModel, breaks = myBreaks, lags = myLags, method = "Fixed",pn = 0.1, critval = "bootstrap", print.results = "print")
#-7.472766 < - 4.51 => On rejette H0 : phi = 0 
# => Pas de Racine Unitaire !!!

#Avec une seule date de rupture || lag = 5  
myBreaks <- 1
myModel <- "break"
myLags <- 5 
myParallel_LS <- ur.ls.bootstrap(y=djpFDI, model = myModel, breaks = myBreaks, lags = myLags, method = "Fixed",pn = 0.1, critval = "bootstrap", print.results = "print")
# Error in { : task 1 failed

#Avec une seule date de rupture || lag = 4  
myBreaks <- 1
myModel <- "break"
myLags <- 4 
myParallel_LS <- ur.ls.bootstrap(y=djpFDI, model = myModel, breaks = myBreaks, lags = myLags, method = "Fixed",pn = 0.1, critval = "bootstrap", print.results = "print")
#-5.475226 < - 4.51 => On rejette H0 : phi = 0 
# => Pas de Racine Unitaire !!!


#Conclusion retenue : Pas de Racine Unitaire !!! pour notre série temporelle “djpFDI” 


################################ Fin du projet 1 ############################