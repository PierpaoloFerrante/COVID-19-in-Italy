#####################################################global numbers #####################################################
#Last date of analysis
Mdata<-736 #ultima data di studio
Mdata1<-Stime$date1[Stime$date==Mdata] #ultimo giorno


per<-Stime$date1<=Mdata 

#lethality
lethality<-sum(Stime$IFRVarVac[per]*Stime$NVarVacc[per])/sum(Stime$NVarVacc[per])
c(max(Stime$IFRVarVac[per]),as.character(Stime$date[Stime$IFRVarVac==max(Stime$IFRVarVac)]))[1]
c(min(Stime$IFRVarVac[per]),as.character(Stime$date[Stime$IFRVarVac==min(Stime$IFRVarVac)]))[1]
lethality1<-sum(Stime$IFRVar[per]*Stime$NVarVacc1[per])/sum(Stime$NVarVacc1[per])

#deaths
deaths<-sum(Stime$dWMA[per])
max(Stime$dWMA[per])
Stime$date[Stime$dWMA==max(Stime$dWMA[per])]
deaths1<-sum(Stime$IFRVar[per]*Stime$NVarVacc1[per])

#incidence
sum(Stime$NVarVacc[per])
qnbinom(0.975, deaths, lethality, lower.tail = TRUE, log.p = FALSE)+deaths
qnbinom(0.975, deaths, lethality, lower.tail = FALSE, log.p = FALSE)+deaths

sum(Stime$NVarVacc1[per])
qnbinom(0.975, deaths1, lethality1, lower.tail = TRUE, log.p = FALSE)+deaths1
qnbinom(0.975, deaths1, lethality1, lower.tail = FALSE, log.p = FALSE)+deaths1


#n tot deaths, detected cases & tests
sum(Covid19all$dWMA[Covid19all$IDDate<=Mdata],na.rm=TRUE)
sum(Covid19all$nWMA[Covid19all$IDDate<=Mdata],na.rm=TRUE)
sum(Covid19all$testWMA[Covid19all$IDDate<=Mdata],na.rm=TRUE)
sum(Covid19all$nWMA[Covid19all$IDDate<=Mdata],na.rm=TRUE)/sum(Stime$NVarVacc[per])

#sum(Covid19all$dPC[Covid19all$date1<=Mdata1 & Covid19all$IDReg==23],na.rm=TRUE)
#sum(Covid19all$nPC[Covid19all$date1<=Mdata1 & Covid19all$IDReg==23],na.rm=TRUE)

#VOC
min(Stime$IDDate[Stime$IFR!=Stime$IFRVar & !is.na(Stime$IFRVar)],na.rm = TRUE) #inizio Alpha
min(Stime$date[Stime$IFR!=Stime$IFRVar & !is.na(Stime$IFRVar)],na.rm=TRUE) #inizio Alpha

min(Pred$IDDate[Pred$Per=="Alpha"]) #the begining of Alpha's dominance
min(Pred$IDDate[Pred$Per=="Delta"]) #the begining of Delta's dominance
min(Pred$IDDate[Pred$Per=="Omicron"]) #the begining of Omicron's dominance
ifelse("Beta" %in% Pred$Per,min(Pred$IDDate[Pred$Per=="Beta"]),"Beta was never dominant in Italy") #the begining of Beta's dominance
ifelse("Gamma" %in% Pred$Per,min(Pred$IDDate[Pred$Per=="Gamma"]),"Gamma was never dominant in Italy") #the begining of Gamma's dominance
              
Pred$Per[Pred$IDDate>330 & Pred$IDDate<360]
v1<-Pred$date[Pred$IDDate==min(Pred$IDDate[Pred$Per=="Alpha"])] #inizio Alpha
v2<-Pred$date[Pred$IDDate==min(Pred$IDDate[Pred$Per=="Delta"])] #inizio Delta
v3<-Pred$date[Pred$IDDate==min(Pred$IDDate[Pred$Per=="Omicron"])] #inizio Omicron
v2-v1 # n. giorni alpha
v3-v2 # n. giorni Delta
Mdata1-Pred$IDDate[Pred$IDDate==min(Pred$IDDate[Pred$Per=="Omicron"])] # n. giorni Omicron, fino al 28/02/2022

#Vaccini
V1<-sum(dati.ISS$`CV1/2`[dati.ISS$IDDate==Mdata1]) # 1 dose
V2<-sum(dati.ISS$CVOltre4M[dati.ISS$IDDate==Mdata1]) # 2 oltre 4 mesi dosi
V3<-sum(dati.ISS$CVin4M[dati.ISS$IDDate==Mdata1] ) # 2 entro 4 mesi dosi
V4<-sum(dati.ISS$Boostercum[dati.ISS$IDDate==Mdata1]) # 3 dosi
V0<-sum(dati.ISS$NoCVcorr[dati.ISS$IDDate==Mdata1]) # 0 dosi
V0+V1+V2+V3+V4 # totale
V0/(V0+V1+V2+V3+V4)
V1/(V0+V1+V2+V3+V4)
V2/(V0+V1+V2+V3+V4)
V3/(V0+V1+V2+V3+V4)
V4/(V0+V1+V2+V3+V4)
7098195+1506872+7447233+4830603+36783081 #non vaccinati e vaccinati (x livello: una dose, 2 dosi da oltre 120 gg, ...) da report ISS del 06/04/2022
(2000000+7098195)/(2000000+7098195+1506872+7447233+4830603+36783081) #% non vaccinati da report ISS del 06/04/2022 (2000k sono bambini 0-4 anni)

#test marzo 2020 e feb 2022
sum(Covid19all$test[Covid19all$date>="2020-03-01" & Covid19all$date<="2020-03-31"],na.rm=TRUE)
sum(Covid19all$test[Covid19all$date>="2022-02-01" & Covid19all$date<="2022-02-28"],na.rm=TRUE)

#Diff IFRMed IFRISTAT in gg<=120
(Stime$IFRISTAT[Stime$IDDate<120]-Stime$IFRMed[Stime$IDDate<120])/Stime$IFRISTAT[Stime$IDDate<120]
z1<-Stime$IFRVarVac[Stime$IDDate>120 & Stime$IFRVarVac!=0]
z3<-(Stime$IFRVarVac[ Stime$IDDate>120 & Stime$IFRVarVac!=0]-Stime$IFRVarVac[Stime$IDDate>120 & Stime$IFRVarVac!=0])/Stime$IFRVarVac[Stime$IDDate>120 & Stime$IFRVarVac!=0]
c(z1,z3)

#deaths saved by vaccines to 28/02/2022
Stime$date[Stime$date1==307] # data inizio campagna vaccinazione
per1<-Stime$date1>307 & Stime$date1<=736 # record da selezionare
sum(Stime$NVarVacc1[per1]*Stime$IFRVar[per1])-sum(Stime$dWMA[per1]) #decessi evitati
(sum(Stime$NVarVacc1[per1]*Stime$IFRVar[per1])-sum(Stime$dWMA[per1]))/sum(Stime$NVarVacc1[per1]*Stime$IFRVar[per1]) #%decessi evitati
sum(Stime$NVarVacc1[per1]*Stime$IFRVar[per1])-sum(Stime$NVarVacc[per1]*Stime$IFRVar[per1]) #decessi evitati tra i contagi che ci sarebbero stati
sum(Stime$NVarVacc[per1]*Stime$IFRVar[per1])-sum(Stime$dWMA[per1]) #decessi evitati tra i contagi che ci sono stati
62901.23/(62901.23+51187.74)
sum(Stime$NVarVacc1[Stime$date1<737]*Stime$IFRVar[Stime$date1<737])
sum(Stime$NVarVacc1[Stime$date1>307 & Stime$date1<737]*Stime$IFRVar[Stime$date1>307 & Stime$date1<737])

#deaths saved by vaccines to 31/01/2022
per2<-Stime$date1>307 & Stime$date1<=708 # record da selezionare
sum(Stime$NVarVacc1[per2]*Stime$IFRVar[per2])-sum(Stime$dWMA[per2]) #decessi evitati
(sum(Stime$NVarVacc1[per2]*Stime$IFRVar[per2])-sum(Stime$dWMA[per2]))/sum(Stime$NVarVacc1[per2]*Stime$IFRVar[per2]) #%decessi evitati
sum(Stime$NVarVacc1[per2]*Stime$IFRVar[per2])-sum(Stime$NVarVacc[per2]*Stime$IFRVar[per2]) #decessi evitati tra i contagi che ci sarebbero stati
sum(Stime$NVarVacc[per2]*Stime$IFRVar[per2])-sum(Stime$dWMA[per2]) #decessi evitati tra i contagi che ci sono stati
62901.23/(62901.23+51187.74)
sum(Stime$NVarVacc1[Stime$date1<737]*Stime$IFRVar[Stime$date1<737])
sum(Stime$NVarVacc1[Stime$date1>307 & Stime$date1<737]*Stime$IFRVar[Stime$date1>307 & Stime$date1<737])

#infections avoided  by vaccines
sum(Stime$NVarVacc1[per1])-sum(Stime$NVarVacc[per1])
1-sum(Stime$NVarVacc[per1])/sum(Stime$NVarVacc1[per1])
(sum(Stime$NVarVacc1[per2])-sum(Stime$NVarVacc[per2]))*sum(Covid19all$nWMA[Covid19all$date1<=708],na.rm=TRUE)/sum(Stime$NVarVacc[Stime$date1<=708])
#####################################################Fine global numbers #####################################################

#####################################################Fine inserisco onde in Stime #####################################################
Stime$wave[Stime$date1<=147]<-1                     # 147
Stime$wave[Stime$date1>147 & Stime$date1<=347]<-2 #(347-147)
Stime$wave[Stime$date1>347 & Stime$date1<=486]<-3 #(486-347)
Stime$wave[Stime$date1>486 & Stime$date1<=586]<-4 #(586-486)
Stime$wave[Stime$date1>586 & Stime$date1<=736]<-5 #(736-586)
Stime$wave[Stime$date1>736]<-0
#####################################################Fine inserisco onde in Stime #####################################################

##################################################### Tab onde #####################################################
#Prendo selezione la regione "Italia" da Stime
It<-Stime$IDDate<737

############tutte le varianti
#tabella con caratteristiche onde: decessi, incidenza (con IC), diagnosi e pesi decessi e incidenza per onda
a<-aggregate(list(Stime$dWMA[It],Stime$IFRVar[It]*Stime$NVarVacc1[It],Stime$IFRVarVac[It]*Stime$nWMA[It],Stime$nWMA[It],Stime$IFRVarVac[It]*Stime$NVarVacc[It],Stime$NVarVacc[It],Stime$IFRVar[It]*Stime$NVarVacc1[It],Stime$NVarVacc1[It]),list(Stime$wave[It]),sum)
colnames(a)<-c("wave","dWMA","dWMA1","IFRVarVacn*nWMA","nWMA","IFRVarVacN*NVarVacc","NVarVacc","IFRVarN*NVarVacc1","NVarVacc1")

#tolgo l'onda zero, messaper girni superiori a 736
a<-a[a$wave!=0,]

#pesi delle onde per diagnosi e incidenza
a$PernWMA<-a$nWMA/sum(a$nWMA)
a$PerNVarVacc<-a$NVarVacc/sum(a$NVarVacc)

#lealità per onda. la calcolo con pesi delle diagnosi e dei casi giornalieri. 
#Come pesi uso incidenza che mi da restituisce un incidenza x onda pari alla somma delle incidenze gionaliere.
a$IFRVarVacn<-a$"IFRVarVacn*nWMA"/a$nWMA #letalità media pesata x onda con pesi delle diagnosi giornaliere
a$IFRVarVacN<-a$"IFRVarVacN*NVarVacc"/a$NVarVacc #letalità media pesata x onda con pesi dell'incidenza giornaliera. 
a$IFRVarN<-a$"IFRVarN*NVarVacc1"/a$NVarVacc1 #letalità media pesata senza vaccini x onda con pesi dell'incidenza giornaliera. 

#incidenza onde con iternalli di confidenza
a$N<-a$dWMA/a$IFRVarVacN
a$NLB<-qnbinom(0.975, a$dWMA, a$IFRVarVacN, lower.tail = TRUE, log.p = FALSE)+a$dWMA
a$NUB<-qnbinom(0.975, a$dWMA, a$IFRVarVacN, lower.tail = FALSE, log.p = FALSE)+a$dWMA
a$N1<-a$dWMA1/a$IFRVarN
a$N1LB<-qnbinom(0.975, a$dWMA1, a$IFRVarN, lower.tail = TRUE, log.p = FALSE)+a$dWMA1
a$N1UB<-qnbinom(0.975, a$dWMA1, a$IFRVarN, lower.tail = FALSE, log.p = FALSE)+a$dWMA1
sum(a$N)

#Durata onda Incidenza in gg e massimo
mxN<-aggregate(list(Stime$date1[It],Stime$NVarVacc[It]),list(Stime$wave[It]),max)
colnames(mxN)<-c("wave","GmaxN","maxN")
a<-merge(a,mxN)

#Durata onda decessi in gg e massimo
mxd<-aggregate(list(Stime$date1[It],Stime$dWMA[It]),list(Stime$wave[It]),max)
colnames(mxd)<-c("wave","Gmaxd","maxd")
a<-merge(a,mxd)

#creo tabella con: n onda,  Incidenza (come somma giornaliera e come quella di periodo)
a<-a[,c("wave","dWMA","dWMA1","NVarVacc","NVarVacc1","N","NLB","NUB","IFRVarVacN","GmaxN" ,"maxN","Gmaxd","maxd","N1","N1LB","N1UB","IFRVarN")]

#aggiungo periodo x onda
D1W<-aggregate(Stime$date1[It & Stime$wave!=0],list(Stime$wave[It & Stime$wave!=0]),min) #inizio x onda in num gg
colnames(D1W)<-c("wave","Dmin")
D1W$Dmin<-Stime$date[Stime$date1 %in% D1W$Dmin & It]#trasformo num giorni in data
D2W<-aggregate(Stime$date1[It & Stime$wave!=0],list(Stime$wave[It & Stime$wave!=0]),max) #fine x onda in num gg
colnames(D2W)<-c("wave","Dmax")
D2W$Dmax<-Stime$date[Stime$date1 %in% D2W$Dmax & It]#trasformo num giorni in data
D1W<-merge(D1W,D2W) #unisco inizio e fine
D1W<-merge(D1W,a) #unisco con tab a
colnames(a)

# picchi incidenza e mortalià x onda
mxd<-aggregate(list(Stime$NVarVacc[It],Stime$dWMA[It]),list(Stime$wave[It]),max)
colnames(mxd)<-c("wave","Nmx","dWMAmx")
mxd<-mxd[mxd$wave!=0,]

# data data picchi incidenza e mortalià x onda
mxd$GmaxN<-Stime$date[Stime$NVarVacc %in% mxd$N & It]
mxd$Gmaxd<-Stime$date[Stime$dWMA %in% mxd$dWMA & It]

D1W<-merge(D1W[,-c(12,14)],mxd[,-c(2:3)])  #unisco con tab D1W

#inserisco i test x onda
a<-aggregate(list(Stime$nWMA[It],Stime$testWMA[It]),list(Stime$wave[It]),sum)
colnames(a)<-c("wave","nWMA","testWMA")
Tab<-merge(D1W,a[a$wave!=0,])  #unisco con tab D1W
Tab$VOC<-"zALL"
Tab
sum(Tab$NVarVacc)
sum(Stime$NVarVacc[It])
sum(Tab$dWMA)

#giorno inizio varianti
Stime$date1[Stime$date=="2020-10-01"] #alfa
Stime$date1[Stime$date=="2020-06-01"] #beta
Stime$date1[Stime$date=="2020-12-01"] #gamma
Stime$date1[Stime$date=="2020-11-01"] #delta
Stime$date1[Stime$date=="2021-12-01"] #omicron
nrow(Stime)/23

#########Alpha
#tabella con caratteristiche onde: decessi, incidenza (con IC), diagnosi e pesi decessi e incidenza per onda
a1<-aggregate(list(Stime$NVarVacc[It]*Stime$Alfa[It],Stime$NVarVacc1[It]*Stime$Alfa[It],Stime$nWMA[It]*Stime$Alfa[It],Stime$IFRVarVacAlfa[It]*Stime$NVarVacc[It]*Stime$Alfa[It],Stime$IFRVarAlfa[It]*Stime$NVarVacc1[It]*Stime$Alfa[It]),list(Stime$wave[It]),sum)
colnames(a1)<-c("wave","NVarVacc","NVarVacc1","nWMA","dWMA","dWMA1")

#tolgo l'onda zero, messaper girni superiori a 736
a1<-a1[a1$wave!=0,]

#pesi delle onde per diagnosi e incidenza
a1$PernWMA<-a1$nWMA/sum(a1$nWMA)
a1$PerNVarVacc<-a1$NVarVacc/sum(a1$NVarVacc)

#lealità per onda. la calcolo con pesi delle diagnosi e dei casi giornalieri. 
#Come pesi uso incidenza che mi da restituisce un incidenza x onda pari alla somma delle incidenze gionaliere.
#a1$IFRVarVacn<-a1$"IFRVarVacn*nWMA"/a1$nWMA #letalità media pesata x onda con pesi delle diagnosi giornaliere
a1$IFRVarVacN<-a1$dWMA/a1$NVarVacc #letalità media pesata x onda con pesi dell'incidenza giornaliera. 
a1$IFRVarN<-a1$dWMA1/a1$NVarVacc1 #letalità media pesata x onda con pesi dell'incidenza giornaliera. 


#incidenza onde con iternalli di confidenza
a1$N<-a1$dWMA/a1$IFRVarVacN
a1$NLB[2:5]<-qnbinom(0.975, a1$dWMA[!is.na(a1$IFRVarVacN)], a1$IFRVarVacN[!is.na(a1$IFRVarVacN)], lower.tail =FALSE, log.p = FALSE)+a1$dWMA[!is.na(a1$IFRVarVacN)]
a1$NUB[2:5]<-qnbinom(0.975, a1$dWMA[!is.na(a1$IFRVarVacN)], a1$IFRVarVacN[!is.na(a1$IFRVarVacN)], lower.tail =TRUE , log.p = FALSE)+a1$dWMA[!is.na(a1$IFRVarVacN)]
a1$N1<-a1$dWMA1/a1$IFRVarN
a1$N1LB[2:5]<-qnbinom(0.975, a1$dWMA1[!is.na(a1$IFRVarN)], a1$IFRVarVacN[!is.na(a1$IFRVarN)], lower.tail =FALSE, log.p = FALSE)+a1$dWMA1[!is.na(a1$IFRVarVacN)]
a1$N1UB[2:5]<-qnbinom(0.975, a1$dWMA1[!is.na(a1$IFRVarN)], a1$IFRVarVacN[!is.na(a1$IFRVarN)], lower.tail =TRUE , log.p = FALSE)+a1$dWMA1[!is.na(a1$IFRVarVacN)]

#Durata onda Incidenza in gg e massimo
mxN<-aggregate(list(Stime$date1[It],Stime$NVarVacc[It]*Stime$Alfa[It]),list(Stime$wave[It]),max)
colnames(mxN)<-c("wave","GmaxN","maxN")
a1<-merge(a1,mxN)

#Durata onda decessi in gg e massimo
mxd<-aggregate(list(Stime$date1[It],Stime$NVarVacc[It]*Stime$Alfa[It]*Stime$IFRVarVacAlfa[It]),list(Stime$wave[It]),max)
colnames(mxd)<-c("wave","Gmaxd","maxd")
a1<-merge(a1,mxd)


#aggiungo periodo x onda
D1W<-aggregate(Stime$date1[It & Stime$wave!=0],list(Stime$wave[It & Stime$wave!=0]),min) #inizio x onda in num gg
colnames(D1W)<-c("wave","Dmin")
D1W$Dmin<-Stime$date[Stime$date1 %in% D1W$Dmin & It]#trasformo num giorni in data
D2W<-aggregate(Stime$date1[It & Stime$wave!=0],list(Stime$wave[It & Stime$wave!=0]),max) #fine x onda in num gg
colnames(D2W)<-c("wave","Dmax")
D2W$Dmax<-Stime$date[Stime$date1 %in% D2W$Dmax & It]#trasformo num giorni in data
D1W<-merge(D1W,D2W) #unisco inizio e fine
D1W<-merge(D1W,a1) #unisco con tab a

# picchi incidenza e mortalià x onda
mxd<-aggregate(list(Stime$NVarVacc[It]*Stime$Alfa[It],Stime$NVarVacc[It]*Stime$Alfa[It]*Stime$IFRVarVacAlfa[It],Stime$NVarVacc1[It]*Stime$Alfa[It],Stime$NVarVacc1[It]*Stime$Alfa[It]*Stime$IFRVarAlfa[It]),list(Stime$wave[It]),max)
colnames(mxd)<-c("wave","Nmx","dWMAmx","N1mx","dWMA1mx")
mxd<-mxd[mxd$wave!=0,]

# data picchi incidenza e mortalià x onda
mxd$GmaxN[mxd$Nmx>0] <-as.character(Stime$date[round(Stime$NVarVacc*Stime$Alfa,4) %in% round(mxd$Nmx[mxd$Nmx>0],4) & It])
mxd$Gmaxd[mxd$Nmx>0] <-as.character(Stime$date[round(Stime$NVarVacc*Stime$Alfa*Stime$IFRVarVacAlfa,4) %in% round(mxd$dWMAmx[mxd$dWMAmx>0],4) & It])
#mxd$GmaxN[mxd$N1mx>0]<-as.character(Stime$date[round(Stime$NVarVacc1*Stime$Alfa,4) %in% round(mxd$N1mx[mxd$Nmx>0],4) & It])
#mxd$Gmaxd[mxd$N1mx>0]<-as.character(Stime$date[round(Stime$NVarVacc1*Stime$Alfa*Stime$IFRVarAlfa,4) %in% round(mxd$dWMA1mx[mxd$dWMAmx>0],4) & It])


D1W<-merge(D1W[,-c(19,21)],mxd[,-c(2:5)])  #unisco con tab D1W

#inserisco i test x onda
a1<-aggregate(list(Stime$nWMA[It]*Stime$Alfa[It],Stime$testWMA[It]*Stime$Alfa[It]),list(Stime$wave[It]),sum)
colnames(a1)<-c("wave","nWMA","testWMA")
D1W<-merge(D1W,a1[a1$wave!=0,])  #unisco con tab D1W

D1W<-D1W[,-c(9:10)]
D1W$VOC<-"Alpha"

Tab<-rbind(Tab,D1W)
colnames(Tab)
colnames(D1W)
colnames(D1W) %in% colnames(Tab)

#########Beta
#tabella con caratteristiche onde: decessi, incidenza (con IC), diagnosi e pesi decessi e incidenza per onda
a1<-aggregate(list(Stime$NVarVacc[It]*Stime$Beta[It],Stime$NVarVacc1[It]*Stime$Beta[It],Stime$nWMA[It]*Stime$Beta[It],Stime$IFRVarVacBeta[It]*Stime$NVarVacc[It]*Stime$Beta[It],Stime$IFRVarBeta[It]*Stime$NVarVacc1[It]*Stime$Beta[It]),list(Stime$wave[It]),sum)
colnames(a1)<-c("wave","NVarVacc","NVarVacc1","nWMA","dWMA","dWMA1")

#tolgo l'onda zero, messaper girni superiori a 736
a1<-a1[a1$wave!=0,]

#pesi delle onde per diagnosi e incidenza
a1$PernWMA<-a1$nWMA/sum(a1$nWMA)
a1$PerNVarVacc<-a1$NVarVacc/sum(a1$NVarVacc)

#lealità per onda. la calcolo con pesi delle diagnosi e dei casi giornalieri. 
#Come pesi uso incidenza che mi da restituisce un incidenza x onda pari alla somma delle incidenze gionaliere.
#a1$IFRVarVacn<-a1$"IFRVarVacn*nWMA"/a1$nWMA #letalità media pesata x onda con pesi delle diagnosi giornaliere
a1$IFRVarVacN<-a1$dWMA/a1$NVarVacc #letalità media pesata x onda con pesi dell'incidenza giornaliera. 
a1$IFRVarN<-a1$dWMA1/a1$NVarVacc1 #letalità media pesata x onda con pesi dell'incidenza giornaliera. 


#incidenza onde con iternalli di confidenza
a1$N<-a1$dWMA/a1$IFRVarVacN
a1$NLB<-a1$N
a1$NLB[2:5]<-qnbinom(0.975, a1$dWMA[!is.na(a1$IFRVarVacN)], a1$IFRVarVacN[!is.na(a1$IFRVarVacN)], lower.tail =FALSE, log.p = FALSE)+a1$dWMA[!is.na(a1$IFRVarVacN)]
a1$NUB<-a1$N
a1$NUB[2:5]<-qnbinom(0.975, a1$dWMA[!is.na(a1$IFRVarVacN)], a1$IFRVarVacN[!is.na(a1$IFRVarVacN)], lower.tail =TRUE , log.p = FALSE)+a1$dWMA[!is.na(a1$IFRVarVacN)]
a1$N1<-a1$dWMA1/a1$IFRVarN
a1$N1LB[2:5]<-qnbinom(0.975, a1$dWMA1[!is.na(a1$IFRVarN)], a1$IFRVarVacN[!is.na(a1$IFRVarN)], lower.tail =FALSE, log.p = FALSE)+a1$dWMA1[!is.na(a1$IFRVarVacN)]
a1$N1UB[2:5]<-qnbinom(0.975, a1$dWMA1[!is.na(a1$IFRVarN)], a1$IFRVarVacN[!is.na(a1$IFRVarN)], lower.tail =TRUE , log.p = FALSE)+a1$dWMA1[!is.na(a1$IFRVarVacN)]


#Durata onda Incidenza in gg e massimo
mxN<-aggregate(list(Stime$date1[It],Stime$NVarVacc[It]*Stime$Beta[It]),list(Stime$wave[It]),max)
colnames(mxN)<-c("wave","GmaxN","maxN")
a1<-merge(a1,mxN)

#Durata onda decessi in gg e massimo
mxd<-aggregate(list(Stime$date1[It],Stime$NVarVacc[It]*Stime$Beta[It]*Stime$IFRVarVacBeta[It]),list(Stime$wave[It]),max)
colnames(mxd)<-c("wave","Gmaxd","maxd")
a1<-merge(a1,mxd)


#aggiungo periodo x onda
D1W<-aggregate(Stime$date1[It & Stime$wave!=0],list(Stime$wave[It & Stime$wave!=0]),min) #inizio x onda in num gg
colnames(D1W)<-c("wave","Dmin")
D1W$Dmin<-Stime$date[Stime$date1 %in% D1W$Dmin & It]#trasformo num giorni in data
D2W<-aggregate(Stime$date1[It & Stime$wave!=0],list(Stime$wave[It & Stime$wave!=0]),max) #fine x onda in num gg
colnames(D2W)<-c("wave","Dmax")
D2W$Dmax<-Stime$date[Stime$date1 %in% D2W$Dmax & It]#trasformo num giorni in data
D1W<-merge(D1W,D2W) #unisco inizio e fine
D1W<-merge(D1W,a1) #unisco con tab a


# picchi incidenza e mortalià x onda
mxd<-aggregate(list(Stime$NVarVacc[It]*Stime$Beta[It],Stime$NVarVacc[It]*Stime$Beta[It]*Stime$IFRVarVacBeta[It]),list(Stime$wave[It]),max)
colnames(mxd)<-c("wave","Nmx","dWMAmx")
mxd<-mxd[mxd$wave!=0,]

# data picchi incidenza e mortalià x onda
mxd$GmaxN[mxd$N>0]<-as.character(Stime$date[round(Stime$NVarVacc*Stime$Beta,4) %in% round(mxd$Nmx[mxd$Nmx>0],4) & It])
mxd$Gmaxd[mxd$N>0]<-as.character(Stime$date[round(Stime$NVarVacc*Stime$Beta*Stime$IFRVarVacBeta,4) %in% round(mxd$dWMA[mxd$dWMA>0],4) & It])

D1W<-merge(D1W[,-c(19,21)],mxd[,-c(2:3)])  #unisco con tab D1W

#inserisco i test x onda
a1<-aggregate(list(Stime$nWMA[It]*Stime$Beta[It],Stime$testWMA[It]*Stime$Beta[It]),list(Stime$wave[It]),sum)
colnames(a1)<-c("wave","nWMA","testWMA")
D1W<-merge(D1W,a1[a1$wave!=0,])  #unisco con tab D1W

D1W<-D1W[,-c(9:10)]
D1W$VOC<-"Beta"

Tab<-rbind(Tab,D1W)
colnames(Tab)
colnames(D1W)
colnames(D1W) %in% colnames(Tab)

#########Gamma
#tabella con caratteristiche onde: decessi, incidenza (con IC), diagnosi e pesi decessi e incidenza per onda
a1<-aggregate(list(Stime$NVarVacc[It]*Stime$Gamma[It],Stime$NVarVacc1[It]*Stime$Gamma[It],Stime$nWMA[It]*Stime$Gamma[It],Stime$IFRVarVacGamma[It]*Stime$NVarVacc[It]*Stime$Gamma[It],Stime$IFRVarGamma[It]*Stime$NVarVacc1[It]*Stime$Gamma[It]),list(Stime$wave[It]),sum)
colnames(a1)<-c("wave","NVarVacc","NVarVacc1","nWMA","dWMA","dWMA1")

#tolgo l'onda zero, messaper girni superiori a 736
a1<-a1[a1$wave!=0,]

#pesi delle onde per diagnosi e incidenza
a1$PernWMA<-a1$nWMA/sum(a1$nWMA)
a1$PerNVarVacc<-a1$NVarVacc/sum(a1$NVarVacc)

#lealità per onda. la calcolo con pesi delle diagnosi e dei casi giornalieri. 
#Come pesi uso incidenza che mi da restituisce un incidenza x onda pari alla somma delle incidenze gionaliere.
#a1$IFRVarVacn<-a1$"IFRVarVacn*nWMA"/a1$nWMA #letalità media pesata x onda con pesi delle diagnosi giornaliere
a1$IFRVarVacN<-a1$dWMA/a1$NVarVacc #letalità media pesata x onda con pesi dell'incidenza giornaliera. 
a1$IFRVarN<-a1$dWMA1/a1$NVarVacc1 #letalità media pesata x onda con pesi dell'incidenza giornaliera. 


#incidenza onde con iternalli di confidenza
a1$N<-a1$dWMA/a1$IFRVarVacN
a1$NLB<-a1$N
a1$NLB[2:5]<-qnbinom(0.975, a1$dWMA[!is.na(a1$IFRVarVacN)], a1$IFRVarVacN[!is.na(a1$IFRVarVacN)], lower.tail =FALSE, log.p = FALSE)+a1$dWMA[!is.na(a1$IFRVarVacN)]
a1$NUB<-a1$N
a1$NUB[2:5]<-qnbinom(0.975, a1$dWMA[!is.na(a1$IFRVarVacN)], a1$IFRVarVacN[!is.na(a1$IFRVarVacN)], lower.tail =TRUE , log.p = FALSE)+a1$dWMA[!is.na(a1$IFRVarVacN)]
a1$N1<-a1$dWMA1/a1$IFRVarN
a1$N1LB[2:5]<-qnbinom(0.975, a1$dWMA1[!is.na(a1$IFRVarN)], a1$IFRVarVacN[!is.na(a1$IFRVarN)], lower.tail =FALSE, log.p = FALSE)+a1$dWMA1[!is.na(a1$IFRVarVacN)]
a1$N1UB[2:5]<-qnbinom(0.975, a1$dWMA1[!is.na(a1$IFRVarN)], a1$IFRVarVacN[!is.na(a1$IFRVarN)], lower.tail =TRUE , log.p = FALSE)+a1$dWMA1[!is.na(a1$IFRVarVacN)]


#Durata onda Incidenza in gg e massimo
mxN<-aggregate(list(Stime$date1[It],Stime$NVarVacc[It]*Stime$Gamma[It]),list(Stime$wave[It]),max)
colnames(mxN)<-c("wave","GmaxN","maxN")
a1<-merge(a1,mxN)

#Durata onda decessi in gg e massimo
mxd<-aggregate(list(Stime$date1[It],Stime$NVarVacc[It]*Stime$Gamma[It]*Stime$IFRVarVacGamma[It]),list(Stime$wave[It]),max)
colnames(mxd)<-c("wave","Gmaxd","maxd")
a1<-merge(a1,mxd)


#aggiungo periodo x onda
D1W<-aggregate(Stime$date1[It & Stime$wave!=0],list(Stime$wave[It & Stime$wave!=0]),min) #inizio x onda in num gg
colnames(D1W)<-c("wave","Dmin")
D1W$Dmin<-Stime$date[Stime$date1 %in% D1W$Dmin & It]#trasformo num giorni in data
D2W<-aggregate(Stime$date1[It & Stime$wave!=0],list(Stime$wave[It & Stime$wave!=0]),max) #fine x onda in num gg
colnames(D2W)<-c("wave","Dmax")
D2W$Dmax<-Stime$date[Stime$date1 %in% D2W$Dmax & It]#trasformo num giorni in data
D1W<-merge(D1W,D2W) #unisco inizio e fine
D1W<-merge(D1W,a1) #unisco con tab a


# picchi incidenza e mortalià x onda
mxd<-aggregate(list(Stime$NVarVacc[It]*Stime$Gamma[It],Stime$NVarVacc[It]*Stime$Gamma[It]*Stime$IFRVarVacGamma[It]),list(Stime$wave[It]),max)
colnames(mxd)<-c("wave","Nmx","dWMAmx")
mxd<-mxd[mxd$wave!=0,]

# data picchi incidenza e mortalià x onda
mxd$GmaxN[mxd$N>0]<-as.character(Stime$date[round(Stime$NVarVacc*Stime$Gamma,4) %in% round(mxd$Nmx[mxd$Nmx>0],4) & It])
mxd$Gmaxd[mxd$N>0]<-as.character(Stime$date[round(Stime$NVarVacc*Stime$Gamma*Stime$IFRVarVacGamma,4) %in% round(mxd$dWMA[mxd$dWMA>0],4) & It])

D1W<-merge(D1W[,-c(19,21)],mxd[,-c(2:3)])  #unisco con tab D1W

#inserisco i test x onda
a1<-aggregate(list(Stime$nWMA[It]*Stime$Gamma[It],Stime$testWMA[It]*Stime$Gamma[It]),list(Stime$wave[It]),sum)
colnames(a1)<-c("wave","nWMA","testWMA")
D1W<-merge(D1W,a1[a1$wave!=0,])  #unisco con tab D1W

D1W<-D1W[,-c(9:10)]
D1W$VOC<-"cGamma"

Tab<-rbind(Tab,D1W)



#########Delta
#tabella con caratteristiche onde: decessi, incidenza (con IC), diagnosi e pesi decessi e incidenza per onda
a1<-aggregate(list(Stime$NVarVacc[It]*Stime$Delta[It],Stime$NVarVacc1[It]*Stime$Delta[It],Stime$nWMA[It]*Stime$Delta[It],Stime$IFRVarVacDelta[It]*Stime$NVarVacc[It]*Stime$Delta[It],Stime$IFRVarDelta[It]*Stime$NVarVacc1[It]*Stime$Delta[It]),list(Stime$wave[It]),sum)
colnames(a1)<-c("wave","NVarVacc","NVarVacc1","nWMA","dWMA","dWMA1")

#tolgo l'onda zero, messaper girni superiori a 736
a1<-a1[a1$wave!=0,]

#pesi delle onde per diagnosi e incidenza
a1$PernWMA<-a1$nWMA/sum(a1$nWMA)
a1$PerNVarVacc<-a1$NVarVacc/sum(a1$NVarVacc)

#lealità per onda. la calcolo con pesi delle diagnosi e dei casi giornalieri. 
#Come pesi uso incidenza che mi da restituisce un incidenza x onda pari alla somma delle incidenze gionaliere.
#a1$IFRVarVacn<-a1$"IFRVarVacn*nWMA"/a1$nWMA #letalità media pesata x onda con pesi delle diagnosi giornaliere
a1$IFRVarVacN<-a1$dWMA/a1$NVarVacc #letalità media pesata x onda con pesi dell'incidenza giornaliera. 
a1$IFRVarN<-a1$dWMA1/a1$NVarVacc1 #letalità media pesata x onda con pesi dell'incidenza giornaliera. 


#incidenza onde con iternalli di confidenza
a1$N<-a1$dWMA/a1$IFRVarVacN
a1$NLB<-a1$N
a1$NLB[2:5]<-qnbinom(0.975, a1$dWMA[!is.na(a1$IFRVarVacN)], a1$IFRVarVac[!is.na(a1$IFRVarVacN)], lower.tail =FALSE, log.p = FALSE)+a1$dWMA[!is.na(a1$IFRVarVacN)]
a1$NUB<-a1$N
a1$NUB[2:5]<-qnbinom(0.975, a1$dWMA[!is.na(a1$IFRVarVacN)], a1$IFRVarVac[!is.na(a1$IFRVarVacN)], lower.tail =TRUE , log.p = FALSE)+a1$dWMA[!is.na(a1$IFRVarVacN)]
a1$N1<-a1$dWMA1/a1$IFRVarN
a1$N1LB[2:5]<-qnbinom(0.975, a1$dWMA1[!is.na(a1$IFRVarN)], a1$IFRVarVacN[!is.na(a1$IFRVarN)], lower.tail =FALSE, log.p = FALSE)+a1$dWMA1[!is.na(a1$IFRVarVacN)]
a1$N1UB[2:5]<-qnbinom(0.975, a1$dWMA1[!is.na(a1$IFRVarN)], a1$IFRVarVacN[!is.na(a1$IFRVarN)], lower.tail =TRUE , log.p = FALSE)+a1$dWMA1[!is.na(a1$IFRVarVacN)]


#Durata onda Incidenza in gg e massimo
mxN<-aggregate(list(Stime$date1[It],Stime$NVarVacc[It]*Stime$Delta[It]),list(Stime$wave[It]),max)
colnames(mxN)<-c("wave","GmaxN","maxN")
a1<-merge(a1,mxN)

#Durata onda decessi in gg e massimo
mxd<-aggregate(list(Stime$date1[It],Stime$NVarVacc[It]*Stime$Delta[It]*Stime$IFRVarVacDelta[It]),list(Stime$wave[It]),max)
colnames(mxd)<-c("wave","Gmaxd","maxd")
a1<-merge(a1,mxd)


#aggiungo periodo x onda
D1W<-aggregate(Stime$date1[It & Stime$wave!=0],list(Stime$wave[It & Stime$wave!=0]),min) #inizio x onda in num gg
colnames(D1W)<-c("wave","Dmin")
D1W$Dmin<-Stime$date[Stime$date1 %in% D1W$Dmin & It]#trasformo num giorni in data
D2W<-aggregate(Stime$date1[It & Stime$wave!=0],list(Stime$wave[It & Stime$wave!=0]),max) #fine x onda in num gg
colnames(D2W)<-c("wave","Dmax")
D2W$Dmax<-Stime$date[Stime$date1 %in% D2W$Dmax & It]#trasformo num giorni in data
D1W<-merge(D1W,D2W) #unisco inizio e fine
D1W<-merge(D1W,a1) #unisco con tab a


# picchi incidenza e mortalià x onda
mxd<-aggregate(list(Stime$NVarVacc[It]*Stime$Delta[It],Stime$NVarVacc[It]*Stime$Delta[It]*Stime$IFRVarVacDelta[It]),list(Stime$wave[It]),max)
colnames(mxd)<-c("wave","Nmx","dWMAmx")
mxd<-mxd[mxd$wave!=0,]

# data picchi incidenza e mortalià x onda
mxd$GmaxN[mxd$N>0]<-as.character(Stime$date[round(Stime$NVarVacc*Stime$Delta,4) %in% round(mxd$Nmx[mxd$Nmx>0],4) & It])
mxd$Gmaxd[mxd$N>0]<-as.character(Stime$date[round(Stime$NVarVacc*Stime$Delta*Stime$IFRVarVacDelta,4) %in% round(mxd$dWMA[mxd$dWMA>0],4) & It])

D1W<-merge(D1W[,-c(19,21)],mxd[,-c(2:3)])  #unisco con tab D1W

#inserisco i test x onda
a1<-aggregate(list(Stime$nWMA[It]*Stime$Delta[It],Stime$testWMA[It]*Stime$Delta[It]),list(Stime$wave[It]),sum)
colnames(a1)<-c("wave","nWMA","testWMA")
D1W<-merge(D1W,a1[a1$wave!=0,])  #unisco con tab D1W

D1W<-D1W[,-c(9:10)]
D1W$VOC<-"Delta"

Tab<-rbind(Tab,D1W)



#########Omicron
#tabella con caratteristiche onde: decessi, incidenza (con IC), diagnosi e pesi decessi e incidenza per onda
a1<-aggregate(list(Stime$NVarVacc[It]*Stime$Omicron[It],Stime$NVarVacc1[It]*Stime$Omicron[It],Stime$nWMA[It]*Stime$Omicron[It],Stime$IFRVarVacOmicron[It]*Stime$NVarVacc[It]*Stime$Omicron[It],Stime$IFRVarOmicron[It]*Stime$NVarVacc1[It]*Stime$Omicron[It]),list(Stime$wave[It]),sum)
colnames(a1)<-c("wave","NVarVacc","NVarVacc1","nWMA","dWMA","dWMA1")

#tolgo l'onda zero, messaper girni superiori a 736
a1<-a1[a1$wave!=0,]

#pesi delle onde per diagnosi e incidenza
a1$PernWMA<-a1$nWMA/sum(a1$nWMA)
a1$PerNVarVacc<-a1$NVarVacc/sum(a1$NVarVacc)

#lealità per onda. la calcolo con pesi delle diagnosi e dei casi giornalieri. 
#Come pesi uso incidenza che mi da restituisce un incidenza x onda pari alla somma delle incidenze gionaliere.
#a1$IFRVarVacn<-a1$"IFRVarVacn*nWMA"/a1$nWMA #letalità media pesata x onda con pesi delle diagnosi giornaliere
a1$IFRVarVacN<-a1$dWMA/a1$NVarVacc #letalità media pesata x onda con pesi dell'incidenza giornaliera. 
a1$IFRVarN<-a1$dWMA1/a1$NVarVacc1 #letalità media pesata x onda con pesi dell'incidenza giornaliera. 


#incidenza onde con iternalli di confidenza
a1$N<-a1$dWMA/a1$IFRVarVacN
a1$NLB<-a1$N
a1$NLB[5]<-qnbinom(0.975, a1$dWMA[!is.na(a1$IFRVarVacN)], a1$IFRVarVac[!is.na(a1$IFRVarVacN)], lower.tail =FALSE, log.p = FALSE)+a1$dWMA[!is.na(a1$IFRVarVacN)]
a1$NUB<-a1$N
a1$NUB[5]<-qnbinom(0.975, a1$dWMA[!is.na(a1$IFRVarVacN)], a1$IFRVarVac[!is.na(a1$IFRVarVacN)], lower.tail =TRUE , log.p = FALSE)+a1$dWMA[!is.na(a1$IFRVarVacN)]
a1$N1<-a1$dWMA1/a1$IFRVarN
a1$N1LB[5]<-qnbinom(0.975, a1$dWMA1[!is.na(a1$IFRVarN)], a1$IFRVarVacN[!is.na(a1$IFRVarN)], lower.tail =FALSE, log.p = FALSE)+a1$dWMA1[!is.na(a1$IFRVarVacN)]
a1$N1UB[5]<-qnbinom(0.975, a1$dWMA1[!is.na(a1$IFRVarN)], a1$IFRVarVacN[!is.na(a1$IFRVarN)], lower.tail =TRUE , log.p = FALSE)+a1$dWMA1[!is.na(a1$IFRVarVacN)]

#Durata onda Incidenza in gg e massimo
mxN<-aggregate(list(Stime$date1[It],Stime$NVarVacc[It]*Stime$Omicron[It]),list(Stime$wave[It]),max)
colnames(mxN)<-c("wave","GmaxN","maxN")
a1<-merge(a1,mxN)

#Durata onda decessi in gg e massimo
mxd<-aggregate(list(Stime$date1[It],Stime$NVarVacc[It]*Stime$Omicron[It]*Stime$IFRVarVacOmicron[It]),list(Stime$wave[It]),max)
colnames(mxd)<-c("wave","Gmaxd","maxd")
a1<-merge(a1,mxd)


#aggiungo periodo x onda
D1W<-aggregate(Stime$date1[It & Stime$wave!=0],list(Stime$wave[It & Stime$wave!=0]),min) #inizio x onda in num gg
colnames(D1W)<-c("wave","Dmin")
D1W$Dmin<-Stime$date[Stime$date1 %in% D1W$Dmin & It]#trasformo num giorni in data
D2W<-aggregate(Stime$date1[It & Stime$wave!=0],list(Stime$wave[It & Stime$wave!=0]),max) #fine x onda in num gg
colnames(D2W)<-c("wave","Dmax")
D2W$Dmax<-Stime$date[Stime$date1 %in% D2W$Dmax & It]#trasformo num giorni in data
D1W<-merge(D1W,D2W) #unisco inizio e fine
D1W<-merge(D1W,a1) #unisco con tab a


# picchi incidenza e mortalià x onda
mxd<-aggregate(list(Stime$NVarVacc[It]*Stime$Omicron[It],Stime$NVarVacc[It]*Stime$Omicron[It]*Stime$IFRVarVacOmicron[It]),list(Stime$wave[It]),max)
colnames(mxd)<-c("wave","Nmx","dWMAmx")
mxd<-mxd[mxd$wave!=0,]

# data picchi incidenza e mortalià x onda
mxd$GmaxN[mxd$N>0]<-as.character(Stime$date[round(Stime$NVarVacc*Stime$Omicron,4) %in% round(mxd$Nmx[mxd$Nmx>0],4) & It])
mxd$Gmaxd[mxd$N>0]<-as.character(Stime$date[round(Stime$NVarVacc*Stime$Omicron*Stime$IFRVarVacOmicron,4) %in% round(mxd$dWMA[mxd$dWMA>0],4) & It])

D1W<-merge(D1W[,-c(19,21)],mxd[,-c(2:3)])  #unisco con tab D1W

#inserisco i test x onda
a1<-aggregate(list(Stime$nWMA[It]*Stime$Omicron[It],Stime$testWMA[It]*Stime$Omicron[It]),list(Stime$wave[It]),sum)
colnames(a1)<-c("wave","nWMA","testWMA")
D1W<-merge(D1W,a1[a1$wave!=0,])  #unisco con tab D1W

D1W<-D1W[,-c(9:10)]
D1W$VOC<-"Omicron"

Tab<-rbind(Tab,D1W)


#########NonVoc
#tabella con caratteristiche onde: decessi, incidenza (con IC), diagnosi e pesi decessi e incidenza per onda
a1<-aggregate(list(Stime$NVarVacc[It]*Stime$NonVoc[It],Stime$NVarVacc1[It]*Stime$NonVoc[It],Stime$nWMA[It]*Stime$NonVoc[It],Stime$IFRVarVacNonVoc[It]*Stime$NVarVacc[It]*Stime$NonVoc[It],Stime$IFRVarNonVoc[It]*Stime$NVarVacc1[It]*Stime$NonVoc[It]),list(Stime$wave[It]),sum)
colnames(a1)<-c("wave","NVarVacc","NVarVacc1","nWMA","dWMA","dWMA1")

#tolgo l'onda zero, messaper girni superiori a 736
a1<-a1[a1$wave!=0,]

#pesi delle onde per diagnosi e incidenza
a1$PernWMA<-a1$nWMA/sum(a1$nWMA)
a1$PerNVarVacc<-a1$NVarVacc/sum(a1$NVarVacc)

#lealità per onda. la calcolo con pesi delle diagnosi e dei casi giornalieri. 
#Come pesi uso incidenza che mi da restituisce un incidenza x onda pari alla somma delle incidenze gionaliere.
#a1$IFRVarVacn<-a1$"IFRVarVacn*nWMA"/a1$nWMA #letalità media pesata x onda con pesi delle diagnosi giornaliere
a1$IFRVarVacN<-a1$dWMA/a1$NVarVacc #letalità media pesata x onda con pesi dell'incidenza giornaliera. 
a1$IFRVarN<-a1$dWMA1/a1$NVarVacc1 #letalità media pesata x onda con pesi dell'incidenza giornaliera. 


#incidenza onde con iternalli di confidenza
a1$N<-a1$dWMA/a1$IFRVarVacN
a1$NLB<-a1$N
a1$NLB<-qnbinom(0.975, a1$dWMA[!is.na(a1$IFRVarVacN)], a1$IFRVarVac[!is.na(a1$IFRVarVacN)], lower.tail =FALSE, log.p = FALSE)+a1$dWMA[!is.na(a1$IFRVarVacN)]
a1$NUB<-a1$N
a1$NUB<-qnbinom(0.975, a1$dWMA[!is.na(a1$IFRVarVacN)], a1$IFRVarVac[!is.na(a1$IFRVarVacN)], lower.tail =TRUE , log.p = FALSE)+a1$dWMA[!is.na(a1$IFRVarVacN)]
a1$N1<-a1$dWMA1/a1$IFRVarN
a1$N1LB<-qnbinom(0.975, a1$dWMA1[!is.na(a1$IFRVarN)], a1$IFRVarVacN[!is.na(a1$IFRVarN)], lower.tail =FALSE, log.p = FALSE)+a1$dWMA1[!is.na(a1$IFRVarVacN)]
a1$N1UB<-qnbinom(0.975, a1$dWMA1[!is.na(a1$IFRVarN)], a1$IFRVarVacN[!is.na(a1$IFRVarN)], lower.tail =TRUE , log.p = FALSE)+a1$dWMA1[!is.na(a1$IFRVarVacN)]

#Durata onda Incidenza in gg e massimo
mxN<-aggregate(list(Stime$date1[It],Stime$NVarVacc[It]*Stime$NonVoc[It]),list(Stime$wave[It]),max)
colnames(mxN)<-c("wave","GmaxN","maxN")
a1<-merge(a1,mxN)

#Durata onda decessi in gg e massimo
mxd<-aggregate(list(Stime$date1[It],Stime$NVarVacc[It]*Stime$NonVoc[It]*Stime$IFRVarVacNonVoc[It]),list(Stime$wave[It]),max)
colnames(mxd)<-c("wave","Gmaxd","maxd")
a1<-merge(a1,mxd)


#aggiungo periodo x onda
D1W<-aggregate(Stime$date1[It & Stime$wave!=0],list(Stime$wave[It & Stime$wave!=0]),min) #inizio x onda in num gg
colnames(D1W)<-c("wave","Dmin")
D1W$Dmin<-Stime$date[Stime$date1 %in% D1W$Dmin & It]#trasformo num giorni in data
D2W<-aggregate(Stime$date1[It & Stime$wave!=0],list(Stime$wave[It & Stime$wave!=0]),max) #fine x onda in num gg
colnames(D2W)<-c("wave","Dmax")
D2W$Dmax<-Stime$date[Stime$date1 %in% D2W$Dmax & It]#trasformo num giorni in data
D1W<-merge(D1W,D2W) #unisco inizio e fine
D1W<-merge(D1W,a1) #unisco con tab a


# picchi incidenza e mortalià x onda
mxd<-aggregate(list(Stime$NVarVacc[It]*Stime$NonVoc[It],Stime$NVarVacc[It]*Stime$NonVoc[It]*Stime$IFRVarVacNonVoc[It]),list(Stime$wave[It]),max)
colnames(mxd)<-c("wave","Nmx","dWMAmx")
mxd<-mxd[mxd$wave!=0,]

# data picchi incidenza e mortalià x onda
mxd$GmaxN[mxd$N>0]<-as.character(Stime$date[round(Stime$NVarVacc*Stime$NonVoc,4) %in% round(mxd$Nmx[mxd$Nmx>0],4) & It])
mxd$Gmaxd[mxd$N>0]<-as.character(Stime$date[round(Stime$NVarVacc*Stime$NonVoc*Stime$IFRVarVacNonVoc,4) %in% round(mxd$dWMA[mxd$dWMA>0],4) & It])

D1W<-merge(D1W[,-c(19,21)],mxd[,-c(2:3)])  #unisco con tab D1W

#inserisco i test x onda
a1<-aggregate(list(Stime$nWMA[It]*Stime$NonVoc[It],Stime$testWMA[It]*Stime$NonVoc[It]),list(Stime$wave[It]),sum)
colnames(a1)<-c("wave","nWMA","testWMA")
D1W<-merge(D1W,a1[a1$wave!=0,])  #unisco con tab D1W

D1W<-D1W[,-c(9:10)]
D1W$VOC<-"wNonVoc"

Tab<-rbind(Tab,D1W)
Tab$N[is.na(Tab$N)]<-Tab$NLB[is.na(Tab$NLB)]<-Tab$NUB[is.na(Tab$NUB)]<-Tab$IFRVarVacN[is.na(Tab$NUB)]<-0 
Tab$N1[is.na(Tab$N1)]<-Tab$N1LB[is.na(Tab$N1LB)]<-Tab$N1UB[is.na(Tab$N1UB)]<-Tab$IFRVarN[is.na(Tab$N1UB)]<-0 


#metto 0 tutti i valoni nulli
Tab<-Tab[order(Tab$wave,Tab$VOC),]

#write.csv2(Tab,file="C:\\Users\\xf49652\\OneDrive - INAIL\\Pierpaolo\\COVID-19\\Second\\Tab22.csv")


#########totale x variante
#tabella con caratteristiche onde: decessi, incidenza (con IC), diagnosi e pesi decessi e incidenza per onda

#Creo tabella con totali x VOC
C1W<-rbind(D1W,D1W[D1W$wave>=4,])
C1W$wave<-"zAll"
C1W$VOC<-c("Alpha","Beta","cGamma","Delta","Omicron","wNonVoc","zAll")
C1W$Dmin<-"2020-02-24"
C1W$Dmax<-"2022-02-28"


#Creo var x pesi della media x lethality con vaccini
et<-Stime$NVarVacc[It]
eta<-et*Stime$Alfa[It]
etb<-et*Stime$Beta[It]
etg<-et*Stime$Gamma[It]
etd<-et*Stime$Delta[It]
etn<-et*Stime$NonVoc[It]
eto<-et*Stime$Omicron[It]

#lethality con vaccini
C1W$IFRVarVacN<-c(sum(eta*Stime$IFRVarVacAlfa[It]/sum(eta)),sum(etb*Stime$IFRVarVacBeta[It]/sum(etb)),sum(etg*Stime$IFRVarVacGamma[It]/sum(etg)),sum(etd*Stime$IFRVarVacDelta[It]/sum(etd)),sum(eto*Stime$IFRVarVacOmicron[It]/sum(eto)),sum(etn*Stime$IFRVarVacNonVoc[It]/sum(etn)),sum(et*Stime$IFRVarVac[It]/sum(et)))
#controllo
sum(Stime$IFRVarVacAlfa[It]*(Stime$Alfa[It])*Stime$NVarVacc[It])/sum((Stime$Alfa[It])*Stime$NVarVacc[It])
sum(Stime$IFRVarVacBeta[It]*(Stime$Beta[It])*Stime$NVarVacc[It])/sum((Stime$Beta[It])*Stime$NVarVacc[It])
sum(Stime$IFRVarVacGamma[It]*(Stime$Gamma[It])*Stime$NVarVacc[It])/sum((Stime$Gamma[It])*Stime$NVarVacc[It])
sum(Stime$IFRVarVacDelta[It]*(Stime$Delta[It])*Stime$NVarVacc[It])/sum((Stime$Delta[It])*Stime$NVarVacc[It])
sum(Stime$IFRVarVacOmicron[It]*(Stime$Omicron[It])*Stime$NVarVacc[It])/sum((Stime$Omicron[It])*Stime$NVarVacc[It])
sum(Stime$IFRVarVacNonVoc[It]*(Stime$NonVoc[It])*Stime$NVarVacc[It])/sum((Stime$NonVoc[It])*Stime$NVarVacc[It])
sum(Stime$IFRVarVac[It]*Stime$NVarVacc[It])/sum(Stime$NVarVacc[It])


#controllo
ss<-aggregate(list(Tab$IFRVarVacN*Tab$N,Tab$N),list(Tab$VOC),sum)
colnames(ss)<-c("VOC","IFRVarVacNP","P")
ss$IFRVarVacN<-ss$IFRVarVacNP/ss$P
sum(Tab$IFRVarVacN*Tab$N)/sum(Tab$N)


#Creo var x pesi della media lethality senza vaccini
et1<-Stime$NVarVacc1[It]
eta1<-et1*Stime$Alfa[It]
etb1<-et1*Stime$Beta[It]
etg1<-et1*Stime$Gamma[It]
etd1<-et1*Stime$Delta[It]
etn1<-et1*Stime$NonVoc[It]
eto1<-et1*Stime$Omicron[It]

#lethality senza vaccini
C1W$IFRVarN<-c(sum(eta1*Stime$IFRVarAlfa[It]/sum(eta1)),sum(etb1*Stime$IFRVarBeta[It]/sum(etb1)),sum(etg1*Stime$IFRVarGamma[It]/sum(etg1)),sum(etd1*Stime$IFRVarDelta[It]/sum(etd1)),sum(eto1*Stime$IFRVarOmicron[It]/sum(eto1)),sum(etn1*Stime$IFRVarNonVoc[It]/sum(etn1)),sum(et1*Stime$IFRVar[It]/sum(et1)))
#controllo
sum(Stime$IFRVarAlfa[It]*(Stime$Alfa[It])*Stime$NVarVacc1[It])/sum((Stime$Alfa[It])*Stime$NVarVacc1[It])
sum(Stime$IFRVarBeta[It]*(Stime$Beta[It])*Stime$NVarVacc1[It])/sum((Stime$Beta[It])*Stime$NVarVacc1[It])
sum(Stime$IFRVarGamma[It]*(Stime$Gamma[It])*Stime$NVarVacc1[It])/sum((Stime$Gamma[It])*Stime$NVarVacc1[It])
sum(Stime$IFRVarDelta[It]*(Stime$Delta[It])*Stime$NVarVacc1[It])/sum((Stime$Delta[It])*Stime$NVarVacc1[It])
sum(Stime$IFRVarOmicron[It]*(Stime$Omicron[It])*Stime$NVarVacc1[It])/sum((Stime$Omicron[It])*Stime$NVarVacc1[It])
sum(Stime$IFRVarNonVoc[It]*(Stime$NonVoc[It])*Stime$NVarVacc1[It])/sum((Stime$NonVoc[It])*Stime$NVarVacc1[It])
sum(Stime$IFRVar[It]*Stime$NVarVacc1[It])/sum(Stime$NVarVacc1[It])

#controllo
ss<-aggregate(list(Tab$IFRVarN*Tab$N1,Tab$N1),list(Tab$VOC),sum)
colnames(ss)<-c("VOC","IFRVarNP","P")
ss$IFRVarN<-ss$IFRVarNP/ss$P
sum(Tab$IFRVarN*Tab$N1)/sum(Tab$N1)

#decessi con vaccini
C1W$dWMA<-c(sum(eta*Stime$IFRVarVacAlfa[It]),sum(etb*Stime$IFRVarVacBeta[It]),sum(etg*Stime$IFRVarVacGamma[It]),sum(etd*Stime$IFRVarVacDelta[It]),sum(eto*Stime$IFRVarVacOmicron[It]),sum(etn*Stime$IFRVarVacNonVoc[It]),sum(et*Stime$IFRVarVac[It]))
C1W$dWMA[7]<-sum(C1W$dWMA[1:6])
sum(Stime$dWMA[It])
sum(Tab$dWMA[Tab$VOC!="zALL"])
aggregate(Tab$dWMA,list(Tab$VOC),sum)

#decessi senza vaccino
C1W$dWMA1<-c(sum(eta1*Stime$IFRVarAlfa[It]),sum(etb1*Stime$IFRVarBeta[It]),sum(etg1*Stime$IFRVarGamma[It]),sum(etd1*Stime$IFRVarDelta[It]),sum(eto1*Stime$IFRVarOmicron[It]),sum(etn1*Stime$IFRVarNonVoc[It]),sum(et1*Stime$IFRVar[It]))
C1W$dWMA1[7]<-sum(C1W$dWMA1[1:6])
sum(Stime$NVarVacc1[It]*Stime$IFRVar[It])
sum(Tab$dWMA1[Tab$VOC!="zALL"])
aggregate(Tab$dWMA1,list(Tab$VOC),sum)


#incidenza con 95% IC con vaccini
C1W$N<-c(sum(eta),sum(etb),sum(etg),sum(etd),sum(eto),sum(etn),sum(et))
C1W$N[7]<-sum(C1W$N[1:6])
sum(Stime$NVarVacc[It])
sum(Tab$N[Tab$VOC!="zALL"])
C1W$NLB<-qnbinom(0.975, C1W$dWMA, C1W$IFRVarVacN, lower.tail =FALSE, log.p = FALSE)+C1W$dWMA
C1W$NUB<-qnbinom(0.975, C1W$dWMA, C1W$IFRVarVacN, lower.tail =TRUE , log.p = FALSE)+C1W$dWMA
#controllo
aggregate(Tab$N,list(Tab$VOC),sum)


#incidenza con 95% IC senza vaccini
C1W$N1<-c(sum(eta1),sum(etb1),sum(etg1),sum(etd1),sum(eto1),sum(etn1),sum(et1))
C1W$N1[7]<-sum(C1W$N1[1:6])
sum(Stime$NVarVacc1[It])
sum(Tab$N1[Tab$VOC!="zALL"])
C1W$N1LB<-qnbinom(0.975, C1W$dWMA1, C1W$IFRVarN, lower.tail =FALSE, log.p = FALSE)+C1W$dWMA
C1W$N1UB<-qnbinom(0.975, C1W$dWMA1, C1W$IFRVarN, lower.tail =TRUE , log.p = FALSE)+C1W$dWMA
#controllo
aggregate(Tab$N1,list(Tab$VOC),sum)


#diagnosis
C1W$nWMA[1:6]<-c(sum(Stime$nWMA[It]*Stime$Alfa[It]),sum(Stime$nWMA[It]*Stime$Beta[It]),sum(Stime$nWMA[It]*Stime$Gamma[It]),sum(Stime$nWMA[It]*Stime$Delta[It]),sum(Stime$nWMA[It]*Stime$Omicron[It]),sum(Stime$nWMA[It]*Stime$NonVoc[It]))
C1W$nWMA[7]<-sum(C1W$nWMA[1:6])

#controllo
da<-sum(Stime$nWMA[It]*Stime$Alfa[It])
db<-sum(Stime$nWMA[It]*Stime$Beta[It])
dg<-sum(Stime$nWMA[It]*Stime$Gamma[It])
dd<-sum(Stime$nWMA[It]*Stime$Delta[It])
do<-sum(Stime$nWMA[It]*Stime$Omicron[It])
dnv<-sum(Stime$nWMA[It]*Stime$NonVoc[It])
da+db+dg+dd+do+dnv
sum(Stime$nWMA[It])
sum(Tab$nWMA)
aggregate(Tab$nWMA,list(Tab$VOC),sum)

#test
C1W$testWMA[1:6]<-c(sum(Stime$testWMA[It]*Stime$Alfa[It]),sum(Stime$testWMA[It]*Stime$Beta[It]),sum(Stime$testWMA[It]*Stime$Gamma[It]),sum(Stime$testWMA[It]*Stime$Delta[It]),sum(Stime$testWMA[It]*Stime$Omicron[It]),sum(Stime$testWMA[It]*Stime$NonVoc[It]))
C1W$testWMA[7]<-sum(C1W$testWMA[1:6])
#controllo
ta<-sum(Stime$testWMA[It]*(Stime$Alfa[It]))
tb<-sum(Stime$testWMA[It]*(Stime$Beta[It]))
tg<-sum(Stime$testWMA[It]*(Stime$Gamma[It]))
td<-sum(Stime$testWMA[It]*(Stime$Delta[It]))
to<-sum(Stime$testWMA[It]*(Stime$Omicron[It]))
tnv<-sum(Stime$testWMA[It]*(Stime$NonVoc[It]))
ta+tb+tg+td+to+tnv
aggregate(Tab$testWMA,list(Tab$VOC),sum)


C1W$nWMA/C1W$testWMA*100
C1W$nWMA/C1W$N*100


#max decessi con data
C1W$maxd<-c(max(eta*Stime$IFRVarVacAlfa[It]),max(etb*Stime$IFRVarVacBeta[It]),max(etg*Stime$IFRVarVacGamma[It]),max(etd*Stime$IFRVarVacDelta[It]),max(etn*Stime$IFRVarVacNonVoc[It]),max(eto*Stime$IFRVarVacOmicron[It]),max(et*Stime$IFRVarVac[It]))
C1W$maxd[7]<-max(C1W$maxd[1:6])
max(Stime$dWMA[It])

C1W$Gmaxd[1]<-as.character(Stime$date[Stime$NVarVacc*Stime$Alfa*Stime$IFRVarVacAlfa==max(Stime$NVarVacc*Stime$Alfa*Stime$IFRVarVacAlfa)])
C1W$Gmaxd[2]<-as.character(Stime$date[Stime$NVarVacc*Stime$Beta*Stime$IFRVarVacBeta==max(Stime$NVarVacc*Stime$Beta*Stime$IFRVarVacBeta)])
C1W$Gmaxd[3]<-as.character(Stime$date[Stime$NVarVacc*Stime$Gamma*Stime$IFRVarVacGamma==max(Stime$NVarVacc*Stime$Gamma*Stime$IFRVarVacGamma)])
C1W$Gmaxd[4]<-as.character(Stime$date[Stime$NVarVacc*Stime$Delta*Stime$IFRVarVacDelta==max(Stime$NVarVacc*Stime$Delta*Stime$IFRVarVacDelta)])
C1W$Gmaxd[5]<-as.character(unique(Stime$date[Stime$NVarVacc*Stime$NonVoc*Stime$IFRVarVacNonVoc==max(Stime$NVarVacc*Stime$NonVoc*Stime$IFRVarVacNonVoc)]))
C1W$Gmaxd[6]<-as.character(Stime$date[Stime$NVarVacc*Stime$Omicron*Stime$IFRVarVacOmicron==max(Stime$NVarVacc*Stime$Omicron*Stime$IFRVarVacOmicron)])
C1W$Gmaxd[7]<-C1W$Gmaxd[C1W$maxd[1:6]==max(C1W$maxd[1:6])]


##max incidenza con data
C1W$maxN<-c(max(eta),max(etb),max(etg),max(etd),max(etn),max(eto),max(et))
C1W$maxN[7]<-max(C1W$maxN[1:6])
max(c(eta,etb,etg,etd,eto,etn))
C1W$GmaxN[1]<-as.character(Stime$date[Stime$NVarVacc*Stime$Alfa==max(Stime$NVarVacc*Stime$Alfa)])
C1W$GmaxN[2]<-as.character(Stime$date[Stime$NVarVacc*Stime$Beta==max(Stime$NVarVacc*Stime$Beta)])
C1W$GmaxN[3]<-as.character(Stime$date[Stime$NVarVacc*Stime$Gamma==max(Stime$NVarVacc*Stime$Gamma)])
C1W$GmaxN[4]<-as.character(Stime$date[Stime$NVarVacc*Stime$Delta==max(Stime$NVarVacc*Stime$Delta)])
C1W$GmaxN[5]<-as.character(Stime$date[Stime$NVarVacc*Stime$NonVoc==max(Stime$NVarVacc*Stime$NonVoc)])
C1W$GmaxN[6]<-as.character(Stime$date[Stime$NVarVacc*Stime$Omicron==max(Stime$NVarVacc*Stime$Omicron)])
C1W$GmaxN[7]<-C1W$GmaxN[C1W$maxN[1:6]==max(C1W$maxN[1:6])]

Tab<-rbind(Tab,C1W)

write.csv(Tab,file=paste(your_data_folder,"Tab1.csv",sep=""))