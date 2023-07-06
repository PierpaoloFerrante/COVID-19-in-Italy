library(readxl)
library(spatstat)
#####################################################coding tables#####################################################
# Table Date
dates <- seq(as.Date("2020-02-24"), as.Date("2022-04-30"), by = "days")
TabDate <- data.frame(date = dates, IDDate = 1:length(dates))

# Table Age
age_intervals <- cut(0:101, breaks = c(seq(-1, 100, 5), Inf), labels = FALSE)
age_labels <- c(paste0((0:17) * 5, "-", (1:18) * 5 - 1),rep("90+",12))
Eta <- data.frame(Eta = 0:101, CEta = age_labels[age_intervals])
Eta$CEtaCod <- ifelse(Eta$Eta<90,age_intervals,19)
Eta$CEtaCod2 <- ifelse(Eta$Eta<90,ceiling(Eta$CEtaCod / 2),10)
Eta$CEta2 <- ifelse(Eta$Eta<90,paste0(floor(Eta$Eta / 10) * 10, "-", floor(Eta$Eta / 10) * 10 + 9),"90+")
Eta$CEtaCod3 <- ceiling(Eta$Eta / 5) * 5 + 2

rm(list=c("dates","age_intervals","age_labels"))


#####################################################Functions#####################################################
#n-days weighted moving average 
mySMA <- function (price,n,w){
  sma <- c()
  sma[1:(n-1)] <- NA
  weight<-w
  for (i in n:length(price)){
    sma[i]<-weighted.mean(price[(i-n+1):i],w)}
  #sma <- reclass(sma,price)
  return(sma)
}

#n-days moving average 
mySMA1 <- function (price,n){
  sma <- c()
  sma[1:(n-1)] <- NA
  for (i in n:length(price)){
    sma[i]<-mean(price[(i-n+1):i])}
  #sma <- reclass(sma,price)
  return(sma)
}

#n-days moving sum
mySMA2 <- function (price,n){
  sma <- c()
  sma[1:(n-1)] <- NA
  for (i in n:length(price)){
    sma[i]<-sum(price[(i-n+1):i])}
  #sma <- reclass(sma,price)
  return(sma)
}

#7-days moving average 
mySMA7 <- function (price){
  sma <- c()
  sma[1:6] <- NA
  for (i in 7:length(price)){
    sma[i]<-mean(price[(i-7+1):i])}
  #sma <- reclass(sma,price)
  return(sma)
}

###############################################################Weights for deaths and diagnoses in weighted moving average########################### 
#deaths: periodo 1 March/2020-Feb/2021 (Median = 12, Q1 = 7, Q3 = 19, QQ1 = 5 + 5 gg x sintomi),
mud1=18
Qd11<-12
Qd13<-24
IQRd1<-Qd13-Qd11
sigmad1=IQRd1/1.34896
#varianza normale troncata: sigmad01*(1-c1-d1)
facd1<-pnorm(2*mud1+1,mean=mud1,sd=sigmad1)-pnorm(0,mean=mud1,sd=sigmad1)
wed1<-rep(NA,2*mud1+1)
for (i in 1:(2*mud1+1)){
  wed1[i]<-(pnorm(i,mean=mud1,sd=sigmad1)-pnorm(i-1,mean=mud1,sd=sigmad1))/facd1}
sum(wed1)
length(wed1)

#deaths: periodo 2 March/2021-Oct/2021 (Median = 16, Q1 = 10, Q3 = 22, QQ1 = 5 + 5 gg x sintomi),
mud2=21
Qd21<-15
Qd23<-27
IQRd2<-Qd23-Qd21
sigmad2=IQRd2/1.34896
#varianza normale troncata: sigmad2=sigmad02*(1-c2-d2)
facd2<-pnorm(2*mud2+1,mean=mud2,sd=sigmad2)-pnorm(0,mean=mud2,sd=sigmad2)
wed2<-rep(NA,2*mud2+1)
for (i in 1:(2*mud2+1)){
  wed2[i]<-(pnorm(i,mean=mud2,sd=sigmad2)-pnorm(i-1,mean=mud2,sd=sigmad2))/facd2}
sum(wed2)
length(wed2)

#diagnoses: periodo 1 March-May/2020 (Median = 5, Qn1 = 2, Q3 = 9, QQn1 = 1 + 5gg x sintomi),
mun1=10.5
Qn11<-7
Qn13<-14
IQRn1<-Qn13-Qn11
sigman1=IQRn1/1.34896
#varianza normale troncata: sigman01*(1-cn1-dn1)
facn1<-pnorm(2*mun1+1,mean=mun1,sd=sigman1)-pnorm(0,mean=mun1,sd=sigman1)
wen1<-rep(NA,2*mun1+1)
for (i in 1:(2*mun1+1)){
  wen1[i]<-(pnorm(i,mean=mun1,sd=sigman1)-pnorm(i-1,mean=mun1,sd=sigman1))/facn1}
sum(wen1)
length(wen1)

#diagnoses: periodo 2 June-September/2020 (Median = 3, Q1 = 1, Q3 = 7, QQ1 = 0, QQ1= 0 + 5gg x sintomi, Median1=3.5)
mun2=8.5
Qn21<-5
Qn23<-12
IQRn2<-Qn23-Qn21
sigman2=IQRn2/1.34896
#varianza normale troncata: sigman02*(1-cn2-dn2)
facn2<-pnorm(2*mun2+1,mean=mun2,sd=sigman2)-pnorm(0,mean=mun2,sd=sigman2)
wen2<-rep(NA,2*mun2+1)
for (i in 1:(2*mun2+1)){
  wen2[i]<-(pnorm(i,mean=mun2,sd=sigman2)-pnorm(i-1,mean=mun2,sd=sigman2))/facn2}
sum(wen2)
length(wen2)

#diagnoses: periodo 3 October/2020-Feb/2021 (Median = 3, Q1=1, Q3=6, QQ1 = 0 + 5gg x sintomi).
mun3=8
Qn31<-5
Qn33<-11
IQRn3<-Qn33-Qn31
sigman3=IQRn3/1.34896
#varianza normale troncata: sigman03*(1-cn3-dn3)
facn3<-pnorm(2*mun3+1,mean=mun3,sd=sigman3)-pnorm(0,mean=mun3,sd=sigman3)
wen3<-rep(NA,2*mun3+1)
for (i in 1:(2*mun3+1)){
  wen3[i]<-(pnorm(i,mean=mun3,sd=sigman3)-pnorm(i-1,mean=mun3,sd=sigman3))/facn3}
sum(wen3)
length(wen3)

#diagnoses: periodo 4 Feb-Marc/2021 (Median = 3, Q1=1, Q3=6, QQ1 = 0 + 5gg x sintomi).
mun4=6
Qn41<-3
Qn43<-9
IQRn4<-Qn43-Qn41
sigman4=IQRn4/1.34896
#varianza normale troncata: sigman04*(1-cn4-dn4)
facn4<-pnorm(2*mun4+1,mean=mun4,sd=sigman4)-pnorm(0,mean=mun4,sd=sigman4)
wen4<-rep(NA,2*mun4+1)
for (i in 1:(2*mun4+1)){
  wen4[i]<-(pnorm(i,mean=mun4,sd=sigman4)-pnorm(i-1,mean=mun4,sd=sigman4))/facn4}
sum(wen4)
length(wen4)

rm(list=c("facd1","facd2","facn1","facn2","facn3","facn4","i","IQRd1","IQRd2","IQRn1","IQRn2","IQRn3","IQRn4","Qd11","Qd13","Qd21","Qd23","Qn11","Qn13","Qn21","Qn23","Qn31","Qn33","Qn41","Qn43","sigmad1","sigmad2","sigman1","sigman2","sigman3","sigman4"))


#####################################################Data Import############################################################################
#local paths to needed datafiles
your_data_folder<-"C:\\your\\local\\folder\\"

# Import ISTAT Italian Population 2020-2023 (you can do it directly with the system function)
Pop <- read.csv(paste(your_data_folder,"regioni.csv",sep=""))
Pop<-Pop[Pop$ITTER107=="IT",]
Pop<-Pop[Pop$ETA!="TOTAL",c(8,4,3,9)]
colnames(Pop)<-c("anno","sdmx.reg","eta","Pop")
Pop$Eta<-as.numeric(ifelse(nchar(Pop$eta)==2,substring(Pop$eta,2,2),ifelse(nchar(Pop$eta)==3,substring(Pop$eta,2,3),substring(Pop$eta,5,7))))
Pop<-merge(Pop,Eta)
Pop<-Pop[,c(2,1,6:10,5)]

#Add the median age by region-year
x<-as.data.frame(matrix(NA,nrow=3,ncol=2),byrow=TRUE)
colnames(x)<-c("anno","etaMediana")
for (k in 0:2){
    x$anno[k+1]<-2020+k
    x$etaMediana[k+1]<-ceiling(weighted.median(Pop$Eta[Pop$anno==2020+k],Pop$Pop[Pop$anno==2020+k]))
}
Pop<-merge(Pop,x)
x<-NULL
Pop<-Pop[order(Pop$anno,Pop$Eta),]


#Import weekly Eta median age of infection (from report ISS)
EtaMed<-read_xlsx(paste(your_data_folder,"IFR.xlsx",sep=""),sheet="EtaMed")
EtaMed$dateMed<-as.Date(EtaMed$dateMed)
#traslo le date del periodo medio di tempo tra contagio e diagnosi e agguingo 7 gg. passando da inizio a fine settimana
EtaMed$date<-EtaMed$dateMed
EtaMed$date[EtaMed$dateMed<"2020-06-01"]<-EtaMed$dateMed[EtaMed$dateMed<"2020-06-01"]-11+7
EtaMed$date[EtaMed$dateMed>"2020-05-30" & EtaMed$dateMed<"2020-10-01"]<-EtaMed$dateMed[EtaMed$dateMed>"2020-05-30" & EtaMed$dateMed<"2020-10-01"]-9+7
EtaMed$date[EtaMed$dateMed>"2020-09-30" & EtaMed$dateMed<"2021-03-01"]<-EtaMed$dateMed[EtaMed$dateMed>"2020-09-30" & EtaMed$dateMed<"2021-03-01"]-8+7
EtaMed$date[EtaMed$dateMed>"2021-02-28"]<-EtaMed$dateMed[EtaMed$dateMed>"2021-02-28"]-6+7
#Nei 4 punti sopra il salto tra una nuova data e l'altra non è 7
EtaMed$lagmed<-0
for (j in 1:(nrow(EtaMed)-1)){
  EtaMed$lagmed[j]<-as.numeric(EtaMed$date[j+1]-EtaMed$date[j])
}

# Import data as dataframes
IFR  <- readxl::read_xlsx(paste(your_data_folder,"IFR.xlsx",sep=""), sheet = "Foglio2")
hVar <- readxl::read_xlsx(paste(your_data_folder,"IFR.xlsx",sep=""), sheet = "Hazards")
NISTAT<-readxl::read_xlsx(paste(your_data_folder,"IFR.xlsx",sep=""),sheet="NISTAT")

#vaccini<-read.csv2("https://raw.githubusercontent.com//italia//covid19-opendata-vaccini//master//dati//somministrazioni-vaccini-latest-2020.csv",sep=",")
Vaccini<-rbind(read.csv2(paste(your_data_folder,"Vaccini2020.csv",sep=""),sep=","),read.csv2(paste(your_data_folder,"Vaccini2021.csv",sep=""),sep=","),read.csv2(paste(your_data_folder,"Vaccini2022.csv",sep=""),sep=","))
VarP<-read.csv2(paste(your_data_folder,"VarDef.csv",sep=""),sep=",") #imported from https://www.ecdc.europa.eu/en/publications-data/data-virus-variants-covid-19-eueea
Covid19all<-read.csv2(paste(your_data_folder,"dpc-covid19-ita-regioni.csv",sep=""),sep=",")
RischiVacc<-read.csv2(paste(your_data_folder,"RischiVacc.csv",sep=""),sep=",", stringsAsFactors = FALSE, dec = ".") #Data from ISS reports
#Data from ISS reports

#nomino colonne e tolgo quelle non usate
colnames(Covid19all)[1]<-"data"
Covid19all$data<-as.Date(Covid19all$data)
Covid19all<-Covid19all[Covid19all$data<"2022/04/16",]
Covid19all<-Covid19all[,-c(2:3,5:6)]
colnames(Covid19all)<-c("date","region","standard hosp","Intensive Care","tot hosp","quarantine","active positive","delta active","new active positive","disharged","deaths","cases1","cases2","cases","tests","tested","note","ingressi_terapia_intensiva","note_test","note_casi","totale_positivi_test_molecolare","totale_positivi_test_antigenico_rapido","tamponi_test_molecolare","tamponi_test_antigenico_rapido","codice_nuts_1","codice_nuts_2")

#add daily counts
library(dplyr)
Covid19all <- Covid19all %>%
  group_by(Covid19all$region) %>%
  mutate(
    dailydeaths = c(deaths[1], diff(deaths)),
    dailytests = c(tests[1], diff(tests)),
    dailytested = c(tested[1], diff(tested))
  )
Covid19all<-Covid19all[,c("date","region","active positive","new active positive","disharged","deaths","cases1","cases2","cases","tests","tested","dailytests","dailytested","dailydeaths")]
Covid19all$CFR<-Covid19all$deaths/Covid19all$cases

#creo date1 con format numerico della data
Covid19all$date1<-as.numeric(Covid19all$date)-18315
Covid19all$Set<-trunc(Covid19all$date1/7)

#Inserisco numero di settimane
Covid19all$Set<-trunc((Covid19all$date1-1)/7)+1

#Inserisco il giorno della settimana
Covid19all$GSet<-weekdays(Covid19all$date)

#Inserisco la % test positivi
Covid19all$nts<-Covid19all$"new active positive"/Covid19all$tests

Covid19all<-Covid19all[,c("date","date1","region","Set","GSet","active positive","new active positive","dailydeaths","dailytests","dailytested")]
colnames(Covid19all)[c(6:10)]<-c("p","nPC","dPC","test","tested")

#ordino dataset per data (numerico) e regione ed inserisco IDregione per giorno e IDGiorno x regione  (tarmite data.table)
Covid19all<-Covid19all[order(Covid19all$date1,Covid19all$region),]

library(data.table)
Covid19all<-data.table(Covid19all)
Covid19all[,IDDate:=1:.N,by="region"]

rm(list=c("j","k","x"))


#####################################################Correct deaths records##################################################################
#154 deaths recorded in Parma in August occurred betweem March and May

#period marzo-maggio
PerParma<-(substring(Covid19all$date,6,7)=="03" | substring(Covid19all$date,6,7)=="04" | substring(Covid19all$date,6,7)=="05") & substring(Covid19all$date,1,4)=="2020" 

#data
c1<-Covid19all[Covid19all$region=="Emilia-Romagna" & PerParma,c("date","dPC")]
pesi<-round(tapply(c1$dPC,substring(c1$date,6,7),sum)/sum(tapply(c1$dPC,substring(c1$date,6,7),sum)),2)
sum(c1$d)
sum(Covid19all$dPC[PerParma & Covid19all$region=="Emilia-Romagna"])

#deaths per monh (62,71,21)
154*pesi

#condition region= Emilia-Romagna and month=March

EM<-Covid19all$region=="Emilia-Romagna"

M03<-substring(Covid19all$date,6,7)=="03"

Covid19all$dPC[EM & M03]<-Covid19all$dPC[EM & M03]+62/31

M04<-substring(Covid19all$date,6,7)=="04"
Covid19all$dPC[EM & M04]<-Covid19all$dPC[EM & M04]+71/30

M05<-substring(Covid19all$date,6,7)=="05"
Covid19all$dPC[EM & M05]<-Covid19all$dPC[EM & M05]+21/31

sum(Covid19all$dPC[PerParma & Covid19all$region=="Emilia-Romagna"])

#Delete 154 deaths on 2020/08/15 in Emeilia Romagna
Covid19all$dPC[Covid19all$date=="2020-08-15" & Covid19all$region=="Emilia-Romagna"]<-Covid19all$dPC[Covid19all$date=="2020-08-15" & Covid19all$region=="Emilia-Romagna"]-154

rm(list=c("c1","EM","PerParma","M03","M04","M05","pesi"))


#on 24/06/2020 there are -61 deaths in P.A. Trento del
#1: Remove 2 deaths per day between  2020/03/21 and 2020/04/19 (included)
sum(!is.na(Covid19all$dPC[(Covid19all$region=="P.A. Trento" | Covid19all$region=="TA") & Covid19all$IDDate>26 & Covid19all$IDDate<58]))
sum(Covid19all$dPC[(Covid19all$region=="P.A. Trento" | Covid19all$region=="TA") & Covid19all$IDDate>26 & Covid19all$IDDate<58])
Covid19all$dPC[(Covid19all$region=="P.A. Trento" | Covid19all$region=="TA") & Covid19all$IDDate>26 & Covid19all$IDDate<57]<-Covid19all$dPC[Covid19all$region=="P.A. Trento" & Covid19all$IDDate>26 & Covid19all$IDDate<57]-2

#2: Remove tolgo 1 death on 2020/04/20
Covid19all$dPC[(Covid19all$region=="P.A. Trento" | Covid19all$region=="TA") & Covid19all$IDDate==57]<-Covid19all$dPC[Covid19all$region=="P.A. Trento" & Covid19all$IDDate==57]-1

#check 
sum(Covid19all$dPC[(Covid19all$region=="P.A. Trento" | Covid19all$region=="TA") & Covid19all$IDDate>26 & Covid19all$IDDate<58])
353-61

#azzero i -61 morti del 24/06/2020
Covid19all$dPC[(Covid19all$region=="P.A. Trento" | Covid19all$region=="TA") & Covid19all$IDDate==122]<-0
###############################Finestorno -61 decessi per P.A. Trento del 24/06/2020###############################

############################################COVID-19 data: From regional to National data########################################################
#sum with respect to regions
Covid19all<-aggregate(list(Covid19all$p,Covid19all$nPC,Covid19all$dPC,Covid19all$test,Covid19all$tested),list(Covid19all$date,Covid19all$date1,Covid19all$Set,Covid19all$GSet,Covid19all$IDDate),sum)
colnames(Covid19all)<-c("date","date1","Set","GSet","IDDate","p","nPC","dPC","test","tested")

#add year
Covid19all$anno<-as.numeric(substring(Covid19all$date,1,4))

#add median age
Covid19all<-merge(Covid19all,EtaMed,all.x=TRUE)

#7-gg moving average of test (primi sei giorni nulli)
Covid19all$TestSett<-mySMA7(Covid19all$test)

#1/6-gg moving average of test for first six days
for (i in 1:6)
  {
    Covid19all$TestSett[Covid19all$date1==i]<-mySMA1(Covid19all$test[Covid19all$date1<=i],i)[i]
  }
rm(list=c("i"))
Covid19all$date1[Covid19all$date=="2022-04-15"]

###########################################Fine aggiungo il totale=Italia dopo correzioni########################################################
#import ISS deaths (detected on 2022/04/15)
#DIDD<-openxlsx::read.xlsx("https://github.com/floatingpurr/covid-19_sorveglianza_integrata_italia/raw/main/data/2022-04-15/data.xlsx", sheet = "decessi")
DIDD<-openxlsx::read.xlsx(paste(your_data_folder,"2022-04-15.xlsx",sep=""),sheet = "decessi")

colnames(DIDD)[2:3]<-c("date","d")
DIDD<-DIDD[!is.na(DIDD$date),-1]
DIDD$d[DIDD$d=="<5"]<-"4"
DIDD$d<-as.numeric(DIDD$d)
DIDD<-DIDD[as.numeric(rownames(DIDD))>4,]

#import ISS confirmed cases (detected on 2022/04/15)
#CIDD<-openxlsx::read.xlsx("https://github.com/floatingpurr/covid-19_sorveglianza_integrata_italia/raw/main/data/2022-04-15/data.xlsx", sheet = "casi_prelievo_diagnosi")
CIDD<-openxlsx::read.xlsx(paste(your_data_folder,"2022-04-15.xlsx",sep=""),sheet = "casi_prelievo_diagnosi")

colnames(CIDD)[2:3]<-c("date","n")
CIDD<-CIDD[!is.na(CIDD$date),-1]
CIDD$n[CIDD$n=="<5"]<-"4"
CIDD$n<-as.numeric(CIDD$n)
CIDD<-CIDD[as.numeric(rownames(CIDD))>6,]

#merge deaths e confirmed cases 
DCISS<-merge(DIDD,CIDD,all.y=TRUE)
DCISS$date<-as.Date(paste(substring(DCISS$date,7,10),"-",substring(DCISS$date,4,5),"-",substring(DCISS$date,1,2),sep=""))
DCISS$date1<-as.numeric(DCISS$date)-18315
DCISS$d[is.na(DCISS$d)]<-0

#DCISS with dataframe Covid19
Covid19all<-merge(Covid19all,DCISS[,-1],by.x="date1",by.y="date1",all.x=TRUE)
Covid19all<-Covid19all[!is.na(Covid19all$n),]
rm(list=c("CIDD","DIDD","DCISS"))


############################################Deaths, condirmed cases and test moving average#############################################################
Inizio<-min(Covid19all$date1)
bound1<-Covid19all$date1[Covid19all$date=="2020-05-31"]   #To May
bound2<-Covid19all$date1[Covid19all$date=="2020-09-30"]   #June-September
bound3<-Covid19all$date1[Covid19all$date=="2021-02-07"]   #October/2020-Feb/2021 
Fine<-max(Covid19all$date1)

#########confirmed cases
  MAN1<-mySMA(Covid19all$n[Covid19all$date1>=Inizio & Covid19all$date1<=bound1+2*mun1],2*mun1+1,wen1)
  MAN1<-MAN1[!is.na(MAN1)]
  length(MAN1)
  Covid19all$nWMA[Covid19all$date1<=bound1]<-MAN1
  length(Covid19all$nWMA[Covid19all$date1<=bound1])
  
  MAN2<-mySMA(Covid19all$n[Covid19all$date1>bound1 & Covid19all$date1<=bound2+2*mun2],2*mun2+1,wen2)
  MAN2<-MAN2[!is.na(MAN2)]  
  length(MAN2)
  length(Covid19all$nWMA[Covid19all$date1>bound1 & Covid19all$date1<=bound2])
  Covid19all$nWMA[Covid19all$date1>bound1 & Covid19all$date1<=bound2]<-MAN2
  MAN3<-mySMA(Covid19all$n[Covid19all$date1>bound2 & Covid19all$date1<=bound3+2*mun3],2*mun3+1,wen3)
  MAN3<-MAN3[!is.na(MAN3)]  
  length(MAN3)
  Covid19all$nWMA[Covid19all$date1>bound2 & Covid19all$date1<=bound3]<-MAN3
  length(Covid19all$nWMA[Covid19all$date1>bound2 & Covid19all$date1<=bound3])
  MAN4<-mySMA(Covid19all$n[Covid19all$date1>bound3],2*mun4+1,wen4)
  MAN4<-MAN4[!is.na(MAN4)]  
  length(MAN4)
  Covid19all$nWMA[Covid19all$date1>bound3 & Covid19all$date1<=Fine-2*mun4]<-MAN4
  length(Covid19all$nWMA[Covid19all$date1>bound3 & Covid19all$date1<=Fine-2*mun4])

#########deaths 
  MAD1<-mySMA(Covid19all$d[Covid19all$date1>=Inizio & Covid19all$date1<=bound3+2*mud1],2*mud1+1,wed1)
  MAD1<-MAD1[!is.na(MAD1)]
  length(MAD1)
  Covid19all$dWMA[Covid19all$date1<=bound3]<-MAD1
  length(Covid19all$dWMA[Covid19all$date1<=bound3])
  length(Covid19all$dWMA[is.na(Covid19all$dWMA)])
  sum(Covid19all$d[(nrow(Covid19all)-2*mud1):nrow(Covid19all)]*wed1)
  MAD2<-mySMA(Covid19all$d[Covid19all$date1>bound3],2*mud2+1,wed2)
  MAD2<-MAD2[!is.na(MAD2)]
  length(MAD2)
  Covid19all$dWMA[Covid19all$date1>bound3 & Covid19all$date1<=Fine-2*mud2]<-MAD2
  length(Covid19all$dWMA[Covid19all$date1>bound3 & Covid19all$date1<=Fine-2*mud2])

#########test
  MAT1<-mySMA(Covid19all$test[Covid19all$date1>=Inizio & Covid19all$date1<=bound1+2*mun1],2*mun1+1,wen1)
  MAT1<-MAT1[!is.na(MAT1)]
  Covid19all$testWMA[Covid19all$date1<=bound1]<-MAT1
  length(MAT1)
  length(Covid19all$testWMA[Covid19all$date1<=bound1 & Covid19all$date1<=bound2+2*mun2])
  
  MAT2<-mySMA(Covid19all$test[Covid19all$date1>bound1 & Covid19all$date1<=bound2+2*mun2],2*mun2+1,wen2)
  MAT2<-MAT2[!is.na(MAT2)]  
  length(MAT2)
  Covid19all$testWMA[Covid19all$date1>bound1 & Covid19all$date1<=bound2]<-MAT2
  length(Covid19all$testWMA[Covid19all$date1>bound1 & Covid19all$date1<=bound2])
  
  MAT3<-mySMA(Covid19all$test[Covid19all$date1>bound2 & Covid19all$date1<=bound3+2*mun3],2*mun3+1,wen3)
  MAT3<-MAT3[!is.na(MAT3)]  
  length(MAT3)
  Covid19all$testWMA[Covid19all$date1>bound2 & Covid19all$date1<=bound3]<-MAT3
  length(Covid19all$testWMA[Covid19all$date1>bound2 & Covid19all$date1<=bound3])
  
  MAT4<-mySMA(Covid19all$test[Covid19all$date1>bound3],2*mun4+1,wen4)
  MAT4<-MAT4[!is.na(MAT4)]  
  length(MAT4)
  Covid19all$testWMA[Covid19all$date1>bound3 & Covid19all$date1<=Fine-2*mun4]<-MAT4
  length(Covid19all$testWMA[Covid19all$date1>bound3 & Covid19all$date1<=Fine-2*mun4])

  rm(list=c("bound1","bound2","bound3","Fine","Inizio","MAD1","MAD1","MAD2","MAN1","MAN2","MAN3","MAN4","MAT1","MAT2","MAT3","MAT4"))
  
  
  ##################################################### Fictitious population ####################################################
  #calculate the two ratios: population by age above population in 0 and 100 years
  Pop1<-Pop
  colnames(Pop1)[9]<-"etamedp"
  Pop1$chiave = 1:nrow(Pop1)

  #estraggo i valori età 0 e età 100 e creo due tabelle 
  tabella_eta_0 = Pop1[Pop1$Eta==0,c("anno","Pop")]
  names(tabella_eta_0)[2] = "popolazione_0"
  tabella_eta_100 = Pop1[Pop1$Eta==100,c("anno","Pop")]
  names(tabella_eta_100)[2] = "popolazione_100"
  #faccio la join con la tabella popolazione per dare assegnare ad ogni elemento il corrispondente "popolazione con età 0" e "popolazione con età 100"
  righe = nrow(Pop1)
  Pop1 = merge(x = Pop1, y = tabella_eta_0, by.x = "anno", by.y = "anno",all.x = TRUE)
  Pop1 = merge(x = Pop1, y = tabella_eta_100, by.x = "anno", by.y = "anno",all.x = TRUE)
  #controllo che il numero di righe sia rimasto uguale e che non si siano NA nelle colonne aggiunte, ovvero che la join sia stata fatta 1:1
  if (nrow(Pop1)==righe&all(!is.na(Pop1$popolazione_0))&all(!is.na(Pop1$popolazione_100))) {
    rm(righe,tabella_eta_0,tabella_eta_100)
  } else {
    warning('Errore nella join: non è stata fatta 1:1. Controllare.')
  }
  #creo le colonne a e b e rimuovo le colonne aggiunte, non più necessarie
  Pop1$a = Pop1$Pop/Pop1$popolazione_0
  Pop1$b = Pop1$Pop/Pop1$popolazione_100
  Pop1 = Pop1[!names(Pop1) %in% c("popolazione_0","popolazione_100")]

  
#calculate fictitious populations with median age greater or lesser than 1,2,...,26 years compared to the original population
  for (m in 0:26) {
    #assign a group to each element based on conditions Eta<etamedp-m, Eta>=etamedp-m, Eta<etamedp+m, Eta>=etamedp+m; (2 dicotomicous variables/columns)
    Pop1$G1 = ifelse(Pop1$Eta<=(Pop1$etamedp-m),0,1)
    Pop1$G2 = ifelse(Pop1$Eta<=(Pop1$etamedp+m),0,1)
    #sum of a e b per  group
    temp1 = aggregate(x = Pop1[,c("a","b")],by = Pop1[,c("anno","G1")],FUN = "sum")
    temp2 = aggregate(x = Pop1[,c("a","b")],by = Pop1[,c("anno","G2")],FUN = "sum")
    names(temp1)[3] = "sum_a_1"
    names(temp1)[4] = "sum_b_1"
    names(temp2)[3] = "sum_a_2"
    names(temp2)[4] = "sum_b_2"
    #associate sums based on groups
    Pop1 = merge(x = Pop1, y = temp1, by.x = c("anno","G1"), by.y = c("anno","G1"),all.x = TRUE)
    Pop1 = merge(x = Pop1, y = temp2, by.x = c("anno","G2"), by.y = c("anno","G2"),all.x = TRUE)
    #asses values associate to m
    x = Pop1[Pop1$G1==0,]
    y = Pop1[Pop1$G1==1,]
    x[,ncol(x)+1] = 0.5/x$sum_a_1*x$a
    y[,ncol(y)+1] = 0.5/y$sum_b_1*y$b
    z1 = rbind(x,y)
    z1 = z1[order(z1$chiave),]
    
    x = Pop1[Pop1$G2==0,]
    y = Pop1[Pop1$G2==1,]
    x[,ncol(x)+1] = 0.5/x$sum_a_2*x$a
    y[,ncol(y)+1] = 0.5/y$sum_b_2*y$b
    z2 = rbind(x,y)
    z2 = z2[order(z2$chiave),]
    #add new columns
    Pop1 = Pop1[order(Pop1$chiave),]
    Pop1[,ncol(Pop1)+1] = z1[,ncol(z1)]
    Pop1[,ncol(Pop1)+1] = z2[,ncol(z2)]
    #delete colums that will be recreated in next iteration
    Pop1 = Pop1[!names(Pop1) %in% c("sum_a_1","sum_b_1","sum_a_2","sum_b_2","G1","G2")]
    colnames(Pop1)[(ncol(Pop1)-1):(ncol(Pop1))]<-c(paste("Med-",m,sep=""),paste("Med+",m,sep=""))
  }
  rm(y,z1,z2,temp1,temp2)
  
  #rowbind
  x<-Pop1[,c(1:13,15)]
  colnames(x)[14]<-"DPop"
  x$diff<-0
  colnames(Pop1[,c(15:66)])
  for (j in 15:66)
  {
    y<-Pop1[,c(1:13,j)]
    colnames(y)[14]<-"DPop"
    y$diff<-ifelse(j%%2==0,(j-14)/2,-ceiling((j-14)/2))
    x<-rbind(x,y)
  }
  x$etamedc<-x$etamedp+x$diff  

  rm(list=c("j","m","y"))
  
  #################Add Population and the IFR by median age of infections##################################################### 
  x<-merge(x,IFR)
  PopIFRMed<-aggregate(list(x$Pop,x$DPop,x$IFR*x$DPop),by=list(x$anno,x$etamedc,x$diff),sum)
  colnames(PopIFRMed)<-c("anno","etamedc","diff","pop","DPop","IFRMed")
  Covid19all<-merge(Covid19all,PopIFRMed[,c(1:4,6)],all.x=TRUE,by.x=c("anno","etamediana"),by.y=c("anno","etamedc"),allow.cartesian=TRUE)
  Covid19all<-Covid19all[order(Covid19all$date),]

    
  #####################################inserisco IFRMed infrasettimanali tamite relazione lineare######################################################
  a<-PopIFRMed[PopIFRMed$etamedc==64 & PopIFRMed$anno==2020,]
  Covid19all<-Covid19all[order(Covid19all$date1),]
  Covid19all[Covid19all$date1==1,c("etamediana","IFRMed")]<-a[,c("etamedc","IFRMed")]

    for (i in 2:3){
      Covid19all$IFRMed[Covid19all$date1==i]<-Covid19all$IFRMed[Covid19all$date1==1]+(i-1)*(Covid19all$IFRMed[Covid19all$date1==4]-Covid19all$IFRMed[Covid19all$date1==1])/3
    }
    for (j in seq(from=4, to=(88-7), by=7)){
      for (i in 1:7){
        Covid19all$IFRMed[Covid19all$date1==j+i]<-Covid19all$IFRMed[Covid19all$date1==j]+i*(Covid19all$IFRMed[Covid19all$date1==j+7]-Covid19all$IFRMed[Covid19all$date1==j])/7
      }
    }   
    for (i in 89:96){
      Covid19all$IFRMed[Covid19all$date1==i]<-Covid19all$IFRMed[Covid19all$date1==88]+(i-88)*(Covid19all$IFRMed[Covid19all$date1==97]-Covid19all$IFRMed[Covid19all$date1==88])/9
    }
    for (j in seq(from=97, to=(216-7), by=7)){
      for (i in 1:7){
        Covid19all$IFRMed[Covid19all$date1==j+i]<-Covid19all$IFRMed[Covid19all$date1==j]+i*(Covid19all$IFRMed[Covid19all$date1==j+7]-Covid19all$IFRMed[Covid19all$date1==j])/7
      }
    }
    for (i in 217:223){
      Covid19all$IFRMed[Covid19all$date1==i]<-Covid19all$IFRMed[Covid19all$date1==216]+(i-216)*(Covid19all$IFRMed[Covid19all$date1==224]-Covid19all$IFRMed[Covid19all$date1==216])/8
    } 
    for (j in seq(from=224, to=(364-7), by=7)){
      for (i in 1:7){
        Covid19all$IFRMed[Covid19all$date1==j+i]<-Covid19all$IFRMed[Covid19all$date1==j]+i*(Covid19all$IFRMed[Covid19all$date1==j+7]-Covid19all$IFRMed[Covid19all$date1==j])/7
      }
    }
    for (i in 365:372){
      Covid19all$IFRMed[Covid19all$date1==i]<-Covid19all$IFRMed[Covid19all$date1==364]+(i-364)*(Covid19all$IFRMed[Covid19all$date1==373]-Covid19all$IFRMed[Covid19all$date1==364])/9
    }
    l1<-unique(max(Covid19all$date1[!is.na(Covid19all$etamediana)]))
    for (j in seq(from=373, to=(l1-7),by=7)){
      for (i in 1:7){
        Covid19all$IFRMed[Covid19all$date1==j+i]<-Covid19all$IFRMed[Covid19all$date1==j]+i*(Covid19all$IFRMed[Covid19all$date1==j+7]-Covid19all$IFRMed[Covid19all$date1==j])/7
      }
    }
    l2<-unique(max(Covid19all$date1))
    for (j in l1:l2) {
      Covid19all$IFRMed[Covid19all$date1==j]<-Covid19all$IFRMed[Covid19all$date1==l1]
    }

    #Popolazione per dati ISS
    PopIFR<-merge(merge(Pop,IFR),hVar)
    PopIFR<-aggregate(list(PopIFR$Pop,PopIFR$Pop*PopIFR$IFR,PopIFR$Pop*PopIFR$ha,PopIFR$Pop*PopIFR$hb,PopIFR$Pop*PopIFR$hg,PopIFR$Pop*PopIFR$hd,PopIFR$Pop*PopIFR$ho),by=list(PopIFR$anno, PopIFR$CEtaCod2, PopIFR$CEta2,PopIFR$etaMediana),sum)
    colnames(PopIFR)<-c("anno","CEtaCod2","CEta2","EtaMAnnoPop","pop","popIFR","popha","pophb","pophg","pophd","popho")
    PopIFR$IFR<-PopIFR$popIFR/PopIFR$pop
    PopIFR$ha<-PopIFR$popha/PopIFR$pop
    PopIFR$hb<-PopIFR$pophb/PopIFR$pop
    PopIFR$hg<-PopIFR$pophg/PopIFR$pop
    PopIFR$hd<-PopIFR$pophd/PopIFR$pop
    PopIFR$ho<-PopIFR$popho/PopIFR$pop
    PopIFR<-PopIFR[,!(colnames(PopIFR) %in% c("popIFR","popha","pophb","pophg","pophd","popho"))]
    str(PopIFR)
    PopIFRTot<-aggregate(PopIFR$pop,list(PopIFR$anno),sum)
    colnames(PopIFRTot)<-c("anno","popT")
    
    rm(list=c("a","i","j","l1","l2"))
    
    
    #####################################################Expected IFR  under Uniform Age-Based Virus Spread##############################################
    IFRRegAtt<-aggregate(list(PopIFR$pop,PopIFR$IFR*PopIFR$pop),list(PopIFR$anno),sum)
    colnames(IFRRegAtt)<-c("anno","Pop","IFRPop")
    Covid19all<-merge(Covid19all,IFRRegAtt,by.x="anno",by.y="anno")
    Covid19all$IFRAtt<-Covid19all$IFRPop/Covid19all$Pop

    #####################################################Correted IFRMed by ISTAT serosurveys results##############################################
    #numerosità stimata da indagine sierologica ISTAT (25/Maggio-15/Luglio/2020)
    NISTAT<-NISTAT[NISTAT$codreg==23,-2]
    Covid19all<-merge(Covid19all,NISTAT)
    Covid19all$IFRISTAT<-Covid19all$IFRMed*sum(Covid19all$dWMA[Covid19all$date1<118]/Covid19all$IFRMed[Covid19all$date1<118])/NISTAT$NISTAT
    Covid19all$NISTAT<-NULL

  #####################################################Importo Dati ISS: casi x età e sesso##################################################################
  fileNames <- dir(your_data_folder)
  dati.ISS <- NULL
  for(i in 1:495) {
    dati_i <- read_xlsx(paste(your_data_folder,fileNames[i],sep=""),sheet="sesso_eta")
    dati.ISS <- rbind(dati.ISS, dati_i)
  }
  dati.ISS<-dati.ISS[dati.ISS$SESSO!="Non noto" & dati.ISS$AGE_GROUP!="Non noto",]
  #dati.ISS$AGE_GROUP[dati.ISS$AGE_GROUP=="<3"]<-"90+"
  dati.ISS$AGE_GROUP[dati.ISS$AGE_GROUP==">90"]<-"90+"
  fileNames[495]

  ##################Estimated Table of daily infections and deaths by age class from daily updated data##################################################################
  #The dates refer to the last update so cumulative data could be  decreasing. we ordered cumulaive data to make them not descreasing
  dati.ISS<-data.table(dati.ISS)
  dati.ISS[,IDDate:=1:.N,by=c("SESSO","AGE_GROUP")]
  dati.ISS<-as.data.frame(dati.ISS)
  dati.ISS$IDDate<-dati.ISS$IDDate+287
  dati.ISS<-merge(dati.ISS,unique(Eta[,c("CEta2","CEtaCod2")]),by.x="AGE_GROUP",by.y="CEta2")
  dati.ISS$CodSESSO[dati.ISS$SESSO=="M"]<-1
  dati.ISS$CodSESSO[dati.ISS$SESSO=="F"]<-2
  dati.ISS$DECEDUTI[dati.ISS$DECEDUTI=="<5"]<-4
  dati.ISS$DECEDUTI<-as.numeric(dati.ISS$DECEDUTI)
  dati.ISS$CASI_CUMULATIVI<-as.numeric(dati.ISS$CASI_CUMULATIVI)
  z<-aggregate(dati.ISS$IDDate,list(dati.ISS$DECEDUTI,dati.ISS$CodSESSO,dati.ISS$CEtaCod2),min)
  colnames(z)<-c("d","CodSESSO","CEtaCod2","IDDate")
  v<-aggregate(dati.ISS$IDDate,list(dati.ISS$CASI_CUMULATIVI,dati.ISS$CodSESSO,dati.ISS$CEtaCod2),min)
  colnames(v)<-c("n","CodSESSO","CEtaCod2","IDDate")

  #Riordino le date dei decessi x sesso ed età
  for (j in 1:10){
    for (i in 1:2){
      varappoggio<-z[z$CodSESSO==i & z$CEtaCod2==j,]
      varappoggio<-varappoggio[order(varappoggio$CodSESSO,varappoggio$CEtaCod2,varappoggio$IDDate),]
      z$IDDated[z$CodSESSO==i & z$CEtaCod2==j]<-varappoggio$IDDate
    }
  }
  z[z$IDDated!=z$IDDate,]
  
  #Riordino le date dei casi positivi x sesso ed atà
  for (j in 1:10){
    for (i in 1:2){
      varappoggio<-v[v$CodSESSO==i & v$CEtaCod2==j,]
      varappoggio<-varappoggio[order(varappoggio$CodSESSO,varappoggio$CEtaCod2,varappoggio$IDDate),]
      v$IDDaten[v$CodSESSO==i & v$CEtaCod2==j]<-varappoggio$IDDate
    }
  }
  v[v$IDDaten!=v$IDDate,]
  
  #per ogni valore cumulato dei decessi prendo la data minima (inoltre prendo ultimo valore come massimo e primo come minimo)
  dati.ISS$DECEDUTI1<-0
  for (j in 1:10){
    for (i in 1:2){
      amax<-dati.ISS$DECEDUTI[dati.ISS$CEtaCod2==j & dati.ISS$CodSESSO==i & dati.ISS$IDDate==max(dati.ISS$IDDate[dati.ISS$CEtaCod2==j & dati.ISS$CodSESSO==i])]
      z$d[z$CEtaCod2==j & z$CodSESSO==i & z$d>amax]<-amax
      amin<-dati.ISS$DECEDUTI[dati.ISS$CEtaCod2==j & dati.ISS$CodSESSO==i & dati.ISS$IDDate==min(dati.ISS$IDDate[dati.ISS$CEtaCod2==j & dati.ISS$CodSESSO==i])]
      z$d[z$CEtaCod2==j & z$CodSESSO==i & z$d<amin]<-amin
      #dati.ISS$DECEDUTI1[dati.ISS$CEtaCod2==j & dati.ISS$CodSESSO==i & dati.ISS$DECEDUTI1>amax]<-amax
      z1<-unique(rev(z$d[z$CodSESSO==i & z$CEtaCod2==j]))
      dati.ISS$DECEDUTI1[dati.ISS$CodSESSO==i & dati.ISS$CEtaCod2==j]<-z1[1]
      for (k in 1:(length(z1)-1)) {
        maxdate<-min(z$IDDated[z$CodSESSO==i & z$CEtaCod2==j & z$d==z1[k]])
        dati.ISS$DECEDUTI1[dati.ISS$CodSESSO==i & dati.ISS$CEtaCod2==j & dati.ISS$IDDate<maxdate]<-z1[(k+1)]
      }
    }
  }
  
  #per ogni valore cumulato dei contagi prendo la data minima (inoltre prendo ultimo valore come massimo e primo come minimo)
  dati.ISS$CASI_CUMULATIVI1<-0
  for (j in 1:10){
    for (i in 1:2){
      bmax<-dati.ISS$CASI_CUMULATIVI[dati.ISS$CEtaCod2==j & dati.ISS$CodSESSO==i & dati.ISS$IDDate==max(dati.ISS$IDDate[dati.ISS$CEtaCod2==j & dati.ISS$CodSESSO==i])]
      v$n[v$CEtaCod2==j & v$CodSESSO==i & v$n>bmax]<-bmax
      bmin<-dati.ISS$CASI_CUMULATIVI[dati.ISS$CEtaCod2==j & dati.ISS$CodSESSO==i & dati.ISS$IDDate==min(dati.ISS$IDDate[dati.ISS$CEtaCod2==j & dati.ISS$CodSESSO==i])]
      v$n[v$CEtaCod2==j & v$CodSESSO==i & v$n<bmin]<-bmin
      #dati.ISS$DECEDUTI1[dati.ISS$CEtaCod2==j & dati.ISS$CodSESSO==i & dati.ISS$DECEDUTI1>amax]<-amax
      v1<-unique(rev(v$n[v$CodSESSO==i & v$CEtaCod2==j]))
      dati.ISS$CASI_CUMULATIVI1[dati.ISS$CodSESSO==i & dati.ISS$CEtaCod2==j]<-v1[1]
      for (k in 1:(length(v1)-1)) {
        maxdate<-min(v$IDDaten[v$CodSESSO==i & v$CEtaCod2==j & v$n==v1[k]])
        dati.ISS$CASI_CUMULATIVI1[dati.ISS$CodSESSO==i & dati.ISS$CEtaCod2==j & dati.ISS$IDDate<maxdate]<-v1[(k+1)]
      }
    }
  }
  dati.ISS<-dati.ISS[order(dati.ISS$CEtaCod2,dati.ISS$CodSESSO,dati.ISS$IDDate),]
  
  dati.ISS$date1<-as.Date(paste(substring(dati.ISS$iss_date,7,10),"-",substring(dati.ISS$iss_date,4,5),"-",substring(dati.ISS$iss_date,1,2),sep=""))
  dati.ISS<-aggregate(list(dati.ISS$DECEDUTI1,dati.ISS$CASI_CUMULATIVI1),list(dati.ISS$date1,dati.ISS$IDDate,dati.ISS$AGE_GROUP),sum)
  colnames(dati.ISS)<-c("date","IDDate","age","d","n")
  dati.ISS<-dati.ISS[order(dati.ISS$date,decreasing=FALSE),]

  dati.ISS<-dati.ISS[order(dati.ISS$age,dati.ISS$IDDate),]
  #add daily counts
  library(dplyr)
  dati.ISS <- dati.ISS %>%
  group_by(age) %>%
  mutate(
      dd0 = c(d[1], diff(d)),
      nd0 = c(n[1], diff(n)),
    )
  dati.ISS<-dati.ISS[dati.ISS$IDDate!=288,]
  
  dati.ISS<-merge(dati.ISS,unique(Eta[,4:5]),by.x="age",by.y="CEta2") #inserisco CEtaCod2
  dati.ISS$anno<-as.numeric(substring(dati.ISS$date,1,4)) #inserisco anno

  
  
  ###############################Smoothing the estimated Table of daily infections and deaths by age class:7day moving average  ###############################
  PesiIss<-rep(1,7)
  FineISS<-max(dati.ISS$IDDate)
  dati.ISS<-dati.ISS[order(dati.ISS$IDDate,dati.ISS$CEtaCod2),]
  
  for (j in 1:10)
  {
    for (i in 1:6)
    {
      dati.ISS$dd[dati.ISS$CEtaCod2==j & dati.ISS$IDDate==288+i]<-mySMA1(dati.ISS$dd0[dati.ISS$CEtaCod2==j & dati.ISS$IDDate<=288+i],i)[i]
      dati.ISS$nd[dati.ISS$CEtaCod2==j & dati.ISS$IDDate==288+i]<-mySMA1(dati.ISS$nd0[dati.ISS$CEtaCod2==j & dati.ISS$IDDate<=288+i],i)[i]
    }
  }
  
  for (j in 1:10){
    MMdISS<-mySMA1(dati.ISS$dd0[dati.ISS$CEtaCod2==j],7)
    MMdISS<-MMdISS[!is.na(MMdISS)]
    length(MMdISS)
    dati.ISS$dd[dati.ISS$IDDate>=295 & dati.ISS$CEtaCod2==j]<-MMdISS
    MMnISS<-mySMA1(dati.ISS$nd0[dati.ISS$CEtaCod2==j],7)
    MMnISS<-MMnISS[!is.na(MMnISS)]
    length(MMnISS)
    dati.ISS$nd[dati.ISS$IDDate>=295 & dati.ISS$CEtaCod2==j]<-MMnISS
  }
  
  for (j in 1:10){
    MMdISS<-mySMA(dati.ISS$dd0[dati.ISS$CEtaCod2==j],7,PesiIss)
    MMdISS<-MMdISS[!is.na(MMdISS)]
    length(MMdISS)
    dati.ISS$dd[dati.ISS$IDDate>=295 & dati.ISS$CEtaCod2==j]<-MMdISS
    MMnISS<-mySMA(dati.ISS$nd0[dati.ISS$CEtaCod2==j],7,PesiIss)
    MMnISS<-MMnISS[!is.na(MMnISS)]
    length(MMnISS)
    dati.ISS$nd[dati.ISS$IDDate>=295 & dati.ISS$CEtaCod2==j]<-MMnISS
  }
  dati.ISS<-dati.ISS[order(dati.ISS$CEtaCod2,dati.ISS$IDDate),]

  rm(list=c("dati_i","fileNames","i","j","k","z","v","v1","z1","amax","amin","bmax","bmin","FineISS","MMdISS","MMnISS","PesiIss","maxdate","varappoggio"))
  
  
  ############################################Moving averge with weights from time distribution to infection and death#############################################################
  #########confirmed cases
  InizioISS<-min(dati.ISS$IDDate)
  dati.ISS$date[dati.ISS$IDDate==InizioISS]
  bound1ISS<-unique(dati.ISS$IDDate[dati.ISS$date=="2021-02-07"])  #dic/2020-feb/2021
  Fine<-max(dati.ISS$IDDate)

    for (j in 1:10){
      cj<- dati.ISS$CEtaCod2==j
      MAN01<-mySMA(dati.ISS$nd[dati.ISS$IDDate>=InizioISS & dati.ISS$IDDate<=bound1ISS+2*mun3 & cj],2*mun3+1,wen3)
      MAN01<-MAN01[!is.na(MAN01)]
      length(MAN01)
      dati.ISS$nWMA0[dati.ISS$IDDate<=bound1ISS & cj]<-MAN01
      length(dati.ISS$nWMA0[dati.ISS$IDDate<=bound1ISS & cj])
      MAN02<-mySMA(dati.ISS$nd[dati.ISS$IDDate>bound1ISS & cj],2*mun4+1,wen4)
      MAN02<-MAN02[!is.na(MAN02)]  
      length(MAN02)
      dati.ISS$nWMA0[dati.ISS$IDDate>bound1ISS & dati.ISS$IDDate<=Fine-2*mun4 & cj]<-MAN02
      length(dati.ISS$nWMA0[dati.ISS$IDDate>bound1ISS & dati.ISS$IDDate<=Fine-2*mun4 & cj])
    }
  
  #########deaths
    for (j in 1:10){
      cj<- dati.ISS$CEtaCod2==j
      MAD01<-mySMA(dati.ISS$dd[dati.ISS$IDDate>=InizioISS & dati.ISS$IDDate<=bound1ISS+2*mud1 & cj],2*mud1+1,wed1)
      MAD01<-MAD01[!is.na(MAD01)]
      length(MAD01)
      length(dati.ISS$dWMA0[dati.ISS$IDDate<=bound1ISS & cj])
      dati.ISS$dWMA0[dati.ISS$IDDate<=bound1ISS & cj]<-MAD01
      MAD02<-mySMA(dati.ISS$dd[dati.ISS$IDDate>bound1ISS & cj],2*mud2+1,wed2)
      MAD02<-MAD02[!is.na(MAD02)]  
      length(MAD02)
      length(dati.ISS$dWMA0[dati.ISS$IDDate>bound1ISS & dati.ISS$IDDate<=Fine-2*mud2 &  cj])
      dati.ISS$dWMA0[dati.ISS$IDDate>bound1ISS & dati.ISS$IDDate<=Fine-2*mud2 &  cj]<-MAD02
      sum(dati.ISS$dd[(nrow(dati.ISS)-2*mud2):nrow(dati.ISS)]*wed2)
    }
  
  ISStot<-aggregate(list(dati.ISS$dWMA0,dati.ISS$nWMA0),list(dati.ISS$date),sum)
  colnames(ISStot)<-c("date","TotdWMA0","TotnWMA0")
  dati.ISS<-merge(dati.ISS,ISStot,by.x=c("date"),by.y="date")
  dati.ISS$PerdWMA0<-dati.ISS$dWMA0/dati.ISS$TotdWMA0
  dati.ISS$PernWMA0<-dati.ISS$nWMA0/dati.ISS$TotnWMA0
  dati.ISS<-dati.ISS[order(dati.ISS$IDDate,dati.ISS$CEtaCod2),]
  
  rm(list=c("bound1ISS","cj","Fine","InizioISS","j","ISStot","MAD01","MAD02","MAN01","MAN02"))
  

  ############################################### Variants #################################################
  
    #importo
    VarP<-VarP[VarP$country_code=="IT",c(3:4,9:12)] #remove useless columns
    colnames(VarP)[4:6]<-c("n","N","f") #rename
    VarP$f<-as.numeric(VarP$f)
    
    #add n. of weeks
    VarP$week<-ifelse(substring(VarP$year_week,1,4)=="2020",as.numeric(substring(VarP$year_week,6,7)),ifelse(substring(VarP$year_week,1,4)=="2021",as.numeric(substring(VarP$year_week,6,7))+53,as.numeric(substring(VarP$year_week,6,7))+105))
    
    #add date
    y<-as.data.frame(as.Date(seq(from=as.numeric(as.Date("2020-01-02")), to =as.numeric(as.Date("2022-03-24")),by=7),origin="1970-01-01")) # parto da centro prima settimana e avanzo di 7 giorni fino alla 13.ma settimana del 2022
    colnames(y)<-"date"
    rownames(y)<-NULL
    y$week<-as.numeric(rownames(y))  
    VarP<-merge(VarP,y) 
    
    #select TEssey
    VarP<-VarP[VarP$source=="TESSy",]
    
    # Add VOC names
    VarP$VOC<-ifelse(VarP$variant=="B.1.1.529","Omicron",
                     ifelse(VarP$variant=="B.1.617.2","Delta",
                            ifelse(VarP$variant=="B.1.1.7" | VarP$variant=="B.1.1.7+E484K ","Alpha",
                                   ifelse(VarP$variant=="P.1","Gamma",
                                          ifelse(VarP$variant=="B.1.351","Beta","NonVoc")))))
    
    #dataframe with dates from the beginning (24/02/2020)
    y<-merge(TabDate,as.data.frame(unique(VarP$VOC)))
    colnames(y)[3]<-"VOC"
        VarP<-merge(y,VarP[,-1],all.x = TRUE)
    VarP<-VarP[order(VarP$date,VarP$variant),]
    VarP$n<-ifelse(is.na(VarP$n),0,VarP$n) #replace null with "0"
    VarP$N<-ifelse(is.na(VarP$N),0,VarP$N)
    VarP$f<-ifelse(is.na(VarP$f),0,VarP$f)
    
    #VOCs rank
    VarP$VOCCod<-ifelse(VarP$VOC=="NonVoc",1,
                        ifelse(VarP$VOC=="Alpha",2,
                               ifelse(VarP$VOC=="Beta",3,
                                      ifelse(VarP$VOC=="Gamma",4,
                                             ifelse(VarP$VOC=="Delta",5,6)))))
    
    #sum by VOC: NonVoc includes several lineages 
    VarP<-aggregate(list(VarP$n,VarP$N,!is.na(VarP$N)),list(VarP$date,VarP$VOC,VarP$IDDate,VarP$VOCCod),sum)
    colnames(VarP)<-c("date","VOC","IDDate","VOCCod","n","N1","count")# rinomino campo
    VarP$N<-VarP$N/VarP$count
    VarP$N1<-VarP$N1-VarP$N
    VarP<-VarP[order(VarP$date,VarP$VOCCod),]
    VarP$N1<-VarP$count<-NULL
    
    #set to zero values before the official dates of VOCs identification
    z<-unique(VarP$IDDate[VarP$n>0 & VarP$VOC=="Omicron" & VarP$IDDate<=617])
    VarP$N[VarP$IDDate %in% z & VarP$VOC=="Alpha"]  <-VarP$N[VarP$IDDate %in% z & VarP$VOC=="Omicron"]-VarP$n[VarP$IDDate %in% z & VarP$VOC=="Omicron"]
    VarP$N[VarP$IDDate %in% z & VarP$VOC=="Beta"]   <-VarP$N[VarP$IDDate %in% z & VarP$VOC=="Omicron"]-VarP$n[VarP$IDDate %in% z & VarP$VOC=="Omicron"]
    VarP$N[VarP$IDDate %in% z & VarP$VOC=="Gamma"]  <-VarP$N[VarP$IDDate %in% z & VarP$VOC=="Omicron"]-VarP$n[VarP$IDDate %in% z & VarP$VOC=="Omicron"]
    VarP$N[VarP$IDDate %in% z & VarP$VOC=="Delta"]  <-VarP$N[VarP$IDDate %in% z & VarP$VOC=="Omicron"]-VarP$n[VarP$IDDate %in% z & VarP$VOC=="Omicron"]
    VarP$N[VarP$IDDate %in% z & VarP$VOC=="NonVoc"] <-VarP$N[VarP$IDDate %in% z & VarP$VOC=="Omicron"]-VarP$n[VarP$IDDate %in% z & VarP$VOC=="Omicron"]
    VarP$N[VarP$IDDate %in% z & VarP$VOC=="Omicron"]<-VarP$N[VarP$IDDate %in% z & VarP$VOC=="Omicron"]-VarP$n[VarP$IDDate %in% z & VarP$VOC=="Omicron"]
    VarP$n[VarP$IDDate %in% z & VarP$VOC=="Omicron"]<-0
    
    z<-VarP$IDDate[VarP$n>0 & VarP$VOC=="Gamma" & VarP$IDDate<=252]
    VarP$N[VarP$IDDate %in% z & VarP$VOC=="Alpha"]<-VarP$N[VarP$IDDate %in% z & VarP$VOC=="Gamma"]-VarP$n[VarP$IDDate %in% z & VarP$VOC=="Gamma"]
    VarP$N[VarP$IDDate %in% z & VarP$VOC=="Beta"]<-VarP$N[VarP$IDDate %in% z & VarP$VOC=="Gamma"]-VarP$n[VarP$IDDate %in% z & VarP$VOC=="Gamma"]
    VarP$N[VarP$IDDate %in% z & VarP$VOC=="Delta"]<-VarP$N[VarP$IDDate %in% z & VarP$VOC=="Gamma"]-VarP$n[VarP$IDDate %in% z & VarP$VOC=="Gamma"]
    VarP$N[VarP$IDDate %in% z & VarP$VOC=="Omicron"]<-VarP$N[VarP$IDDate %in% z & VarP$VOC=="Gamma"]-VarP$n[VarP$IDDate %in% z & VarP$VOC=="Gamma"]
    VarP$N[VarP$IDDate %in% z & VarP$VOC=="NonVoc"]<-VarP$N[VarP$IDDate %in% z & VarP$VOC=="Gamma"]-VarP$n[VarP$IDDate %in% z & VarP$VOC=="Gamma"]
    VarP$N[VarP$IDDate %in% z & VarP$VOC=="Gamma"]<-VarP$N[VarP$IDDate %in% z & VarP$VOC=="Gamma"]-VarP$n[VarP$IDDate %in% z & VarP$VOC=="Gamma"]
    VarP$n[VarP$IDDate %in% z & VarP$VOC=="Gamma"]<-0
    
    z<-VarP$IDDate[VarP$n>0 & VarP$VOC=="Delta" & VarP$IDDate<=221]
    VarP$N[VarP$IDDate %in% z & VarP$VOC=="Alpha"]<-VarP$N[VarP$IDDate %in% z & VarP$VOC=="Delta"]-VarP$n[VarP$IDDate %in% z & VarP$VOC=="Delta"]
    VarP$N[VarP$IDDate %in% z & VarP$VOC=="Beta"]<-VarP$N[VarP$IDDate %in% z & VarP$VOC=="Delta"]-VarP$n[VarP$IDDate %in% z & VarP$VOC=="Delta"]
    VarP$N[VarP$IDDate %in% z & VarP$VOC=="Gamma"]<-VarP$N[VarP$IDDate %in% z & VarP$VOC=="Delta"]-VarP$n[VarP$IDDate %in% z & VarP$VOC=="Delta"]
    VarP$N[VarP$IDDate %in% z & VarP$VOC=="Omicron"]<-VarP$N[VarP$IDDate %in% z & VarP$VOC=="Delta"]-VarP$n[VarP$IDDate %in% z & VarP$VOC=="Delta"]
    VarP$N[VarP$IDDate %in% z & VarP$VOC=="NonVoc"]<-VarP$N[VarP$IDDate %in% z & VarP$VOC=="Delta"]-VarP$n[VarP$IDDate %in% z & VarP$VOC=="Delta"]
    VarP$N[VarP$IDDate %in% z & VarP$VOC=="Delta"]<-VarP$N[VarP$IDDate %in% z & VarP$VOC=="Delta"]-VarP$n[VarP$IDDate %in% z & VarP$VOC=="Delta"]
    VarP$n[VarP$IDDate %in% z & VarP$VOC=="Delta"]<-0
    
    z<-VarP$IDDate[VarP$n>0 & VarP$VOC=="Alpha" & VarP$IDDate<=191]
    VarP$N[VarP$IDDate %in% z & VarP$VOC=="Beta"]<-VarP$N[VarP$IDDate %in% z & VarP$VOC=="Alpha"]-VarP$n[VarP$IDDate %in% z & VarP$VOC=="Alpha"]
    VarP$N[VarP$IDDate %in% z & VarP$VOC=="Gamma"]<-VarP$N[VarP$IDDate %in% z & VarP$VOC=="Alpha"]-VarP$n[VarP$IDDate %in% z & VarP$VOC=="Alpha"]
    VarP$N[VarP$IDDate %in% z & VarP$VOC=="Delta"]<-VarP$N[VarP$IDDate %in% z & VarP$VOC=="Alpha"]-VarP$n[VarP$IDDate %in% z & VarP$VOC=="Alpha"]
    VarP$N[VarP$IDDate %in% z & VarP$VOC=="Omicron"]<-VarP$N[VarP$IDDate %in% z & VarP$VOC=="Alpha"]-VarP$n[VarP$IDDate %in% z & VarP$VOC=="Alpha"]
    VarP$N[VarP$IDDate %in% z & VarP$VOC=="NonVoc"]<-VarP$N[VarP$IDDate %in% z & VarP$VOC=="Alpha"]-VarP$n[VarP$IDDate %in% z & VarP$VOC=="Alpha"]
    VarP$N[VarP$IDDate %in% z & VarP$VOC=="Alpha"]<-VarP$N[VarP$IDDate %in% z & VarP$VOC=="Alpha"]-VarP$n[VarP$IDDate %in% z & VarP$VOC=="Alpha"]
    VarP$n[VarP$IDDate %in% z & VarP$VOC=="Alpha"]<-0
    
    z<-VarP$IDDate[VarP$n>0 & VarP$VOC=="Beta" & VarP$IDDate<=68]
    VarP$N[VarP$IDDate %in% z & VarP$VOC=="Gamma"]<-VarP$N[VarP$IDDate %in% z & VarP$VOC=="Beta"]-VarP$n[VarP$IDDate %in% z & VarP$VOC=="Beta"]
    VarP$N[VarP$IDDate %in% z & VarP$VOC=="Delta"]<-VarP$N[VarP$IDDate %in% z & VarP$VOC=="Beta"]-VarP$n[VarP$IDDate %in% z & VarP$VOC=="Beta"]
    VarP$N[VarP$IDDate %in% z & VarP$VOC=="Omicron"]<-VarP$N[VarP$IDDate %in% z & VarP$VOC=="Beta"]-VarP$n[VarP$IDDate %in% z & VarP$VOC=="Beta"]
    VarP$N[VarP$IDDate %in% z & VarP$VOC=="NonVoc"]<-VarP$N[VarP$IDDate %in% z & VarP$VOC=="Beta"]-VarP$n[VarP$IDDate %in% z & VarP$VOC=="Beta"]
    VarP$N[VarP$IDDate %in% z & VarP$VOC=="Alpha"]<-VarP$N[VarP$IDDate %in% z & VarP$VOC=="Beta"]-VarP$n[VarP$IDDate %in% z & VarP$VOC=="Beta"]
    VarP$N[VarP$IDDate %in% z & VarP$VOC=="Beta"]<-VarP$N[VarP$IDDate %in% z & VarP$VOC=="Beta"]-VarP$n[VarP$IDDate %in% z & VarP$VOC=="Beta"]
    VarP$n[VarP$IDDate %in% z & VarP$VOC=="Beta"]<-0
    
    VarP$f<-ifelse(VarP$N==0,0,VarP$n/VarP$N)

    VarP$f[VarP$VOCCod==1 & VarP$IDDate<4]<-1 #assegno 100% alle NONVOC nei primi giorni
   
     #add midweek values
    for (j in seq(from=4, to=753, by=7)){
      for (i in 1:6){
        VarP$f[VarP$IDDate==j+i & VarP$VOCCod==1]<-VarP$f[VarP$IDDate==j & VarP$VOCCod==1]+i*(VarP$f[VarP$IDDate==j+7 & VarP$VOCCod==1]-VarP$f[VarP$IDDate==j & VarP$VOCCod==1])/7
        VarP$f[VarP$IDDate==j+i & VarP$VOCCod==2]<-VarP$f[VarP$IDDate==j & VarP$VOCCod==2]+i*(VarP$f[VarP$IDDate==j+7 & VarP$VOCCod==2]-VarP$f[VarP$IDDate==j & VarP$VOCCod==2])/7
        VarP$f[VarP$IDDate==j+i & VarP$VOCCod==3]<-VarP$f[VarP$IDDate==j & VarP$VOCCod==3]+i*(VarP$f[VarP$IDDate==j+7 & VarP$VOCCod==3]-VarP$f[VarP$IDDate==j & VarP$VOCCod==3])/7
        VarP$f[VarP$IDDate==j+i & VarP$VOCCod==4]<-VarP$f[VarP$IDDate==j & VarP$VOCCod==4]+i*(VarP$f[VarP$IDDate==j+7 & VarP$VOCCod==4]-VarP$f[VarP$IDDate==j & VarP$VOCCod==4])/7
        VarP$f[VarP$IDDate==j+i & VarP$VOCCod==5]<-VarP$f[VarP$IDDate==j & VarP$VOCCod==5]+i*(VarP$f[VarP$IDDate==j+7 & VarP$VOCCod==5]-VarP$f[VarP$IDDate==j & VarP$VOCCod==5])/7
        VarP$f[VarP$IDDate==j+i & VarP$VOCCod==6]<-VarP$f[VarP$IDDate==j & VarP$VOCCod==6]+i*(VarP$f[VarP$IDDate==j+7 & VarP$VOCCod==6]-VarP$f[VarP$IDDate==j & VarP$VOCCod==6])/7
      }
    }
    VarP<-VarP[order(VarP$IDDate,VarP$VOC),] #ordino
    min(VarP$date[VarP$VOC=="Alpha" & VarP$f>0])
    min(VarP$date[VarP$VOC=="Beta" & VarP$f>0])
    min(VarP$date[VarP$VOC=="Gamma" & VarP$f>0])
    min(VarP$date[VarP$VOC=="Delta" & VarP$f>0])
    min(VarP$date[VarP$VOC=="Omicron" & VarP$f>0])
    
    #cbind
    VarP<-cbind(VarP[VarP$VOC=="NonVoc",c(1,7)],cbind(VarP[VarP$VOC=="Alpha",7],cbind(VarP[VarP$VOC=="Beta",7],cbind(VarP[VarP$VOC=="Gamma",7],cbind(VarP[VarP$VOC=="Delta",7],VarP[VarP$VOC=="Omicron",7])))))
    colnames(VarP)[2:7]<-c("NonVoc","Alpha","Beta","Gamma","Delta","Omicron") # rinomino campi
    
    #check
    elementwise.all.equal <- Vectorize(function(x, y) {isTRUE(all.equal(x, y))})
    VarP[!elementwise.all.equal(VarP$NonVoc+VarP$Alpha+VarP$Beta+VarP$Gamma+VarP$Delta+VarP$Omicron, 1.00),]
 
    # merge with dati.ISS
    dati.ISS<-merge(dati.ISS,VarP,all.x=TRUE)
    
    #prevalent varints
    Pred<-dati.ISS[dati.ISS$CEtaCod2==6,c("date","IDDate","NonVoc","Alpha","Beta","Gamma","Delta","Omicron")]
    C1<-Pred$NonVoc>Pred$Alpha & Pred$NonVoc>Pred$Beta & Pred$NonVoc>Pred$Gamma & Pred$NonVoc>Pred$Delta & Pred$NonVoc>Pred$Omicron 
    C2<-Pred$Alpha>Pred$NonVoc & Pred$Alpha>Pred$Beta & Pred$Alpha>Pred$Gamma & Pred$Alpha>Pred$Delta & Pred$Alpha>Pred$Omicron 
    C3<-Pred$Beta>Pred$NonVoc & Pred$Beta>Pred$Alpha & Pred$Beta>Pred$Gamma & Pred$Beta>Pred$Delta & Pred$Beta>Pred$Omicron 
    C4<-Pred$Gamma>Pred$NonVoc & Pred$Gamma>Pred$Alpha & Pred$Gamma>Pred$Beta & Pred$Gamma>Pred$Delta & Pred$Gamma>Pred$Omicron 
    C5<-Pred$Delta>Pred$NonVoc & Pred$Delta>Pred$Alpha & Pred$Delta>Pred$Beta & Pred$Delta>Pred$Gamma & Pred$Delta>Pred$Omicron 
    C6<-Pred$Omicron>Pred$NonVoc & Pred$Omicron>Pred$Alpha & Pred$Omicron>Pred$Beta & Pred$Omicron>Pred$Delta & Pred$Omicron>Pred$Gamma 
    Pred$Per<-ifelse(C1,"NonVoc",
                     ifelse(C2,"Alpha",
                            ifelse(C3,"Beta",
                                   ifelse(C4,"Gamma",
                                          ifelse(C5,"Delta","Omicron")))))
    rm(list=c("y","C1","C2","C3","C4","C5","C6","i","j","z"))
    
    
    ############################################### Vaccines #################################################
    
    #add IDDate and as.numeric
    Vaccini$data<-as.Date(Vaccini$data)
    Vaccini<-merge(Vaccini,TabDate,by.x="data",by.y="date")
    Vaccini$forn[Vaccini$forn=="Pfizer Pediatrico"]<-"Pfizer/BioNTech"
    Vaccini$d1<-as.numeric(Vaccini$d1)
    Vaccini$d2<-as.numeric(Vaccini$d2)
    Vaccini$dpi<-as.numeric(Vaccini$dpi)
    Vaccini$db1<-as.numeric(Vaccini$db1)
    Vaccini$db2<-as.numeric(Vaccini$db2)
    Vaccini$db3<-as.numeric(Vaccini$db3)
    
    #Shorten the 05-11 class and enlarge the 12-19 class by redistributing 2/7 of the frequencies from the 05-11 class to the 12-19 class.
    Vaccini[Vaccini[Vaccini$eta=="12-19",c("IDDate","area","forn")] %in% Vaccini[Vaccini$eta=="05-11",c("IDDate","area","forn")],c("d1","d2","db1","db2","db3")]<-Vaccini[Vaccini[Vaccini$eta=="12-19",c("IDDate","area","forn")] %in% Vaccini[Vaccini$eta=="05-11",c("IDDate","area","forn")],c("d1","d2","db1","db2","db3")]+2/7*Vaccini[Vaccini[Vaccini$eta=="05-11",c("IDDate","area","forn")] %in% Vaccini[Vaccini$eta=="12-19",c("IDDate","area","forn")],c("d1","d2","db1","db2","db3")]
    Vaccini[Vaccini$eta=="05-11",c("d1","d2","db1","db2","db3")]<-5/7*Vaccini[Vaccini$eta=="05-11",c("d1","d2","db1","db2","db3")]

    #define the vaccine cycle    
    Vaccini$CV<-ifelse(Vaccini$forn=="Janssen",Vaccini$d1,Vaccini$d2+Vaccini$dpi)
    Vaccini$d1<-Vaccini$d1+Vaccini$dpi   
    
    #rename 
    Vaccini$eta[Vaccini$eta=="00-04" | Vaccini$eta=="05-11"]<-"0-9"
    Vaccini$eta[Vaccini$eta=="12-19"]<-"10-19"
    
    #aggregate data
    Vaccini<-aggregate(list(Vaccini$d1,Vaccini$d2,Vaccini$db1,Vaccini$db2,Vaccini$db3,Vaccini$CV),list(Vaccini$data,Vaccini$IDDate,Vaccini$eta),sum)
    colnames(Vaccini) <-c("date","IDDate","eta","d1","d2","db1","db2","db3","CV")
    
    #add age class code
    Vaccini<-merge(Vaccini,unique(Eta[,c(4,5)]),by.x="eta",by.y="CEta2")
    
    #add dates before first vaccination  
    Vaccini<-merge(Vaccini[,!(colnames(Vaccini) %in% c("date","eta"))],merge(TabDate[TabDate$IDDate>288,],unique(Eta[,c("CEta2","CEtaCod2")])),all.y =TRUE)
    Vaccini$d1[is.na(Vaccini$d1)]<-0
    Vaccini$d2[is.na(Vaccini$d2)]<-0
    Vaccini$db1[is.na(Vaccini$db1)]<-0
    Vaccini$db2[is.na(Vaccini$db2)]<-0
    Vaccini$db3[is.na(Vaccini$db3)]<-0
    Vaccini$CV[is.na(Vaccini$CV)]<-0
    str(Vaccini)
    
    
    #calculate cumulative data
    library(data.table)
    Vaccini<-setDT(Vaccini)[, csum := cumsum(d1), list(Vaccini$CEta2)][]
    colnames(Vaccini)[11]<-"dose1cum"
    Vaccini<-setDT(Vaccini)[, csum := cumsum(CV), list(Vaccini$CEta2)][]
    colnames(Vaccini)[12]<-"CVcum"
    Vaccini$"CV1/2"<-Vaccini$dose1cum-Vaccini$CVcum
    Vaccini<-setDT(Vaccini)[, csum := cumsum(db1), list(Vaccini$CEta2)][]
    colnames(Vaccini)[14]<-"Booster1cum"    
    Vaccini<-setDT(Vaccini)[, csum := cumsum(db2), list(Vaccini$CEta2)][]
    colnames(Vaccini)[15]<-"Booster2cum"   
    Vaccini<-setDT(Vaccini)[, csum := cumsum(db3), list(Vaccini$CEta2)][]
    colnames(Vaccini)[16]<-"Booster3cum"    
    

    #order
    Vaccini<-Vaccini[order(Vaccini$CEtaCod2,Vaccini$IDDate),]
    
    #complete vaccine cycle within 120 gg
    for (i in 1:10){
        Vaccini$CVa[Vaccini$CEtaCod2==i]<-mySMA2(Vaccini$CV[Vaccini$CEtaCod2==i],120)
        Vaccini$CVa[Vaccini$IDDate<428 & Vaccini$CEtaCod2==i]<-Vaccini$CVcum[Vaccini$IDDate<428 & Vaccini$CEtaCod2==i] #period of 4 months from the begining
        Vaccini$CVb[Vaccini$CEtaCod2==i]<-Vaccini$CVcum[Vaccini$CEtaCod2==i]
    }
    Vaccini$CVin4M<-Vaccini$CVa
    Vaccini$CVOltre4M<-Vaccini$CVb-Vaccini$CVa-Vaccini$Booster1cum
    
    #complete vaccine cycle within 150 gg
    for (i in 1:10){
        Vaccini$CVa[Vaccini$CEtaCod2==i]<-mySMA2(Vaccini$CV[Vaccini$CEtaCod2==i],150)
        Vaccini$CVa[Vaccini$IDDate<458 & Vaccini$CEtaCod2==i]<-Vaccini$CVcum[Vaccini$IDDate<458 & Vaccini$CEtaCod2==i] #period of 5 months from the begining
        Vaccini$CVb[Vaccini$CEtaCod2==i]<-Vaccini$CVcum[Vaccini$CEtaCod2==i]
    }
    Vaccini$CVin5M<-Vaccini$CVa
    Vaccini$CVOltre5M<-Vaccini$CVb-Vaccini$CVa-Vaccini$Booster1cum
    
    #complete vaccine cycle within 180 gg
    for (i in 1:10){
        Vaccini$CVa[Vaccini$CEtaCod2==i]<-mySMA2(Vaccini$CV[Vaccini$CEtaCod2==i],180)
        Vaccini$CVa[Vaccini$IDDate<488 & Vaccini$CEtaCod2==i]<-cumsum(Vaccini$CV[Vaccini$IDDate<488 & Vaccini$CEtaCod2==i])   
        Vaccini$CVa[Vaccini$IDDate<488 & Vaccini$CEtaCod2==i]<-Vaccini$CVcum[Vaccini$IDDate<488 & Vaccini$CEtaCod2==i] #period of 6 months from the begining
        Vaccini$CVb[Vaccini$CEtaCod2==i]<-Vaccini$CVcum[Vaccini$CEtaCod2==i]
    }
    Vaccini$CVin6M<-Vaccini$CVa
    Vaccini$CVOltre6M<-Vaccini$CVb-Vaccini$CVa-Vaccini$Booster1cum
  
    #calculate Population be 10-years age classes (0-9,10-19,...,80-89,100+)
    Pop2<-aggregate(Pop$Pop,list(Pop$anno,Pop$CEta2),sum)
    colnames(Pop2)<-c("anno","CEta2","pop")

    #calculate Population be 10-years age classes starting from 5-9 (5-9,10-19,...,80-89,100+)
    PopVacc<-aggregate(Pop$Pop[Pop$CEtaCod>1],list(Pop$anno[Pop$CEtaCod>1],Pop$CEta2[Pop$CEtaCod>1]),sum)
    colnames(PopVacc)<-c("anno","CEta2","pop")

    #Add populations to Vaccini table
    Vaccini$anno<-as.numeric(substring(Vaccini$date,1,4))
    Vaccini<-merge(merge(Vaccini,PopVacc),Pop2)
    colnames(Vaccini)[colnames(Vaccini) %in% c("CEta2","pop.x","pop.y")]<-c("etaV","pop59","pop09")
   
    Vaccini<-Vaccini[order(Vaccini$IDDate,Vaccini$CEtaCod2),]
    Vaccini<-Vaccini[,c("anno","date","IDDate","CEtaCod2","etaV","pop59","pop09","dose1cum", "CVcum","CV1/2","Booster1cum","Booster2cum","Booster3cum","CVa","CVb","CVin4M","CVOltre4M","CVin5M","CVOltre5M","CVin6M","CVOltre6M")]
    
    
    
    ##########weigths for risks vaccinated/non-vaccinated
    #ralative risk 1-dose Vs No vaccinated 
    Vaccini$PV1<-Vaccini$"CV1/2"
    
    #completed vaccine cycle over 4 months (from 12/01/2020), 5 months (from 11/01-30/2020), 6 months (from 10/01-31/2020)
    Vaccini$PV2<-Vaccini$CVOltre4M
    Vaccini$PV2[Vaccini$IDDate<647]<-Vaccini$CVOltre5M[Vaccini$IDDate<647] #novembre 2021           unique(dati.ISS$IDDate[dati.ISS$date=="2021-11-30"])
    Vaccini$PV2[Vaccini$IDDate<617]<-Vaccini$CVOltre6M[Vaccini$IDDate<617] #ottobre 2021            unique(dati.ISS$IDDate[dati.ISS$date=="2021-10-31"])
    Vaccini$PV2[Vaccini$IDDate<586]<-Vaccini$CVcum[Vaccini$IDDate<586] #fino a 30 settembre 2021    unique(dati.ISS$IDDate[dati.ISS$date=="2021-09-30"])
    
    #completed vaccine cycle within 4 months (from 12/01/2020), 5 months (from 11/01-30/2020), 6 months (from 10/01-31/2020)
    Vaccini$PV3<-Vaccini$CVin4M
    Vaccini$PV3[Vaccini$IDDate<647]<-Vaccini$CVin5M[Vaccini$IDDate<647] #novembre 2021           unique(dati.ISS$IDDate[dati.ISS$date=="2021-11-30"])
    Vaccini$PV3[Vaccini$IDDate<617]<-Vaccini$CVin6M[Vaccini$IDDate<617] #ottobre 2021            unique(dati.ISS$IDDate[dati.ISS$date=="2021-10-31"])
    Vaccini$PV3[Vaccini$IDDate<586]<-0 #fino a 30 settembre 2021                                 unique(dati.ISS$IDDate[dati.ISS$date=="2021-09-30"])
    
    #booster shot
    Vaccini$PV4<-Vaccini$Booster1cum   #novembre 2021
    #Vaccini$PV4[Vaccini$IDDate<617]<-0   #tolgo da ottobre 2021
    Covid19all$date[Covid19all$date1==477]
    Vaccini$PV1[is.na(Vaccini$PV1)]
    Vaccini$PV2[is.na(Vaccini$PV2)]
    Vaccini$PV3[is.na(Vaccini$PV3)]
    Vaccini$PV4[is.na(Vaccini$PV4)]

    #merge vaccinati and dati.ISS
    Vaccini<-as.data.frame(Vaccini)
    dati.ISS<-merge(dati.ISS,Vaccini[Vaccini$IDDate<=782,!(colnames(Vaccini) %in% c("date","anno","CVa","CVb"))],by.x=c("IDDate","CEtaCod2"),by.y=c("IDDate","CEtaCod2"),all=TRUE)
    colnames(dati.ISS)
    colnames(Vaccini)
    
    
    ###########################################################smoothing risks from one estimation point to the next one#############################################################
    #Siccome fino a settembre il rischio vaccinati non è diviso in 2 (>120,<120), da fine guigno lo faccio arrivare fino alla media dei 2 richi calcolata a ottobre
    a1<-merge(RischiVacc[RischiVacc$IDDate==586,c("CEtaCod2","IFRC2","IFRM2","IFRC3","IFRM3")],Vaccini[Vaccini$IDDate==586,c("CEtaCod2","PV2","PV3")],by.x="CEtaCod2",by.y="CEtaCod2")
    a1$IFRC2Medio<-ifelse(a1$PV2+a1$PV3==0,1,(a1$IFRC2*a1$PV2+a1$IFRC3*a1$PV3)/(a1$PV2+a1$PV3))
    a1$IFRM2Medio<-ifelse(a1$PV2+a1$PV3==0,1,(a1$IFRM2*a1$PV2+a1$IFRM3*a1$PV3)/(a1$PV2+a1$PV3))
    
    date1 <-unique(dati.ISS$IDDate[dati.ISS$date=="2021-06-30"])
    date2a<-unique(dati.ISS$IDDate[dati.ISS$date=="2021-09-30"])
    date2 <-unique(dati.ISS$IDDate[dati.ISS$date=="2021-10-01"])
    date3 <-unique(dati.ISS$IDDate[dati.ISS$date=="2021-11-15"])
    date4 <-unique(dati.ISS$IDDate[dati.ISS$date=="2021-12-15"])
    date5 <-unique(dati.ISS$IDDate[dati.ISS$date=="2022-01-15"])
    date6 <-unique(dati.ISS$IDDate[dati.ISS$date=="2022-02-15"])
    date7 <-unique(dati.ISS$IDDate[dati.ISS$date=="2022-03-15"])
    date8 <-unique(dati.ISS$IDDate[dati.ISS$date=="2021-10-10"])
    date9 <-unique(dati.ISS$IDDate[dati.ISS$date=="2021-11-20"])
    
    #Risk vaccinati-totali/non-Vaccinati goes linearly from June to 30 settembre, dove al 30/09 c'è la media dei risichi dei vaccinati in e oltre 120 gg
    for (j in date1:date2a){
      for (i in 1:10){
        c1<-RischiVacc$IDDate==date1
        c2<-RischiVacc$IDDate==date2a
        c3<-RischiVacc$CEtaCod2==i
        int<-date2a-date1
        RischiVacc$IFRM2[RischiVacc$IDDate==j & c3]<-RischiVacc$IFRM2[c1 & c3]+(j-date1)*(a1$IFRM2Medio[a1$CEtaCod2==i]-RischiVacc$IFRM2[c1 & c3])/int
        RischiVacc$IFRC2[RischiVacc$IDDate==j & c3]<-RischiVacc$IFRC2[c1 & c3]+(j-date1)*(a1$IFRC2Medio[a1$CEtaCod2==i]-RischiVacc$IFRC2[c1 & c3])/int
      }
    }
    #Rischio booster lo faccio andare linearmente da giugno (=1 perché non c'era) al 15 Nov (dve c'è prima stima del rishio) per coprire i casi prima della prima stima
    for (j in date1:date3){
      for (i in 1:10){
        c1<-RischiVacc$IDDate==date1
        c2<-RischiVacc$IDDate==date3
        c3<-RischiVacc$CEtaCod2==i
        int<-date3-date1
        RischiVacc$IFRM4[RischiVacc$IDDate==j & c3]<-RischiVacc$IFRM4[c1 & c3]+(j-date1)*(RischiVacc$IFRM4[c2 & c3]-RischiVacc$IFRM4[c1 & c3])/int
        RischiVacc$IFRC4[RischiVacc$IDDate==j & c3]<-RischiVacc$IFRC4[c1 & c3]+(j-date1)*(RischiVacc$IFRC4[c2 & c3]-RischiVacc$IFRC4[c1 & c3])/int
      }
    }
    
    for (j in date1:date2){
      for (i in 1:10){
        c1<-RischiVacc$IDDate==date1
        c2<-RischiVacc$IDDate==date2
        c3<-RischiVacc$CEtaCod2==i
        int<-date2-date1
        RischiVacc$IFRM1[RischiVacc$IDDate==j & c3]<-RischiVacc$IFRM1[c1 & c3]+(j-date1)*(RischiVacc$IFRM1[c2 & c3]-RischiVacc$IFRM1[c1 & c3])/int
        RischiVacc$IFRM3[RischiVacc$IDDate==j & c3]<-RischiVacc$IFRM3[c1 & c3]+(j-date1)*(RischiVacc$IFRM3[c2 & c3]-RischiVacc$IFRM3[c1 & c3])/int
        RischiVacc$IFRC1[RischiVacc$IDDate==j & c3]<-RischiVacc$IFRC1[c1 & c3]+(j-date1)*(RischiVacc$IFRC1[c2 & c3]-RischiVacc$IFRC1[c1 & c3])/int
        RischiVacc$IFRC3[RischiVacc$IDDate==j & c3]<-RischiVacc$IFRC3[c1 & c3]+(j-date1)*(RischiVacc$IFRC3[c2 & c3]-RischiVacc$IFRC3[c1 & c3])/int
      }
    }
    for (j in date2:date3){
      for (i in 1:10){
        c1<-RischiVacc$IDDate==date2
        c2<-RischiVacc$IDDate==date3
        c3<-RischiVacc$CEtaCod2==i
        int<-date3-date2
        RischiVacc$IFRM1[RischiVacc$IDDate==j & c3]<-RischiVacc$IFRM1[c1 & c3]+(j-date2)*(RischiVacc$IFRM1[c2 & c3]-RischiVacc$IFRM1[c1 & c3])/int
        RischiVacc$IFRM2[RischiVacc$IDDate==j & c3]<-RischiVacc$IFRM2[c1 & c3]+(j-date2)*(RischiVacc$IFRM2[c2 & c3]-RischiVacc$IFRM2[c1 & c3])/int
        RischiVacc$IFRM3[RischiVacc$IDDate==j & c3]<-RischiVacc$IFRM3[c1 & c3]+(j-date2)*(RischiVacc$IFRM3[c2 & c3]-RischiVacc$IFRM3[c1 & c3])/int
        RischiVacc$IFRC1[RischiVacc$IDDate==j & c3]<-RischiVacc$IFRC1[c1 & c3]+(j-date2)*(RischiVacc$IFRC1[c2 & c3]-RischiVacc$IFRC1[c1 & c3])/int
        RischiVacc$IFRC2[RischiVacc$IDDate==j & c3]<-RischiVacc$IFRC2[c1 & c3]+(j-date2)*(RischiVacc$IFRC2[c2 & c3]-RischiVacc$IFRC2[c1 & c3])/int
        RischiVacc$IFRC3[RischiVacc$IDDate==j & c3]<-RischiVacc$IFRC3[c1 & c3]+(j-date2)*(RischiVacc$IFRC3[c2 & c3]-RischiVacc$IFRC3[c1 & c3])/int
      }
    }
    for (j in date3:date4){
      for (i in 1:10){
        c1<-RischiVacc$IDDate==date3
        c2<-RischiVacc$IDDate==date4
        c3<-RischiVacc$CEtaCod2==i
        int<-date4-date3
        RischiVacc$IFRM1[RischiVacc$IDDate==j & c3]<-RischiVacc$IFRM1[c1 & c3]+(j-date3)*(RischiVacc$IFRM1[c2 & c3]-RischiVacc$IFRM1[c1 & c3])/int
        RischiVacc$IFRM2[RischiVacc$IDDate==j & c3]<-RischiVacc$IFRM2[c1 & c3]+(j-date3)*(RischiVacc$IFRM2[c2 & c3]-RischiVacc$IFRM2[c1 & c3])/int
        RischiVacc$IFRM3[RischiVacc$IDDate==j & c3]<-RischiVacc$IFRM3[c1 & c3]+(j-date3)*(RischiVacc$IFRM3[c2 & c3]-RischiVacc$IFRM3[c1 & c3])/int
        RischiVacc$IFRM4[RischiVacc$IDDate==j & c3]<-RischiVacc$IFRM4[c1 & c3]+(j-date3)*(RischiVacc$IFRM4[c2 & c3]-RischiVacc$IFRM4[c1 & c3])/int
        RischiVacc$IFRC1[RischiVacc$IDDate==j & c3]<-RischiVacc$IFRC1[c1 & c3]+(j-date3)*(RischiVacc$IFRC1[c2 & c3]-RischiVacc$IFRC1[c1 & c3])/int
        RischiVacc$IFRC2[RischiVacc$IDDate==j & c3]<-RischiVacc$IFRC2[c1 & c3]+(j-date3)*(RischiVacc$IFRC2[c2 & c3]-RischiVacc$IFRC2[c1 & c3])/int
        RischiVacc$IFRC3[RischiVacc$IDDate==j & c3]<-RischiVacc$IFRC3[c1 & c3]+(j-date3)*(RischiVacc$IFRC3[c2 & c3]-RischiVacc$IFRC3[c1 & c3])/int
        RischiVacc$IFRC4[RischiVacc$IDDate==j & c3]<-RischiVacc$IFRC4[c1 & c3]+(j-date3)*(RischiVacc$IFRC4[c2 & c3]-RischiVacc$IFRC4[c1 & c3])/int
      }
    }
    for (j in date4:date5){
      for (i in 1:10){
        c1<-RischiVacc$IDDate==date4
        c2<-RischiVacc$IDDate==date5
        c3<-RischiVacc$CEtaCod2==i
        int<-date5-date4
        RischiVacc$IFRM1[RischiVacc$IDDate==j & c3]<-RischiVacc$IFRM1[c1 & c3]+(j-date4)*(RischiVacc$IFRM1[c2 & c3]-RischiVacc$IFRM1[c1 & c3])/int
        RischiVacc$IFRM2[RischiVacc$IDDate==j & c3]<-RischiVacc$IFRM2[c1 & c3]+(j-date4)*(RischiVacc$IFRM2[c2 & c3]-RischiVacc$IFRM2[c1 & c3])/int
        RischiVacc$IFRM3[RischiVacc$IDDate==j & c3]<-RischiVacc$IFRM3[c1 & c3]+(j-date4)*(RischiVacc$IFRM3[c2 & c3]-RischiVacc$IFRM3[c1 & c3])/int
        RischiVacc$IFRM4[RischiVacc$IDDate==j & c3]<-RischiVacc$IFRM4[c1 & c3]+(j-date4)*(RischiVacc$IFRM4[c2 & c3]-RischiVacc$IFRM4[c1 & c3])/int
        RischiVacc$IFRC1[RischiVacc$IDDate==j & c3]<-RischiVacc$IFRC1[c1 & c3]+(j-date4)*(RischiVacc$IFRC1[c2 & c3]-RischiVacc$IFRC1[c1 & c3])/int
        RischiVacc$IFRC2[RischiVacc$IDDate==j & c3]<-RischiVacc$IFRC2[c1 & c3]+(j-date4)*(RischiVacc$IFRC2[c2 & c3]-RischiVacc$IFRC2[c1 & c3])/int
        RischiVacc$IFRC3[RischiVacc$IDDate==j & c3]<-RischiVacc$IFRC3[c1 & c3]+(j-date4)*(RischiVacc$IFRC3[c2 & c3]-RischiVacc$IFRC3[c1 & c3])/int
        RischiVacc$IFRC4[RischiVacc$IDDate==j & c3]<-RischiVacc$IFRC4[c1 & c3]+(j-date4)*(RischiVacc$IFRC4[c2 & c3]-RischiVacc$IFRC4[c1 & c3])/int
      }
    }
    for (j in date5:date6){
      for (i in 1:10){
        c1<-RischiVacc$IDDate==date5
        c2<-RischiVacc$IDDate==date6
        c3<-RischiVacc$CEtaCod2==i
        int<-date6-date5
        RischiVacc$IFRM1[RischiVacc$IDDate==j & c3]<-RischiVacc$IFRM1[c1 & c3]+(j-date5)*(RischiVacc$IFRM1[c2 & c3]-RischiVacc$IFRM1[c1 & c3])/int
        RischiVacc$IFRM2[RischiVacc$IDDate==j & c3]<-RischiVacc$IFRM2[c1 & c3]+(j-date5)*(RischiVacc$IFRM2[c2 & c3]-RischiVacc$IFRM2[c1 & c3])/int
        RischiVacc$IFRM3[RischiVacc$IDDate==j & c3]<-RischiVacc$IFRM3[c1 & c3]+(j-date5)*(RischiVacc$IFRM3[c2 & c3]-RischiVacc$IFRM3[c1 & c3])/int
        RischiVacc$IFRM4[RischiVacc$IDDate==j & c3]<-RischiVacc$IFRM4[c1 & c3]+(j-date5)*(RischiVacc$IFRM4[c2 & c3]-RischiVacc$IFRM4[c1 & c3])/int
        RischiVacc$IFRC1[RischiVacc$IDDate==j & c3]<-RischiVacc$IFRC1[c1 & c3]+(j-date5)*(RischiVacc$IFRC1[c2 & c3]-RischiVacc$IFRC1[c1 & c3])/int
        RischiVacc$IFRC2[RischiVacc$IDDate==j & c3]<-RischiVacc$IFRC2[c1 & c3]+(j-date5)*(RischiVacc$IFRC2[c2 & c3]-RischiVacc$IFRC2[c1 & c3])/int
        RischiVacc$IFRC3[RischiVacc$IDDate==j & c3]<-RischiVacc$IFRC3[c1 & c3]+(j-date5)*(RischiVacc$IFRC3[c2 & c3]-RischiVacc$IFRC3[c1 & c3])/int
        RischiVacc$IFRC4[RischiVacc$IDDate==j & c3]<-RischiVacc$IFRC4[c1 & c3]+(j-date5)*(RischiVacc$IFRC4[c2 & c3]-RischiVacc$IFRC4[c1 & c3])/int
      }
    }
    for (j in date6:date7){
      for (i in 1:10){
        c1<-RischiVacc$IDDate==date6
        c2<-RischiVacc$IDDate==date7
        c3<-RischiVacc$CEtaCod2==i
        int<-date7-date6
        RischiVacc$IFRM1[RischiVacc$IDDate==j & c3]<-RischiVacc$IFRM1[c1 & c3]+(j-date6)*(RischiVacc$IFRM1[c2 & c3]-RischiVacc$IFRM1[c1 & c3])/int
        RischiVacc$IFRM2[RischiVacc$IDDate==j & c3]<-RischiVacc$IFRM2[c1 & c3]+(j-date6)*(RischiVacc$IFRM2[c2 & c3]-RischiVacc$IFRM2[c1 & c3])/int
        RischiVacc$IFRM3[RischiVacc$IDDate==j & c3]<-RischiVacc$IFRM3[c1 & c3]+(j-date6)*(RischiVacc$IFRM3[c2 & c3]-RischiVacc$IFRM3[c1 & c3])/int
        RischiVacc$IFRM4[RischiVacc$IDDate==j & c3]<-RischiVacc$IFRM4[c1 & c3]+(j-date6)*(RischiVacc$IFRM4[c2 & c3]-RischiVacc$IFRM4[c1 & c3])/int
        RischiVacc$IFRC1[RischiVacc$IDDate==j & c3]<-RischiVacc$IFRC1[c1 & c3]+(j-date6)*(RischiVacc$IFRC1[c2 & c3]-RischiVacc$IFRC1[c1 & c3])/int
        RischiVacc$IFRC2[RischiVacc$IDDate==j & c3]<-RischiVacc$IFRC2[c1 & c3]+(j-date6)*(RischiVacc$IFRC2[c2 & c3]-RischiVacc$IFRC2[c1 & c3])/int
        RischiVacc$IFRC3[RischiVacc$IDDate==j & c3]<-RischiVacc$IFRC3[c1 & c3]+(j-date6)*(RischiVacc$IFRC3[c2 & c3]-RischiVacc$IFRC3[c1 & c3])/int
        RischiVacc$IFRC4[RischiVacc$IDDate==j & c3]<-RischiVacc$IFRC4[c1 & c3]+(j-date6)*(RischiVacc$IFRC4[c2 & c3]-RischiVacc$IFRC4[c1 & c3])/int
      }
    }
    
    #Add risks in dati.ISS 
    RischiVacc$RR0<-RischiVacc$IFRC0/RischiVacc$IFRM0
    RischiVacc$RR1<-RischiVacc$IFRC1/RischiVacc$IFRM1
    RischiVacc$RR2<-RischiVacc$IFRC2/RischiVacc$IFRM2
    RischiVacc$RR3<-RischiVacc$IFRC3/RischiVacc$IFRM3
    RischiVacc$RR4<-RischiVacc$IFRC4/RischiVacc$IFRM4
    
    RischiVacc$RR00<-1/RischiVacc$IFRM0
    RischiVacc$RR01<-1/RischiVacc$IFRM1
    RischiVacc$RR02<-1/RischiVacc$IFRM2
    RischiVacc$RR03<-1/RischiVacc$IFRM3
    RischiVacc$RR04<-1/RischiVacc$IFRM4
    
    #smooth around points where change the reference period of vaccination (180gg to 150gg)
    for (j in date8:date9){
      for (i in 1:10){
        c1<-RischiVacc$IDDate==date8
        c2<-RischiVacc$IDDate==date9
        c3<-RischiVacc$CEtaCod2==i
        int<-date9-date8
        RischiVacc$RR1[RischiVacc$IDDate==j & c3]<-RischiVacc$RR1[c1 & c3]+(j-date8)*(RischiVacc$RR1[c2 & c3]-RischiVacc$RR1[c1 & c3])/int
        RischiVacc$RR2[RischiVacc$IDDate==j & c3]<-RischiVacc$RR2[c1 & c3]+(j-date8)*(RischiVacc$RR2[c2 & c3]-RischiVacc$RR2[c1 & c3])/int
        RischiVacc$RR3[RischiVacc$IDDate==j & c3]<-RischiVacc$RR3[c1 & c3]+(j-date8)*(RischiVacc$RR3[c2 & c3]-RischiVacc$RR3[c1 & c3])/int
        RischiVacc$RR4[RischiVacc$IDDate==j & c3]<-RischiVacc$RR4[c1 & c3]+(j-date8)*(RischiVacc$RR4[c2 & c3]-RischiVacc$RR4[c1 & c3])/int
        RischiVacc$RR01[RischiVacc$IDDate==j & c3]<-RischiVacc$RR01[c1 & c3]+(j-date8)*(RischiVacc$RR01[c2 & c3]-RischiVacc$RR01[c1 & c3])/int
        RischiVacc$RR02[RischiVacc$IDDate==j & c3]<-RischiVacc$RR02[c1 & c3]+(j-date8)*(RischiVacc$RR02[c2 & c3]-RischiVacc$RR02[c1 & c3])/int
        RischiVacc$RR03[RischiVacc$IDDate==j & c3]<-RischiVacc$RR03[c1 & c3]+(j-date8)*(RischiVacc$RR03[c2 & c3]-RischiVacc$RR03[c1 & c3])/int
        RischiVacc$RR04[RischiVacc$IDDate==j & c3]<-RischiVacc$RR04[c1 & c3]+(j-date8)*(RischiVacc$RR04[c2 & c3]-RischiVacc$RR04[c1 & c3])/int
      } 
    }
    rm(list=c("date1","date2","date2a","date3","date4","date5","date6","date7","date8","date9","c1","c2","c3","i","j","int","a1"))
    

    ###########################################################Add Risks in dati.ISS##############################################################
    dati.ISS<- merge(dati.ISS,RischiVacc[,c(1:2,13:22)],all.x=TRUE)
    dati.ISS$RR0[dati.ISS$IDDate<308]<-dati.ISS$RR1[dati.ISS$IDDate<308]<-dati.ISS$RR2[dati.ISS$IDDate<308]<-dati.ISS$RR3[dati.ISS$IDDate<308]<-dati.ISS$RR4[dati.ISS$IDDate<308]<-1
    dati.ISS$RR00[dati.ISS$IDDate<308]<-dati.ISS$RR01[dati.ISS$IDDate<308]<-dati.ISS$RR02[dati.ISS$IDDate<308]<-dati.ISS$RR03[dati.ISS$IDDate<308]<-dati.ISS$RR04[dati.ISS$IDDate<308]<-1
    
    dati.ISS<-dati.ISS[,c("anno","date","IDDate","age","etaV","CEtaCod2","d","n","dd0","nd0","dd","nd","nWMA0","dWMA0","TotdWMA0","TotnWMA0","PerdWMA0","PernWMA0","NonVoc","Alpha","Beta","Gamma","Delta","Omicron","pop59","pop09","dose1cum","CVcum","CV1/2","Booster1cum","Booster2cum","Booster3cum","CVin4M","CVOltre4M","CVin5M","CVOltre5M","CVin6M","CVOltre6M","PV1","PV2","PV3","PV4","RR0","RR1","RR2","RR3","RR4","RR00","RR01","RR02","RR03","RR04")]

    dati.ISS<-merge(dati.ISS,PopIFR,by.x=c("anno","CEtaCod2"),by.y=c("anno","CEtaCod2"))
    
    #estimate lethality by variant 
    p0<-dati.ISS$IFR*dati.ISS$NonVoc
    p1<-(1-(1-dati.ISS$IFR)^dati.ISS$ha)*dati.ISS$Alpha
    p2<-(1-(1-dati.ISS$IFR)^dati.ISS$hb)*dati.ISS$Beta
    p3<-(1-(1-dati.ISS$IFR)^dati.ISS$hg)*dati.ISS$Gamma
    p4<-(1-(1-dati.ISS$IFR)^dati.ISS$hd)*dati.ISS$Delta
    p5<-(1-(1-dati.ISS$IFR)^dati.ISS$ho)*dati.ISS$Omicron
    max(dati.ISS$RR1)
    PVar<-dati.ISS$NonVoc+dati.ISS$Alpha+dati.ISS$Beta+dati.ISS$Gamma+dati.ISS$Delta+dati.ISS$Omicron
    p0[dati.ISS$date=="2022-03-01"]  
    
    #Estimate weigthed relative risks vacc/non-vacc
    RR1 <- dati.ISS$RR1*dati.ISS$PV1
    RR2 <- dati.ISS$RR2*dati.ISS$PV2
    RR3 <- dati.ISS$RR3*dati.ISS$PV3
    RR4 <- dati.ISS$RR4*dati.ISS$PV4
    RR01<-dati.ISS$RR01*dati.ISS$PV1
    RR02<-dati.ISS$RR02*dati.ISS$PV2
    RR03<-dati.ISS$RR03*dati.ISS$PV3
    RR04<-dati.ISS$RR04*dati.ISS$PV4

    #Etimate population of non-vaccinated
    dati.ISS$NoCV<-dati.ISS$pop09-dati.ISS$PV1-dati.ISS$PV2-dati.ISS$PV3-dati.ISS$PV4
   
    #Adjust negative estimate by considering 5% of population by age class cannot be vaccinated
    PopNeg<-!is.na(dati.ISS$NoCV) & dati.ISS$NoCV<0
    aa<-dati.ISS[dati.ISS$NoCV<0 & !is.na(dati.ISS$NoCV),]
    aa$Pop09corr<-ceiling(aa$pop09-aa$NoCV+5/100*(aa$pop09-aa$NoCV))
    dati.ISS$pop09corr<-dati.ISS$pop09
    dati.ISS$pop09corr[dati.ISS$NoCV<0 & !is.na(dati.ISS$NoCV)]<-aa$Pop09corr #adjusted population
    dati.ISS$NoCVcorr<-dati.ISS$pop09corr-dati.ISS$PV1-dati.ISS$PV2-dati.ISS$PV3-dati.ISS$PV4 #adjusted population of non-vaccinated
    colnames(dati.ISS)
    
    #calculate IFR b age class, variant and vaccination coverage
    dati.ISS$IFRVarVac <-(p0+p1+p2+p3+p4+p5)/PVar*(dati.ISS$NoCVcorr+RR1+RR2+RR3+RR4)/dati.ISS$pop09corr
    dati.ISS$IFRVarVac1<-(p0+p1+p2+p3+p4+p5)/PVar*(dati.ISS$NoCVcorr+RR01+RR02+RR03+RR04)/dati.ISS$pop09corr
    dati.ISS$IFRVar    <-(p0+p1+p2+p3+p4+p5)/PVar
    dati.ISS$IFR0      <-dati.ISS$IFR
    
    #check
    PVar[!elementwise.all.equal(PVar, 1.00)]
    xx<-dati.ISS[!elementwise.all.equal(dati.ISS$NonVoc+dati.ISS$Alpha+dati.ISS$Beta+dati.ISS$Gamma+dati.ISS$Delta+dati.ISS$Omicron, 1.00) & dati.ISS$IDDate<=737,]
    
    #calculate IFR by age of each variant
    dati.ISS$IFRNonVOC<-ifelse(dati.ISS$NonVoc==0,0,p0/dati.ISS$NonVoc*(dati.ISS$NoCVcorr+RR1+RR2+RR3+RR4)/dati.ISS$pop09corr)
    dati.ISS$IFRAlfa  <-ifelse(dati.ISS$Alpha==0,0,p1/dati.ISS$Alpha*(dati.ISS$NoCVcorr+RR1+RR2+RR3+RR4)/dati.ISS$pop09corr)
    dati.ISS$IFRBeta  <-ifelse(dati.ISS$Beta==0,0,p2/dati.ISS$Beta*(dati.ISS$NoCVcorr+RR1+RR2+RR3+RR4)/dati.ISS$pop09corr)
    dati.ISS$IFRGamma <-ifelse(dati.ISS$Gamma==0,0,p3/dati.ISS$Gamma*(dati.ISS$NoCVcorr+RR1+RR2+RR3+RR4)/dati.ISS$pop09corr)
    dati.ISS$IFRDelta <-ifelse(dati.ISS$Delta==0,0,p4/dati.ISS$Delta*(dati.ISS$NoCVcorr+RR1+RR2+RR3+RR4)/dati.ISS$pop09corr)
    dati.ISS$IFROmic  <-ifelse(dati.ISS$Omicron==0,0,p5/dati.ISS$Omicron*(dati.ISS$NoCVcorr+RR1+RR2+RR3+RR4)/dati.ISS$pop09corr)
    
    #calculate IFR by age of each variant without vaccines
    dati.ISS$IFRNonVOC1<-ifelse(dati.ISS$NonVoc==0,0,p0/dati.ISS$NonVoc)
    dati.ISS$IFRAlfa1  <-ifelse(dati.ISS$Alpha==0,0,p1/dati.ISS$Alpha)
    dati.ISS$IFRBeta1  <-ifelse(dati.ISS$Beta==0,0,p2/dati.ISS$Beta)
    dati.ISS$IFRGamma1 <-ifelse(dati.ISS$Gamma==0,0,p3/dati.ISS$Gamma)
    dati.ISS$IFRDelta1 <-ifelse(dati.ISS$Delta==0,0,p4/dati.ISS$Delta)
    dati.ISS$IFROmic1  <-ifelse(dati.ISS$Omicron==0,0,p5/dati.ISS$Omicron)
    
    #add populations
    dati.ISS0<-merge(dati.ISS[dati.ISS$IDDate<737,],PopIFRTot,by.x="anno",by.y="anno")
    dati.ISS0$DPopG0<-dati.ISS0$pop09/dati.ISS0$popT
    dati.ISS0$DPopG<-dati.ISS0$pop59/dati.ISS0$popT

    #aggregate by age
    dati.ISS0<-aggregate(list(dati.ISS0$pop09corr,dati.ISS0$nWMA0,dati.ISS0$DPopG*dati.ISS0$IFR0,dati.ISS0$PernWMA0*dati.ISS0$IFR0,dati.ISS0$DPopG*dati.ISS0$IFRVar,dati.ISS0$PernWMA0*dati.ISS0$IFRVar,dati.ISS0$DPopG*dati.ISS0$IFRVarVac,dati.ISS0$PernWMA0*dati.ISS0$IFRVarVac,dati.ISS0$PernWMA0*dati.ISS0$IFRVarVac1,dati.ISS0$PernWMA0*dati.ISS0$IFRNonVOC,dati.ISS0$PernWMA0*dati.ISS0$IFRAlfa,dati.ISS0$PernWMA0*dati.ISS0$IFRBeta,dati.ISS0$PernWMA0*dati.ISS0$IFRGamma,dati.ISS0$PernWMA0*dati.ISS0$IFRDelta,dati.ISS0$PernWMA0*dati.ISS0$IFROmic,dati.ISS0$PernWMA0*dati.ISS0$NonVoc,dati.ISS0$PernWMA0*dati.ISS0$Alpha,dati.ISS0$PernWMA0*dati.ISS0$Beta,dati.ISS0$PernWMA0*dati.ISS0$Gamma,dati.ISS0$PernWMA0*dati.ISS0$Delta,dati.ISS0$PernWMA0*dati.ISS0$Omicron,dati.ISS0$PernWMA0*dati.ISS0$IFRNonVOC1,dati.ISS0$PernWMA0*dati.ISS0$IFRAlfa1,dati.ISS0$PernWMA0*dati.ISS0$IFRBeta1,dati.ISS0$PernWMA0*dati.ISS0$IFRGamma1,dati.ISS0$PernWMA0*dati.ISS0$IFRDelta1,dati.ISS0$PernWMA0*dati.ISS0$IFROmic1),list(dati.ISS0$date,dati.ISS0$IDDate),sum)
    colnames(dati.ISS0)<-c("date","IDDate","popcorr","nWMA0","IFRAtt1","IFR","IFRAttVar","IFRVar","IFRAttVarVac","IFRVarVac","IFRVarVac1","IFRVarVacNonVoc","IFRVarVacAlfa","IFRVarVacBeta","IFRVarVacGamma","IFRVarVacDelta","IFRVarVacOmicron","NonVoc","Alfa","Beta","Gamma","Delta","Omicron","IFRVarNonVoc","IFRVarAlfa","IFRVarBeta","IFRVarGamma","IFRVarDelta","IFRVarOmicron")
    dati.ISS0<-merge(dati.ISS0,EtaMed[,-c(1,4)],all.x=TRUE)
    dati.ISS0$anno<-substring(dati.ISS0$date,1,4)
    dati.ISS0$anno<-as.numeric(dati.ISS0$anno)
    colnames(dati.ISS0[,c(31,1:2,30,3:4,18:23,5:17,24:29)])
    dati.ISS0<-dati.ISS0[,c(31,1:2,30,3:4,18:23,5:17,24:29)]

    ############################################ smoothing dWMA around the periond of weights changing #############################################################
    date3 <-unique(Covid19all$date1[Covid19all$date=="2021-02-05"])
    cond<-Covid19all$IDReg==23
    j<-date3+1
    for (j in date3:(date3+5)){
      Covid19all$dWMA[Covid19all$date1==j & cond]<-Covid19all$dWMA[Covid19all$date1==date3 & cond]+(Covid19all$dWMA[Covid19all$date1==date3+5 & cond]-Covid19all$dWMA[Covid19all$date1==date3 & cond])*(j-date3)/5
    }
    Covid19all[Covid19all$date1>=date3 & Covid19all$date1<=(date3+5) & cond,c("date1","dWMA")]
    
    rm(list=c("xx","elementwise.all.equal","date3"))
    

    #########################################################Create Dataset Stime#################################################################
    Covid19all$anno<-as.numeric(Covid19all$anno)
    Stime<-merge(Covid19all[,c(1,3,7,5:6,14:15,2,21,22,24,8,9:12,16:20,23,25:26,28)],dati.ISS0[,c(3,7:31)],by.x="IDDate",by.y="IDDate",all.x=TRUE)
    colnames(Stime)
    colnames(Covid19all[,c(1,4,7,5:6,14:15,2,21,22,24,8,9:12,16:20,23,25:26,28)])
    colnames(Covid19all)
    colnames(dati.ISS0[,c(3,7:31)])
    colnames(dati.ISS0)
    

    #calulate expected infections through true and fictitious populations #######################################
    Stime$NAtt<-Stime$dWMA/Stime$IFRAtt
    Stime$NMed<-Stime$dWMA/Stime$IFRMed
    

    ##############Put FRISTS for the first period (to day 120°, i.e. 21/06/2020), then gradually (for 15 days) to IFR from  fictitious population (IFRMed), then IFRmed to day 289 (last 30 days learly to estimation from dati.ISS)#######################################
    Stime$IFR0<-Stime$IFRMed
    Stime$IFR0[Stime$date1<120]<-Stime$IFRISTAT[Stime$date1<120]
    
    #linearly from IFRISTAT to IFRMed (gg 120-135)
      for (j in 0:15) {
        Stime$IFR0[Stime$date1==120+j]<-Stime$IFRISTAT[Stime$date1==120]-j*(Stime$IFRISTAT[Stime$date1==120]-Stime$IFRMed[Stime$date1==135])/15
      }

    #from day 135 to 289 all equal to IFRMed e pongo uguali tutti gli IFR fino a 289 
    PerToISS2<- Stime$date1>0 & Stime$date1<289
    Stime$IFRVar[PerToISS2]<-Stime$IFRVarVac[PerToISS2]<-Stime$IFRVarVac1[PerToISS2]<-Stime$IFR0[PerToISS2]
    Stime$IFRAttVar[PerToISS2]<-Stime$IFRAttVarVac[PerToISS2]<-Stime$IFRAtt[PerToISS2]

    #IFR of Variants equal to 0 to day 289
    Stime$IFRVarVacAlfa[PerToISS2]<-Stime$IFRVarVacBeta[PerToISS2]<-Stime$IFRVarVacGamma[PerToISS2]<-Stime$IFRVarVacDelta[PerToISS2]<-Stime$IFRVarVacOmicron[PerToISS2]<-0
    Stime$IFRVarAlfa[PerToISS2]<-Stime$IFRVarBeta[PerToISS2]<-Stime$IFRVarGamma[PerToISS2]<-Stime$IFRVarDelta[PerToISS2]<-Stime$IFRVarOmicron[PerToISS2]<-0
    Stime$Alfa[PerToISS2]<-Stime$Beta[PerToISS2]<-Stime$Gamma[PerToISS2]<-Stime$Delta[PerToISS2]<-Stime$Omicron[PerToISS2]<-0
    Stime$NonVoc[PerToISS2]<-rep(1,length(Stime$NonVoc[PerToISS2]))

    #gradually from  day 260 to day 290)
      for (j in 0:20) {
        Stime$IFRVar[Stime$date1==269+j]<-Stime$IFRVar[Stime$date1==269]-j*(Stime$IFRVar[Stime$date1==269]-Stime$IFRVar[Stime$date1==289])/20
        Stime$IFRVarVac[Stime$date1==269+j]<-Stime$IFRVarVac[Stime$date1==269]-j*(Stime$IFRVarVac[Stime$date1==269]-Stime$IFRVarVac[Stime$date1==289])/20
        Stime$IFRVarVac1[Stime$date1==269+j]<-Stime$IFRVarVac1[Stime$date1==269]-j*(Stime$IFRVarVac1[Stime$date1==269]-Stime$IFRVarVac1[Stime$date1==289])/20
       }
    Stime$IFRVarVacNonVoc[PerToISS2]<-Stime$IFRVarVac[PerToISS2]
    Stime$IFRVarNonVoc[PerToISS2]<-Stime$IFRVar[PerToISS2]
    
    Stime$IFR0[Stime$date1<=290]
    Stime$IFRMed[Stime$date1<=290]
    Stime$IFRVar[Stime$date1>=269 & Stime$date1<=290]
    
    Stime$IFRVarVac[Stime$date1>=269 & Stime$date1<=290]
    

    #######################################Estimate of number of infections #######################################
    Stime$NVar<-Stime$dWMA/Stime$IFRVar
    Stime$NVarVacc<-Stime$dWMA/Stime$IFRVarVac
    Stime$NVarVaccUB<-qnbinom(0.975, Stime$dWMA, Stime$IFRVarVac, lower.tail = TRUE, log.p = FALSE)+Stime$dWMA
    Stime$NVarVaccLB<-qnbinom(0.975, Stime$dWMA, Stime$IFRVarVac, lower.tail = FALSE, log.p = FALSE)+Stime$dWMA
    Stime$NVarVacc1<-Stime$dWMA/Stime$IFRVarVac1
    Stime$NVarVacc1UB<-qnbinom(0.975, Stime$dWMA, Stime$IFRVarVac1, lower.tail = TRUE, log.p = FALSE)+Stime$dWMA
    Stime$NVarVacc1LB<-qnbinom(0.975, Stime$dWMA, Stime$IFRVarVac1, lower.tail = FALSE, log.p = FALSE)+Stime$dWMA
    Stime<-Stime[!is.na(Stime$dWMA),]
    
    rm(list=c("PopNeg","PerToISS2","cond","j"))
    
    
