############################################transparent funtion#############################################################
t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  
  ## Save the color
  invisible(t.col)
}
write.csv(dati.ISS[dati.ISS$IDReg==23,],file="C:\\Users\\xf49652\\OneDrive - INAIL\\Pierpaolo\\COVID-19\\R\\R\\dati.ISS.csv")
library (ggplot2)
library(grid)
library(gridExtra)
Pop[Pop$codreg==23,c("anno","etaMediana")]


############################################Plot 3: deaths #############################################################
colTest10<-t_col(col="blue",percent = 98)
colTest1<-t_col(col="blue",percent = 90)
colVacc1<-t_col(col="blue",percent = 85)
colVacc0<-t_col(col="blue",percent = 65)
colOpen<-t_col(col="green",percent = 55)
colsch<-t_col(col="green",percent = 85)
colClosure<-t_col(col="red",percent = 55)
colsafe<-t_col(col="blue",percent = 25)
mycol<-t_col(col="red4",percent = 55)
mycol1<-t_col(col="grey",percent = 75)
mycol2<-t_col(col="black",percent = 75)
mycol3<-t_col(col="black",percent = 90)
mycol4<-t_col(col="grey",percent = 85)

#graphics.off() 
par("mar") 
par(mar=c(2.5,2.5,2.5,2.5))

definizione<-Stime$IDDate<=736
#lines(Covid19all$date1[Covid19all$codreg==23],Covid19all$d[Covid19all$codreg==23],type="l",xlab="date",ylab="deaths",lty=2,col="grey")
plot(Stime$IDDate[definizione],Stime$IFRVar[definizione]*Stime$NVarVacc[definizione],type="l",xlab="date",ylab="deaths",cex.main=1,ylim=c(0,2100),col=colVacc1,xaxt="n")
#lines(Stime$date1[Stime$IDReg==23],Stime$IFR00[Stime$IDReg==23]*Stime$NVarVacc[Stime$IDReg==23],type="l",xlab="date",ylab="deaths",cex.main=1,ylim=c(0,2100),col=colVacc1,xaxt="n")
#lines(Stime$date1[Stime$IDReg==23],Stime$IFRAtt[Stime$IDReg==23]*Stime$NVarVacc[Stime$IDReg==23],type="l",xlab="date",ylab="deaths",cex.main=1,ylim=c(0,2100),col=colVacc1,xaxt="n")
#lines(Stime$date1[Stime$IDReg==23],Stime$IFRVar00[Stime$IDReg==23]*Stime$NVarVacc[Stime$IDReg==23],xlab="date",ylab="deaths",cex.main=1,ylim=c(0,2100),col="gold",xaxt="n")

lines(Stime$date1[definizione],Stime$dWMA[definizione],type="l",xaxt="n")
lines(Stime$date1[definizione],Stime$IFRVarVac[definizione]*Stime$NVarVacc[definizione],type="l",xlab="date",ylab="deaths",cex.main=1)
lines(Stime$date1[definizione],Stime$IFRVar[definizione]*Stime$NVarVacc1[definizione],type="l",xlab="date",ylab="deaths",cex.main=1,col=colTest1)
#lines(Stime$date1[definizione],10*Stime$etamediana[definizione],type="p",col=mycol2,pch = 19,cex=0.75)
lines(Stime$date1[definizione],Stime$d[definizione],type="l",xlab="date",ylab="deaths",cex.main=1,col=mycol1,lty=2)
#lines(Covid19all$date1[Covid19all$codreg==23],Covid19all$IFRMedCorr[Covid19all$codreg==23]*(Covid19all$NMedCorr1Vacc[Covid19all$codreg==23]+1)-1,type="l",xlab="date",ylab="deaths",cex.main=1,ylim=c(0,2000),col="green")
#lines(Covid19all$date1[Covid19all$codreg==23],Covid19all$TestSett[Covid19all$codreg==23]/1000,type="l",xlab="date",ylab="deaths",cex.main=1,ylim=c(0,2000),col="brown")
#lines(Covid19all$date1[Covid19all$codreg==23],Covid19all$IFRMedCorr00[Covid19all$codreg==23]*(Covid19all$NMedCorr00Vacc[Covid19all$codreg==23]+1)-1,type="l",xlab="date",ylab="deaths",cex.main=1,ylim=c(0,2000),col="brown")
#lines(Covid19all$date1[Covid19all$codreg==23],Covid19all$IFRMedCorr[Covid19all$codreg==23]*Covid19all$NMedCorr1Vacc[Covid19all$codreg==23],type="l",xlab="date",ylab="deaths",cex.main=1,col="red")
#lines(Covid19all$date1[Covid19all$codreg==23],Covid19all$IFRMed[Covid19all$codreg==23]*Covid19all$NMedCorrVacc[Covid19all$codreg==23],type="l",xlab="date",ylab="deaths",cex.main=1,col="orange")
#lines(Covid19all$date1[Covid19all$codreg==23],Covid19all$IFR[Covid19all$codreg==23]*Covid19all$N[Covid19all$codreg==23],xaxt="n",type="l",xlab="date",ylab="deaths",cex.lab=1,cex.axis=1,main = unique(Covid19all$region[Covid19all$codreg==j]),cex.main=1,col="green")
lines(c(127,132),c(1880,1880),col="black",lty=1)
text(164,1880,"deaths by infection day",col="black",cex=0.75)
lines(c(127,132),c(1837,1837),col=mycol1,lty=2)
text(168,1840,"deaths by occurrence day",col="black",cex=0.75)
#points(131.5,895,pch = 16,col="red",cex=0.75)
#text(162,895,"Min number of daily deaths",col="black",cex=0.75)
#text(151,865,"(on 2020/08/01)",col="black",cex=0.75)
rect(127, 1780, 132, 1795, col = mycol,border=NA)
text(154,1790,"deaths excess",col="black",cex=0.75)
rect(127, 1735, 132, 1750, col = colVacc1,border=NA)
text(163,1750,"lifes saved by vaccine",col="black",cex=0.75)
text(165,1713,"among infected people",col="black",cex=0.75)
rect(127, 1660, 132, 1665, col = colTest1,border=NA)
text(181,1665,"life saved by vaccine among people",col="black",cex=0.75)
text(174,1618,"who would have been infected",col="black",cex=0.75)
lines(c(127,132),c(1565,1565),col="red")
text(160,1567,"restrictive meaures",cex=0.75)
lines(c(127,132),c(1515,1515),col="green")
text(162,1517,"permissive meaures",cex=0.75)

lines(c(0,342),c(2100,2100))
text((342/2),2050,"non-VOCs")

lines(c(344,487),c(2100,2100))
text((344+(487-344)/2),2050,"Alpha")

lines(c(489,666),c(2100,2100))
text((489+(666-489)/2),2050,"Delta")

lines(c(668,736),c(2100,2100))
text((668+(743-668)/2),2050,"Omicron")
text(743/2,2135,"Prevalent Variants",font=2)

#lines(c(308,308),c(0,2100), lwd=2,lty=2)
#text(312,100,"begining of accine campaign ",cex=0.75, srt=90)

points(c(11,11),c(0,1970),type="l",col=colClosure)
text(10,1200,"school closure",col="black", srt=90,cex=0.75)
points(c(18,18),c(0,1970),type="l",col=colClosure)
text(18,1206,"stay at home",col="black", srt=90,cex=0.75)
points(c(29,29),c(0,1970),type="l",col=colClosure)
text(28.5,1207,"industrial lockdown",col="black", srt=90,cex=0.75)
points(c(84,84),c(0,1970),type="l",col=colOpen)
text(83,1200,"phase 2 - step 1: free  mobility within regions",col="black", srt=90,cex=0.75)
points(c(102,102),c(0,1970),type="l",col=colOpen)
text(101,1200,"phase 2 - step 2: free mobility between regions",col="black", srt=90,cex=0.75)
rect(204, 0, 214, 1970, col = colsch,border=NA)
text(208,1200,"school opening",col="black", srt=90,cex=0.75)
#points(c(204,204),c(0,1900),type="l",col=colsch)
#text(203,500,"school opening in 14 regions out of 20",col="black",cex=0.75, srt=90)
#points(c(214,214),c(0,1900),type="l",col="colOpen")
#text(214,500,"school opening in the remaining 6 regions",col="black",cex=0.75, srt=90)
points(c(244,244),c(0,1970),type="l",col=colClosure)
text(244,1200,"75% of distance learning in high school",col="black", srt=90,cex=0.75)
points(c(256,256),c(0,1970),type="l",col=colClosure)
text(256,1200,"regional restrictions according to Rt",col="black", srt=90,cex=0.75)
points(c(289,289),c(0,1970),type="l",col=colOpen)
text(289,1000,"Incentives to in-store christmas shopping",col="black", srt=90,cex=0.75)
points(c(301,301),c(0,1970),type="l",col=colClosure)
text(301,1050,"No mobility between regions",col="black", srt=90,cex=0.75)
points(c(306,306),c(0,1970),type="l",col=colClosure)
text(306,1050,"No mobility with 1 visit per day to parents within municipalities",col="black", srt=90,cex=0.75)
points(c(318,318),c(0,1970),type="l",col=colOpen)
text(318,1050,"No mobility between regions but regional restrictions according to Rt",col="black", srt=90,cex=0.75)
rect(330, 0, 344, 1970, col = colsch,border=NA)
text(337,1200,"high school opening (50-75% in precence)",col="black", srt=90,cex=0.75)
#points(c(337,337),c(0,1000),type="l",col="colOpen")
#text(337,500,"high school opening (50-75% in precence) in other 4 regions",col="black",cex=0.75, srt=90)
#points(c(344,344),c(0,1000),type="l",col="colOpen")
#text(344,500,"high school opening (50-75% in precence) in remaining 8 regions",col="black",cex=0.75, srt=90)
#points(c(377,377),c(0,1000),type="l",col="colClosure")
#text(377,500,"distance learning according to Rt (in red areas)",col="black",cex=0.75, srt=90)
points(c(386,386),c(0,1970),type="l",col=colClosure)
text(386,1200.8,"More restrictions on mobility (equal rules between yellow and orande regions until Easter)",col="black", srt=90,cex=0.75)
points(c(425,425),c(0,1970),type="l",col=colOpen)
text(425,1200,"put back of yellow area (with curfew 22.00-05.00) and introduction of free pass",col="black", srt=90,cex=0.75)
points(c(455,455),c(0,1970),type="l",col=colOpen)
text(455,1200,"in yellow area: gym opening",col="black", srt=90,cex=0.75)
points(c(464,464),c(0,1970),type="l",col=colOpen)
text(464,1200,"in yellow area: indoor catering",col="black", srt=90,cex=0.75)
points(c(484,484),c(0,1970),type="l",col=colOpen)
text(484,1200,"in yellow area: No curfew",col="black", srt=90,cex=0.75)
rect(499, 0, 504, 1970, col = colsch,border=NA)
text(501,1200,"semi-final and final of 2020 UEFA European Football Championship",col="black", srt=90,cex=0.75)
#points(c(499,499),c(0,1000),type="l",col="green")
#text(499,500,"Italy wins european championship semi-final",col="black",cex=0.75, srt=90)
#points(c(504,504),c(0,1000),type="l",col="green")
#text(504,500,"Italy wins european championship",col="black",cex=0.75, srt=90)
#points(c(506,506),c(0,1000),type="l",col="green")
points(c(530,530),c(0,1970),type="l",col=colClosure)
text(530,1200,"compulsory green pass for many group activities",col="black", srt=90,cex=0.75)
rect(568, 0, 575,1970, col = colsch,border=NA)
text(570.5,1200,"school opening",col="black", srt=90,cex=0.75)
#points(c(568,568),c(0,1000),type="l",col="green")
###text(568,500,"school opening (11 regions)",col="black",cex=0.75, srt=90)
##points(c(570,570),c(0,1000),type="l",col="green")
#text(570,500,"school opening (7 regions)",col="black",cex=0.75, srt=90)
#points(c(575,575),c(0,1000),type="l",col="green")
#text(575,500,"school opening (2 regions)",col="black",cex=0.75, srt=90)
points(c(600,600),c(0,1970),type="l",col=colsch)
text(600,1200,"back to working in presence",col="black", srt=90,cex=0.75)
points(c(643,643),c(0,1970),type="l",col=colClosure)
text(643,1200,"extension of mandatory vaccination and of green pass",col="black", srt=90,cex=0.75)
points(c(673,673),c(0,1970),type="l",col=colClosure)
text(673,1200,"mandatory ffp2 on public transport",col="black", srt=90,cex=0.75)
rect(684, 0, 687,1970, col = colsch,border=NA)
text(685,1200,"school opening",col="black", srt=90,cex=0.75)
rect(310,0,736,1970,col = colTest10,border=FALSE)
#rect(463,1955,540,1900,col = "white",border=FALSE)  
text(296+(736-307)/2.2,1900,"Vaccine  Campaign",col=colVacc1,cex=1,font=4,lwd=2)

lines(c(147,147),c(-30,1500),lty=2,lwd=2)
lines(c(147,147),c(1900,1970),lty=2,lwd=2)

lines(c(347,347),c(-30,1970),lty=2,lwd=2)
lines(c(486,486),c(-30,1970),lty=2,lwd=2)
lines(c(586,586),c(-30,1970),lty=2,lwd=2)

text(147/2,-15,"1-th wave",cex=0.75)
text(147+(347-147)/2,-15,"2-th wave",cex=0.75)
text(347+(486-347)/2,-15,"3-th wave",cex=0.75)
text(486+(586-486)/2,-15,"4-th wave",cex=0.75)
text(586+(734-586)/2,-15,"5-th wave",cex=0.75)


xx1<-c(1,1:138)
yy1<-Stime$IFRAtt[Stime$date1>=1 & Stime$date1<=138]*Stime$NVarVacc[Stime$date1>=1 & Stime$date1<=138]
yy10<-min(Stime$IFRAtt[Stime$date1>=1 & Stime$date1<=138]*Stime$NVarVacc[Stime$date1>=1 & Stime$date1<=138])
yy2<-Stime$dWMA[Stime$date1>=1 & Stime$date1<=138]
yy20<-min(Stime$dWMA[Stime$date1>=1 & Stime$date1<=138])
xx<-c(xx1,xx1)
yy<-c(yy10,yy1,yy20,yy2)
polygon(xx, yy, col = mycol ,border=FALSE)
axis(1, at=c(7,22,38,53,68,83,99,114,129,144,160,175,191,206,221,236,252,267,282,297,313,327,344,358,372,386,403,417,433,447,464,478,494,508,525,539,556,570,586,600,617,631,646,661,676,691,706,721,734,749),labels=c("mar","","apr","","may","","jun","","jul","","aug","","sep","","oct","","nov","","dec","","gen","","feb","","ma","","apr","","mag","","giu","","lug","","ago","","set","","ott","","nov","","dic","","gen","","feb","","mar",""),xlab="date",cex.axis=0.75)

a<-Stime$date1>180 & Stime$date1<343 & Stime$dWMA>Stime$IFRAttVar*Stime$NVarVacc
xx1<-c(250,Stime$date1[a])
yy1<-Stime$IFRAttVar[a]*Stime$NVarVacc[a] #punti inferiori superficie da colorare
yy10<-min(c(Stime$IFRAttVar[a]*Stime$NVarVacc[a],Stime$dWMA[a])) #ultimo punto a destra della superficie da colorare
yy2<-Stime$dWMA[a] #punti superiori superficie da colorare
yy20<-min(c(Stime$IFRAttVar[a]*Stime$NVarVacc[a],Stime$dWMA[a])) #ultimo punto a destra della superficie da colorare
xx<-c(xx1,xx1)
yy<-c(yy10,yy1,yy20,yy2)
polygon(xx, yy, col = mycol ,border=FALSE)
axis(1, at=c(7,22,38,53,68,83,99,114,129,144,160,175,191,206,221,236,252,267,282,297,313,327,344,358,372,386,403,417,433,447,464,478,494,508,525,539,556,570,586,600,617,631,646,661,676,691,706,721,736,749),labels=c("mar","","apr","","may","","jun","","jul","","aug","","sep","","oct","","nov","","dec","","gen","","feb","","ma","","apr","","mag","","giu","","lug","","ago","","set","","ott","","nov","","dic","","gen","","feb","","mar",""),xlab="date",cex.axis=0.75)

sup<-max(Stime$date1[!is.na(Stime$dWMA)])
xx3<-c(308,308:736)
yy3<-c(0,Stime$IFRVar[Stime$date1>=309 & definizione]*Stime$NVarVacc[Stime$date1>=309 & definizione],0)
yy4<-c(0,Stime$dWMA[Stime$date1>=309 & definizione],0)
xx<-c(xx3,xx3)
yy<-c(yy3,yy4)
polygon(xx, yy, col =colVacc1 ,border=NA)

sup<-max(Stime$date1[!is.na(Stime$dWMA)])
xx3<-c(308,308:736)
yy3<-c(0,Stime$IFRVar[Stime$date1>=309 & definizione]*Stime$NVarVacc1[Stime$date1>=309 & definizione],0)
yy4<-c(0,Stime$dWMA[Stime$date1>=309 & definizione],0)
xx<-c(xx3,xx3)
yy<-c(yy3,yy4)
polygon(xx, yy, col =colTest1,border=NA)
