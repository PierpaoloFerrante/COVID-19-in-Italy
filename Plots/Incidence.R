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

library (ggplot2)
library(grid)
library(gridExtra)
graphics.off() 
par("mar") 
par(mar=c(2.5,2.5,2.5,2.5))

############################################ Plot 1: Incidence#############################################################
mycol2<-t_col(col="grey",percent = 65)
mycol0<-t_col(col="grey",percent = 55)
mycol<-t_col(col="red4",percent = 55)
mycol1<-t_col(col="grey",percent = 85)
colOpen<-t_col(col="green",percent = 55)
colsch<-t_col(col="green",percent = 85)
colClosure<-t_col(col="red",percent = 55)
colTest10<-t_col(col="blue",percent = 98)
colTest1<-t_col(col="blue",percent = 90)
colVacc1<-t_col(col="blue",percent = 85)

#plot(Stime$date1[Stime$IDReg==23],Covid19all$testWMA[Stime$IDReg==23]/2,lty=2,col="grey")
par(mar=c(2.5,0.5,2.5,2.5))


range<-Stime$date1<=736
plot(Stime$date1[range],Stime$NVarVacc1UB[range],type="l",xlab="date",ylab="cases (x 1000)",yaxt="n",xaxt="n",main="Incidence of COVID-19 in Italy (2020-2022)",col="white",ylim=c(0,400000))
lines(Stime$date1[range],Stime$NVarVacc1[range],type="l",main="Incidence of COVID-19 in Italy (2020-2022)",col=mycol0)
lines(Stime$date1[range],Stime$NVarVacc[range],type="l",col="grey")
lines(Stime$date1[range],Stime$nWMA[range],type="l",cex.lab=1,cex.axis=1,cex.main=1,col=mycol,lty=2)


text(140,345000,"daily incidence",col="black",cex=0.75)
text(137,340000,"with 95% CI",col="black",cex=0.75)
points(c(109.7,114.3),c(345000,345000),type="l",col=mycol0)
points(c(110,113.6),c(345000,345000),pch=15,col=mycol0)

text(155,330000,"daily incidence with 95% CI",col="black",cex=0.75)
text(140,325000,"without vaccine",col="black",cex=0.75)
points(c(110,114),c(330000,330000),type="l",col=mycol1)
points(c(110,113.7),c(330000,330000),pch=15,col=mycol1)

points(c(110,114),c(315800,315800),type="l",col=mycol,lty=2)
text(138,316000,"detected cases",cex=0.75)
points(112,305000,col="black",pch = 19)
text(136,305000,"Local Minima",cex=0.75)
lines(c(110,114),c(290000,290000),lty=2,lwd=2)
text(136,290000,"end of wave",cex=0.75)

lines(c(0,342),c(400000,400000))
text((342/2),395000,"non-VOCs")

lines(c(344,487),c(400000,400000))
text((344+(487-344)/2),395000,"Alpha")

lines(c(489,666),c(400000,400000))
text((489+(666-489)/2),395000,"Delta")

lines(c(668,743),c(400000,400000))
text((668+(743-668)/2),395000,"Omicron")
text(743/2,405000,"Prevalent VOCs",font=2)

rect(310,0,736,385000,col = colTest10,border=FALSE)
text(307+(736-307)/2.2,380000,"Vaccine  Campaign",col=colVacc1,cex=1,font=4,lwd=2)

#rect(310,-30000,736,0000,col = colTest10,border=FALSE)
#text(307+(736-307)/2.2,-7500,"Vaccine  Campaign",col=colVacc1,cex=1,font=4,lwd=2)


points(c(10,10),c(0,385000),type="l",col=colClosure)
text(9,250000,"school closure",col="black",cex=0.75, srt=90)
points(c(18,18),c(0,385000),type="l",col=colClosure)
text(18,250000,"stay at home",col="black",cex=0.75, srt=90)
points(c(29,29),c(0,385000),type="l",col=colClosure)
text(28.5,250000,"industrial lockdown",col="black",cex=0.75, srt=90)
points(c(84,84),c(0,385000),type="l",col=colOpen)
text(83,250000,"phase 2 - step 1: free  mobility within regions",col="black",cex=0.75, srt=90)
points(c(102,102),c(0,385000),type="l",col=colOpen)
text(101,250000,"phase 2 - step 2: free mobility between regions",col="black",cex=0.75, srt=90)
points(c(126,126),c(0,516),type="l",col=colOpen)
text(101,250000,"phase 2 - step 2: free mobility between regions",col="black",cex=0.75, srt=90)

rect(204, 0, 214, 385000, col = colsch,border=NA)
text(208,200000,"school opening",col="black",cex=0.75, srt=90)
#points(c(204,204),c(0,80000),type="l",col=colsch)
#text(203,40000,"school opening in 14 regions out of 20",col="black",cex=0.75, srt=90)
#points(c(214,214),c(0,80000),type="l",col="colOpen")
#text(214,40000,"school opening in the remaining 6 regions",col="black",cex=0.75, srt=90)
points(c(244,244),c(0,385000),type="l",col=colClosure)
text(244,200000,"75% of distance learning in high school",col="black",cex=0.75, srt=90)
points(c(256,256),c(0,385000),type="l",col=colClosure)
text(256,205000,"regional restrictions according to Rt",col="black",cex=0.75, srt=90)
points(c(289,289),c(0,385000),type="l",col=colOpen)
text(289,250000,"Incentives to in-store christmas shopping",col="black",cex=0.75, srt=90)
points(c(301,301),c(0,385000),type="l",col=colClosure)
text(299,200000,"No mobility between regions",col="black",cex=0.75, srt=90)
points(c(305,305),c(0,385000),type="l",col=colClosure)
text(306,200000,"No mobility but 1 visit per day to parents within municipalities",col="black",cex=0.75, srt=90)
points(c(318,318),c(0,385000),type="l",col=colOpen)
text(318,200000,"No mobility between regions and regional restrictions according to Rt",col="black",cex=0.75, srt=90)
rect(330, 0, 344, 385000, col = colsch,border=NA)
text(337,200000,"high school opening (50-75% in precence)",col="black",cex=0.75, srt=90)
#points(c(337,337),c(0,80000),type="l",col="colOpen")
#text(337,40000,"high school opening (50-75% in precence) in other 4 regions",col="black",cex=0.75, srt=90)
#points(c(344,344),c(0,80000),type="l",col="colOpen")
#text(344,40000,"high school opening (50-75% in precence) in remaining 8 regions",col="black",cex=0.75, srt=90)
#points(c(377,377),c(0,80000),type="l",col="colClosure")
#text(377,40000,"distance learning according to Rt (in red areas)",col="black",cex=0.75, srt=90)
points(c(386,386),c(0,385000),type="l",col=colClosure)
text(386,205000,"More restrictions on mobility (equal rules between yellow and orande regions until Easter)",col="black",cex=0.75, srt=90)
points(c(425,425),c(0,385000),type="l",col=colOpen)
text(425,205000,"put back of yellow area (with curfew 22.00-05.00) and introduction of free pass",col="black",cex=0.75, srt=90)
points(c(455,455),c(0,385000),type="l",col=colOpen)
text(455,200000,"in yellow area: gym opening",col="black",cex=0.75, srt=90)
points(c(464,464),c(0,385000),type="l",col=colOpen)
text(464,200000,"in yellow area: indoor catering",col="black",cex=0.75, srt=90)
points(c(484,484),c(0,385000),type="l",col=colOpen)
text(484,200000,"in yellow area: No curfew",col="black",cex=0.75, srt=90)
rect(499, 0, 504, 385000, col = colsch,border=NA)
text(501,200000,"semi-final and final of 2020 UEFA European Football Championship",col="black",cex=0.75, srt=90)
#points(c(499,499),c(0,80000),type="l",col="green")
#text(499,40000,"Italy wins european championship semi-final",col="black",cex=0.75, srt=90)
#points(c(504,504),c(0,80000),type="l",col="green")
#text(504,40000,"Italy wins european championship",col="black",cex=0.75, srt=90)
#points(c(506,506),c(0,80000),type="l",col="green")
points(c(530,530),c(0,385000),type="l",col=colClosure)
text(530,200000,"compulsory green pass for many group activities",col="black",cex=0.75, srt=90)
rect(568, 0, 575, 385000, col = colsch,border=NA)
text(570.5,200000,"school opening",col="black",cex=0.75, srt=90)
#points(c(568,568),c(0,70000),type="l",col="green")
###text(568,40000,"school opening (11 regions)",col="black",cex=0.75, srt=90)
##points(c(570,570),c(0,70000),type="l",col="green")
#text(570,40000,"school opening (7 regions)",col="black",cex=0.75, srt=90)
#points(c(575,575),c(0,70000),type="l",col="green")
#text(575,30000,"school opening (2 regions)",col="black",cex=0.75, srt=90)
points(c(600,600),c(0,385000),type="l",col=colsch)
text(600,200000,"back to work in office",col="black",cex=0.75, srt=90)
points(c(643,643),c(0,385000),type="l",col=colClosure)
text(643,200000,"extension of mandatory vaccination and of green pass",col="black",cex=0.75, srt=90)
points(c(673,673),c(0,385000),type="l",col=colClosure)
rect(684, 0, 687,385000, col = colsch,border=NA)
text(685,200000,"school opening",col="black", srt=90,cex=0.75)
text(673,200000,"mandatory ffp2 on public transport",col="black",cex=0.75, srt=90)
axis(1, at=c(7,22,38,53,68,83,99,114,129,144,160,175,191,206,221,236,252,267,282,297,313,327,344,358,372,386,403,417,433,447,464,478,494,508,525,539,556,570,586,600,617,631,646,661,676,691,706,721,736),labels=c("mar","","apr","","may","","jun","","jul","","aug","","sep","","oct","","nov","","dec","","gen","","feb","","ma","","apr","","mag","","giu","","lug","","ago","","set","","ott","","nov","","dic","","gen","","feb","",""),xlab="date",cex.axis=0.95)
axis(2, at=c(50000,100000,150000,200000,250000,300000,350000,400000),labels=c("50","100","150","200","250","300","350","400"),cex.axis=0.95)

points(147,516,col="black",pch = 19)
lines(c(147,147),c(-150,385000),lty=2,lwd=2)
points(341,21263,col="black",pch = 19)
lines(c(341,341),c(-150,385000),lty=2,lwd=2)
points(486,2573,col="black",pch = 19)
lines(c(486,486),c(-150,385000),lty=2,lwd=2)
points(586,3810,col="black",pch = 19)
lines(c(586,586),c(-150,385000),lty=2,lwd=2)

text(147/2,-150,"1-th wave",cex=0.75)
text(147+(347-147)/2,-150,"2-th wave",cex=0.75)
text(347+(486-347)/2,-150,"3-th wave",cex=0.75)
text(486+(586-486)/2,-150,"4-th wave",cex=0.75)
text(586+(734-586)/2,-150,"5-th wave",cex=0.75)

xx3<-c(0,0:736)
yy3<-c(0,Stime$NVarVaccUB[range],0)
yy4<-c(0,Stime$NVarVaccLB[range],0)
xx<-c(xx3,xx3)
yy<-c(yy3,yy4)
polygon(xx, yy, col =mycol0,border=NA)

xx3<-c(360,360:736)
yy3<-c(360,Stime$NVarVacc1UB[Stime$date1>360 & range],0)
yy4<-c(360,Stime$NVarVacc1LB[Stime$date1>360 & range],0)
xx<-c(xx3,xx3)
yy<-c(yy3,yy4)
polygon(xx, yy, col=mycol1,border=NA)

