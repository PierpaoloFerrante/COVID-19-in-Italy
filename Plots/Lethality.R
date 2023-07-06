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
colClosure<-t_col(col="red",percent = 55)
mycol4<-t_col(col="grey",percent = 85)
mycol5<-t_col(col="grey",percent = 45)
colTest10<-t_col(col="blue",percent = 98)
colTest1<-t_col(col="blue",percent = 90)
mycol2<-t_col(col="grey",percent = 65)

Stime[Stime$IFRVarVac>0.0099 & Stime$IDReg==23 & Stime$date1>580,]
graphics.off() 
par("mar") 
par(mar=c(4.5,4.5,2.5,4.5))



############################################Lethality Italia#############################################################
par(mar=c(4.5,4.5,2.5,4.5))
mycol3<-t_col(col="black",percent = 90)
definizione<-Stime$IDDate<=736
mycol00<-t_col(col="grey",percent = 45)

plot(Stime$IDDate[Stime$IDDate<308],Stime$IFRVar[Stime$IDDate<308],type="l",yaxt="n",xaxt="n",xlab="date",ylab=" lethality (%)",ylim=c(0,0.035),xlim=c(0,760),lwd=3)
lines(Stime$IDDate[definizione & Stime$IDDate>=308],Stime$IFRVar[definizione & Stime$IDDate>=308],type="l",xaxt="n",xlab="date",ylab="daily lethality",ylim=c(0,0.03),lty=2,lwd=0.5)
lines(Stime$IDDate[definizione & Stime$IDDate>=308],Stime$IFRVarVac[definizione & Stime$IDDate>=308],type="l",lwd=3)
lines(Stime$IDDate[Stime$IDDate<308],Stime$IFRAttVar[Stime$IDDate<308],type="l",xaxt="n",xlab="date",ylab="daily lethality",ylim=c(0,0.035),xlim=c(0,760),col=colClosure)
lines(Stime$IDDate[definizione & Stime$IDDate>=308],Stime$IFRAttVar[definizione & Stime$IDDate>=308],type="l",xaxt="n",xlab="date",ylab="daily lethality",ylim=c(0,0.035),xlim=c(0,760),col=colClosure,lty=2)
lines(Stime$IDDate[definizione & Stime$IDDate>=308],Stime$IFRAttVarVac[definizione & Stime$IDDate>=308],type="l",xaxt="n",xlab="date",ylab="daily lethality",ylim=c(0,0.035),xlim=c(0,760),col=colClosure)

#lines(Stime$date1[Stime$IDReg==23 & Stime$date1>=270],Stime$IFRVarVac1[Stime$IDReg==23 & Stime$date1>=270],col="red",lty=2)
lines(Stime$date1[definizione],Stime$etamediana[definizione]/5000,type="p",col=mycol3,pch = 19,cex=0.75)
Stime$date1[Stime$date=="2021-02-01"]
lines(c(0,344),c(0.0345,0.0345))
text((345/2),0.034,"non-VOCs")

lines(c(346,487),c(0.0345,0.0345))
text((346+(487-336)/2),0.034,"Alpha")

lines(c(489,666),c(0.0345,0.0345))
text((489+(666-489)/2),0.034,"Delta")

lines(c(668,736),c(0.0345,0.0345))
text((668+(736-668)/2),0.034,"Omicron")
text(736/2,0.035,"Prevalent Variants",font=2)

#lines(c(0,800),c(0.00933,0.00933))
#lines(c(0,800),c(0.00939,0.00939))
#lines(c(0,800),c(0.0096,0.0096))
axis(side=4, at=seq(from=0.004,to=0.014,by=0.001), labels=seq(from=20,to=70,by=5),xlab="Age (years)",col=mycol2, col.axis = mycol2,pos=745)
text(775,0.009,"years",col=mycol2,cex=0.85, srt=90)

lines(c(160,165),c(0.032,0.032))
text(230,0.032,"death probability for infected people",cex=0.75)
lines(c(160,165),c(0.031,0.031),col="grey",lty=2)
text(225,0.031,"hypothetical death probability for",cex=0.75)
text(225,0.0302,"infected people without vaccines",cex=0.75)
lines(c(160,165),c(0.029,0.029),col="red")
text(251,0.029,"threshold of death probability for infected people",cex=0.75)
lines(c(160,165),c(0.028,0.028),col="red",lty=2)
text(240,0.028,"hypothetical threshold of death probability",cex=0.75)
text(230,0.0272,"for infected people without vaccines",cex=0.75)
#lines(c(160,165),c(0.028,0.028),lty=2,col=colClosure)
#text(215,0.028,"max hypothetical death probability for",cex=0.75)
#text(210,0.0272,"infected people without vaccines",cex=0.75)
points(162.5,0.026,col=mycol3,pch = 19)
text(222,0.026,"median age of detected cases",cex=0.75)

#lines(c(289,289),c(0,0.05),col="red")
#text(289,0.025,"begining of VOCS data ",cex=0.75, srt=90)

#Stime$date1[Stime$date=="2020-12-27"]
#lines(c(308,308),c(0,0.0325), lwd=2,lty=2)

rect(308,0,736,0.0325,col = colTest10,border=FALSE)
#rect(463,0.0325,540,0.0317,col = "white",border=FALSE)  
text(296+(736-307)/2.2,0.0319,"Vaccine  Campaign",col=colTest1,cex=1,font=4)


lines(c(147,147),c(-0.00035,0.0325),lty=2,lwd=2)
text(147/2,-0.0003,"1-th wave",cex=0.75)
lines(c(347,347),c(-0.00035,0.0325),lty=2,lwd=2)
text(147+(347-147)/2,-0.0003,"2-th wave",cex=0.75)
lines(c(486,486),c(-0.00035,0.0325),lty=2,lwd=2)
text(347+(486-347)/2,-0.0003,"3-th wave",cex=0.75)
lines(c(586,586),c(-0.00035,0.0325),lty=2,lwd=2)
text(486+(586-486)/2,-0.0003,"4-th wave",cex=0.75)
text(586+(734-586)/2,-0.0003,"5-th wave",cex=0.75)

axis(1, at=c(7,22,38,53,68,83,99,114,129,144,160,175,191,206,221,236,252,267,282,297,313,327,344,358,372,386,403,417,433,447,464,478,494,508,525,539,556,570,586,600,617,631,646,661,676,691,706,721,734),labels=c("mar","","apr","","may","","jun","","jul","","aug","","sep","","oct","","nov","","dec","","gen","","feb","","ma","","apr","","mag","","giu","","lug","","ago","","set","","ott","","nov","","dic","","gen","","feb","","mar"),xlab="date",cex.axis=0.75)
axis(2, at=c(0,0.005,0.01,0.015,0.02,0.025,0.03,0.035),labels=c("0","0.5","1.0","1.5","2.0","2.5","0.3.0","3.5"),cex.axis=1.15)
