Stime1<-Stime
################################################################## Tab rappIncr Incidenza############################################################################
#rapporto incrementale prima-dopo chiusura scuole 05 Marzo (11)
m11<-(Stime1$NVarVacc1[Stime1$date1==11]-Stime1$NVarVacc1[Stime1$date1==4])/7
m12<-(Stime1$NVarVacc1[Stime1$date1==18]-Stime1$NVarVacc1[Stime1$date1==11])/7
t17<-ifelse(m12>m11,abs((m12-m11)/m11),-abs((m12-m11)/m11))
m11<-(Stime1$NVarVacc1[Stime1$date1==11]-Stime1$NVarVacc1[Stime1$date1==1])/11
m12<-(Stime1$NVarVacc1[Stime1$date1==25]-Stime1$NVarVacc1[Stime1$date1==11])/14
t114<-ifelse(m12>m11,abs((m12-m11)/m11),-abs((m12-m11)/m11))

#rapporto incrementale prima-dopo lockdown 12 Marzo (18° giorno)
m21<-(Stime1$NVarVacc1[Stime1$date1==18]-Stime1$NVarVacc1[Stime1$date1==11])/7
m22<-(Stime1$NVarVacc1[Stime1$date1==25]-Stime1$NVarVacc1[Stime1$date1==18])/7
t27<-ifelse(m22>m21,abs((m22-m21)/m21),-abs((m22-m21)/m21))
m21<-(Stime1$NVarVacc1[Stime1$date1==18]-Stime1$NVarVacc1[Stime1$date1==4])/14
m22<-(Stime1$NVarVacc1[Stime1$date1==32]-Stime1$NVarVacc1[Stime1$date1==18])/14
t214<-ifelse(m22>m21,abs((m22-m21)/m21),-abs((m22-m21)/m21))

#rapporto incrementale prima-dopo lockdown 23 Marzo (29° giorno)
m31<-(Stime1$NVarVacc[Stime1$date1==29]-Stime1$NVarVacc[Stime1$date1==22])/7
m32<-(Stime1$NVarVacc[Stime1$date1==36]-Stime1$NVarVacc[Stime1$date1==29])/7
t37<-ifelse(m32>m31,abs((m32-m31)/m31),-abs((m32-m31)/m31))
m31<-(Stime1$NVarVacc1[Stime1$date1==29]-Stime1$NVarVacc1[Stime1$date1==15])/14
m32<-(Stime1$NVarVacc1[Stime1$date1==44]-Stime1$NVarVacc1[Stime1$date1==29])/14
t314<-ifelse(m32>m31,abs((m32-m31)/m31),-abs((m32-m31)/m31))

#rapporto incrementale prima-dopo mobilità intraregionale 17 Maggio (84° giorno)
m41<-(Stime1$NVarVacc1[Stime1$date1==84]-Stime1$NVarVacc1[Stime1$date1==77])/7
m42<-(Stime1$NVarVacc1[Stime1$date1==91]-Stime1$NVarVacc1[Stime1$date1==84])/7
t47<-ifelse(m42>m41,abs((m42-m41)/m41),-abs((m42-m41)/m41))
m41<-(Stime1$NVarVacc1[Stime1$date1==84]-Stime1$NVarVacc1[Stime1$date1==70])/14
m42<-(Stime1$NVarVacc1[Stime1$date1==98]-Stime1$NVarVacc1[Stime1$date1==84])/14
t414<-ifelse(m42>m41,abs((m42-m41)/m41),-abs((m42-m41)/m41))

#rapporto incrementale prima-dopo mobilità intraregionale 04 Giugno (102° giorno)
m51<-(Stime1$NVarVacc1[Stime1$date1==102]-Stime1$NVarVacc1[Stime1$date1==95])/7
m52<-(Stime1$NVarVacc1[Stime1$date1==109]-Stime1$NVarVacc1[Stime1$date1==102])/7
t57<-ifelse(m52>m51,abs((m52-m51)/m51),-abs((m52-m51)/m51))
m51<-(Stime1$NVarVacc1[Stime1$date1==102]-Stime1$NVarVacc1[Stime1$date1==88])/14
m52<-(Stime1$NVarVacc1[Stime1$date1==116]-Stime1$NVarVacc1[Stime1$date1==102])/14
t514<-ifelse(m52>m51,abs((m52-m51)/m51),-abs((m52-m51)/m51))

#rapporto incrementale prima-dopo apertura scuole 14 Stembre (204° giorno)
m61<-(Stime1$NVarVacc1[Stime1$date1==204]-Stime1$NVarVacc1[Stime1$date1==197])/7
m62<-(Stime1$NVarVacc1[Stime1$date1==221]-Stime1$NVarVacc1[Stime1$date1==214])/7
t67<-ifelse(m62>m61,abs((m62-m61)/m61),-abs((m62-m61)/m61))
m61<-(Stime1$NVarVacc1[Stime1$date1==204]-Stime1$NVarVacc1[Stime1$date1==190])/14
m62<-(Stime1$NVarVacc1[Stime1$date1==228]-Stime1$NVarVacc1[Stime1$date1==214])/14
t614<-ifelse(m62>m61,abs((m62-m61)/m61),-abs((m62-m61)/m61))

#rapporto incrementale restrizioni con 75% DAD 24 ottobre (244° giorno)
m71<-(Stime1$NVarVacc1[Stime1$date1==244]-Stime1$NVarVacc1[Stime1$date1==237])/7
m72<-(Stime1$NVarVacc1[Stime1$date1==251]-Stime1$NVarVacc1[Stime1$date1==244])/7
t77<-ifelse(m72>m71,abs((m72-m71)/m71),-abs((m72-m71)/m71))
m71<-(Stime1$NVarVacc1[Stime1$date1==244]-Stime1$NVarVacc1[Stime1$date1==230])/14
m72<-(Stime1$NVarVacc1[Stime1$date1==258]-Stime1$NVarVacc1[Stime1$date1==244])/14
t714<-ifelse(m72>m71,abs((m72-m71)/m71),-abs((m72-m71)/m71))

#rapporto incrementale restrizioni regionali basate su Rt 05 novembre (256° giorno)
m81<-(Stime1$NVarVacc1[Stime1$date1==256]-Stime1$NVarVacc1[Stime1$date1==249])/7
m82<-(Stime1$NVarVacc1[Stime1$date1==263]-Stime1$NVarVacc1[Stime1$date1==256])/7
t87<-ifelse(m82>m81,abs((m82-m81)/m81),-abs((m82-m81)/m81))
m81<-(Stime1$NVarVacc1[Stime1$date1==256]-Stime1$NVarVacc1[Stime1$date1==242])/14
m82<-(Stime1$NVarVacc1[Stime1$date1==270]-Stime1$NVarVacc1[Stime1$date1==256])/14
t814<-ifelse(m82>m81,abs((m82-m81)/m81),-abs((m82-m81)/m81))

#rapporto incrementale prima-dopo apertura scuole 08 dicembre (189° giorno)
m91<-(Stime1$NVarVacc1[Stime1$date1==289]-Stime1$NVarVacc1[Stime1$date1==282])/7
m92<-(Stime1$NVarVacc1[Stime1$date1==296]-Stime1$NVarVacc1[Stime1$date1==289])/7
t97<-ifelse(m92>m91,abs((m92-m91)/m91),-abs((m92-m92)/m91))
m91<-(Stime1$NVarVacc1[Stime1$date1==289]-Stime1$NVarVacc1[Stime1$date1==277])/14
m92<-(Stime1$NVarVacc1[Stime1$date1==303]-Stime1$NVarVacc1[Stime1$date1==289])/14
t914<-ifelse(m92>m91,abs((m92-m91)/m91),-abs((m92-m92)/m91))

#rapporto incrementale no mobilità tra regioni 20 dicembre (301° giorno)
m101<-(Stime1$NVarVacc1[Stime1$date1==301]-Stime1$NVarVacc1[Stime1$date1==294])/7
m102<-(Stime1$NVarVacc1[Stime1$date1==308]-Stime1$NVarVacc1[Stime1$date1==301])/7
t107<-ifelse(m102>m101,abs((m102-m101)/m101),-abs((m102-m101)/m101))
m101<-(Stime1$NVarVacc1[Stime1$date1==301]-Stime1$NVarVacc1[Stime1$date1==287])/14
m102<-(Stime1$NVarVacc1[Stime1$date1==315]-Stime1$NVarVacc1[Stime1$date1==301])/14
t1014<-ifelse(m102>m101,abs((m102-m101)/m101),-abs((m102-m101)/m101))

#rapporto incrementale no mobilità tranne 1 visita al giorno a parenti 24 dicembre (305° giorno)
m111<-(Stime1$NVarVacc1[Stime1$date1==305]-Stime1$NVarVacc1[Stime1$date1==298])/7
m112<-(Stime1$NVarVacc1[Stime1$date1==312]-Stime1$NVarVacc1[Stime1$date1==305])/7
t117<-ifelse(m112>m111,abs((m112-m111)/m111),-abs((m112-m111)/m111))
m111<-(Stime1$NVarVacc1[Stime1$date1==305]-Stime1$NVarVacc1[Stime1$date1==291])/14
m112<-(Stime1$NVarVacc1[Stime1$date1==319]-Stime1$NVarVacc1[Stime1$date1==305])/14
t1114<-ifelse(m112>m111,abs((m112-m111)/m111),-abs((m112-m111)/m111))

#rapporto incrementale no mobilità tra regioni, mobilità regionale secondo Rt 7 gennaio (318° giorno)
m121<-(Stime1$NVarVacc1[Stime1$date1==319]-Stime1$NVarVacc1[Stime1$date1==312])/7
m122<-(Stime1$NVarVacc1[Stime1$date1==326]-Stime1$NVarVacc1[Stime1$date1==319])/7
t127<-ifelse(m122>m121,abs((m122-m121)/m121),-abs((m122-m121)/m121))
m121<-(Stime1$NVarVacc1[Stime1$date1==319]-Stime1$NVarVacc1[Stime1$date1==305])/14
m122<-(Stime1$NVarVacc1[Stime1$date1==333]-Stime1$NVarVacc1[Stime1$date1==319])/14
t1214<-ifelse(m122>m121,abs((m122-m121)/m121),-abs((m122-m121)/m121))

#rapporto incrementale no mobilità tra regioni, mobilità regionale secondo Rt 7 gennaio (337° giorno)
m131<-(Stime1$NVarVacc1[Stime1$date1==330]-Stime1$NVarVacc1[Stime1$date1==323])/7
m132<-(Stime1$NVarVacc1[Stime1$date1==351]-Stime1$NVarVacc1[Stime1$date1==344])/7
t137<-ifelse(m132>m131,abs((m132-m131)/m131),-abs((m132-m131)/m131))
m131<-(Stime1$NVarVacc1[Stime1$date1==330]-Stime1$NVarVacc1[Stime1$date1==316])/14
m132<-(Stime1$NVarVacc1[Stime1$date1==358]-Stime1$NVarVacc1[Stime1$date1==344])/14
t1314<-ifelse(m132>m131,abs((m132-m131)/m131),-abs((m132-m131)/m131))


#rapporto incrementale More restrictions on mobility (equal rules between yellow and orande regions until Easter) (386° giorno)
m141<-(Stime1$NVarVacc1[Stime1$date1==386]-Stime1$NVarVacc1[Stime1$date1==379])/7
m142<-(Stime1$NVarVacc1[Stime1$date1==393]-Stime1$NVarVacc1[Stime1$date1==386])/7
t147<-ifelse(m142>m141,abs((m142-m141)/m141),-abs((m142-m141)/m141))
m141<-(Stime1$NVarVacc1[Stime1$date1==386]-Stime1$NVarVacc1[Stime1$date1==372])/14
m142<-(Stime1$NVarVacc1[Stime1$date1==400]-Stime1$NVarVacc1[Stime1$date1==386])/14
t1414<-ifelse(m142>m141,abs((m142-m141)/m141),-abs((m142-m141)/m141))

#rapporto incrementale More restrictions on mobility (put back of yellow area (with curfew 22.00-05.00) and introduction of free pass) (425° giorno)
m151<-(Stime1$NVarVacc1[Stime1$date1==425]-Stime1$NVarVacc1[Stime1$date1==418])/7
m152<-(Stime1$NVarVacc1[Stime1$date1==432]-Stime1$NVarVacc1[Stime1$date1==425])/7
t157<-ifelse(m152>m151,abs((m152-m151)/m151),-abs((m152-m151)/m151))
m151<-(Stime1$NVarVacc1[Stime1$date1==425]-Stime1$NVarVacc1[Stime1$date1==411])/14
m152<-(Stime1$NVarVacc1[Stime1$date1==439]-Stime1$NVarVacc1[Stime1$date1==425])/14
t1514<-ifelse(m152>m151,abs((m152-m151)/m151),-abs((m152-m151)/m151))

#rapporto incrementale less restrictions on in yellow area: gym opening (455° giorno)
m161<-(Stime1$NVarVacc1[Stime1$date1==455]-Stime1$NVarVacc1[Stime1$date1==448])/7
m162<-(Stime1$NVarVacc1[Stime1$date1==462]-Stime1$NVarVacc1[Stime1$date1==455])/7
t167<-ifelse(m162>m161,abs((m162-m161)/m161),-abs((m162-m162)/m161))
m161<-(Stime1$NVarVacc1[Stime1$date1==455]-Stime1$NVarVacc1[Stime1$date1==441])/14
m162<-(Stime1$NVarVacc1[Stime1$date1==469]-Stime1$NVarVacc1[Stime1$date1==455])/14
t1614<-ifelse(m162>m161,abs((m162-m161)/m161),-abs((m162-m162)/m161))

#rapporto incrementale less restrictions in yellow area: indoor catering (464° giorno)
m171<-(Stime1$NVarVacc1[Stime1$date1==464]-Stime1$NVarVacc1[Stime1$date1==457])/7
m172<-(Stime1$NVarVacc1[Stime1$date1==471]-Stime1$NVarVacc1[Stime1$date1==464])/7
t177<-ifelse(m172>m171,abs((m172-m171)/m171),-abs((m172-m171)/m171))
m171<-(Stime1$NVarVacc1[Stime1$date1==464]-Stime1$NVarVacc1[Stime1$date1==450])/7
m172<-(Stime1$NVarVacc1[Stime1$date1==478]-Stime1$NVarVacc1[Stime1$date1==464])/7
t1714<-ifelse(m172>m171,abs((m172-m171)/m171),-abs((m172-m171)/m171))

#rapporto incrementale less restrictions in yellow area: No curfew (484° giorno)
m181<-(Stime1$NVarVacc1[Stime1$date1==484]-Stime1$NVarVacc1[Stime1$date1==477])/7
m182<-(Stime1$NVarVacc1[Stime1$date1==491]-Stime1$NVarVacc1[Stime1$date1==484])/7
t187<-ifelse(m182>m181,abs((m182-m181)/m181),-abs((m182-m181)/m181))
m181<-(Stime1$NVarVacc1[Stime1$date1==484]-Stime1$NVarVacc1[Stime1$date1==470])/7
m182<-(Stime1$NVarVacc1[Stime1$date1==498]-Stime1$NVarVacc1[Stime1$date1==484])/7
t1814<-ifelse(m182>m181,abs((m182-m181)/m181),-abs((m182-m181)/m181))

#rapporto incrementale less restrictions semi-final and final of 2020 UEFA European Football Championship (502° giorno)
m191<-(Stime1$NVarVacc1[Stime1$date1==499]-Stime1$NVarVacc1[Stime1$date1==492])/7
m192<-(Stime1$NVarVacc1[Stime1$date1==511]-Stime1$NVarVacc1[Stime1$date1==504])/7
t197<-ifelse(m192>m191,abs((m192-m191)/m191),-abs((m192-m191)/m191))
m191<-(Stime1$NVarVacc1[Stime1$date1==499]-Stime1$NVarVacc1[Stime1$date1==485])/7
m192<-(Stime1$NVarVacc1[Stime1$date1==518]-Stime1$NVarVacc1[Stime1$date1==504])/7
t1914<-ifelse(m192>m191,abs((m192-m191)/m191),-abs((m192-m191)/m191))

#rapporto incrementale more restrictions mompulsory green pass for many group activities (530° giorno)
m201<-(Stime1$NVarVacc1[Stime1$date1==530]-Stime1$NVarVacc1[Stime1$date1==523])/7
m202<-(Stime1$NVarVacc1[Stime1$date1==537]-Stime1$NVarVacc1[Stime1$date1==530])/7
t207<-ifelse(m202>m201,abs((m202-m201)/m201),-abs((m202-m201)/m201))
m201<-(Stime1$NVarVacc1[Stime1$date1==530]-Stime1$NVarVacc1[Stime1$date1==516])/14
m202<-(Stime1$NVarVacc1[Stime1$date1==544]-Stime1$NVarVacc1[Stime1$date1==530])/14
t2014<-ifelse(m202>m201,abs((m202-m201)/m201),-abs((m202-m201)/m201))

#rapporto incrementale less restrictions school opening (572° giorno)
m211<-(Stime1$NVarVacc1[Stime1$date1==568]-Stime1$NVarVacc1[Stime1$date1==561])/7
m212<-(Stime1$NVarVacc1[Stime1$date1==582]-Stime1$NVarVacc1[Stime1$date1==575])/7
t217<-ifelse(m212>m211,abs((m212-m211)/m211),-abs((m212-m211)/m211))
m211<-(Stime1$NVarVacc1[Stime1$date1==568]-Stime1$NVarVacc1[Stime1$date1==554])/14
m212<-(Stime1$NVarVacc1[Stime1$date1==589]-Stime1$NVarVacc1[Stime1$date1==575])/14
t2114<-ifelse(m212>m211,abs((m212-m211)/m211),-abs((m212-m211)/m211))

#rapporto incrementale less restrictions back to working in presence (600° giorno)
m221<-(Stime1$NVarVacc1[Stime1$date1==600]-Stime1$NVarVacc1[Stime1$date1==593])/7
m222<-(Stime1$NVarVacc1[Stime1$date1==607]-Stime1$NVarVacc1[Stime1$date1==600])/7
t227<-ifelse(m222>m221,abs((m222-m221)/m221),-abs((m222-m221)/m221))
m221<-(Stime1$NVarVacc1[Stime1$date1==600]-Stime1$NVarVacc1[Stime1$date1==586])/14
m222<-(Stime1$NVarVacc1[Stime1$date1==614]-Stime1$NVarVacc1[Stime1$date1==600])/14
t2214<-ifelse(m222>m221,abs((m222-m221)/m221),-abs((m222-m221)/m221))

#rapporto incrementale more restrictions extension of mandatory vaccination and of green pass (643° giorno)
m231<-(Stime1$NVarVacc1[Stime1$date1==643]-Stime1$NVarVacc1[Stime1$date1==636])/7
m232<-(Stime1$NVarVacc1[Stime1$date1==650]-Stime1$NVarVacc1[Stime1$date1==643])/7
t237<-ifelse(m232>m231,abs((m232-m231)/m231),-abs((m232-m231)/m231))
m231<-(Stime1$NVarVacc1[Stime1$date1==643]-Stime1$NVarVacc1[Stime1$date1==629])/14
m232<-(Stime1$NVarVacc1[Stime1$date1==657]-Stime1$NVarVacc1[Stime1$date1==643])/14
t2314<-ifelse(m232>m231,abs((m232-m231)/m231),-abs((m232-m231)/m231))

#rapporto incrementale more restrictions mandatory ffp2 on public transport (673° giorno)
m241<-(Stime1$NVarVacc1[Stime1$date1==673]-Stime1$NVarVacc1[Stime1$date1==666])/7
m242<-(Stime1$NVarVacc1[Stime1$date1==680]-Stime1$NVarVacc1[Stime1$date1==673])/7
t247<-ifelse(m242>m241,abs((m242-m241)/m241),-abs((m242-m241)/m241))
m241<-(Stime1$NVarVacc1[Stime1$date1==673]-Stime1$NVarVacc1[Stime1$date1==659])/14
m242<-(Stime1$NVarVacc1[Stime1$date1==687]-Stime1$NVarVacc1[Stime1$date1==673])/14
t2414<-ifelse(m242>m241,abs((m242-m241)/m241),-abs((m242-m241)/m241))

#rapporto incrementale more school opening (686° giorno)
m251<-(Stime1$NVarVacc1[Stime1$date1==686]-Stime1$NVarVacc1[Stime1$date1==679])/7
m252<-(Stime1$NVarVacc1[Stime1$date1==693]-Stime1$NVarVacc1[Stime1$date1==686])/7
t257<-ifelse(m252>m251,abs((m252-m251)/m251),-abs((m252-m251)/m251))
m251<-(Stime1$NVarVacc1[Stime1$date1==686]-Stime1$NVarVacc1[Stime1$date1==672])/114
m252<-(Stime1$NVarVacc1[Stime1$date1==700]-Stime1$NVarVacc1[Stime1$date1==686])/14
t2514<-ifelse(m252>m251,abs((m252-m251)/m251),-abs((m252-m251)/m251))

TabInc<-round(as.data.frame(matrix(c(t17,t114,t27,t214,t37,t314,t47,t414,t57,t514,t67,t614,t77,t714,t87,t814,t97,t914,t107,t1014,t117,t1114,t127,t1214,t137,t1314,t147,t1414,t157,t1514,t167,t1614,t177,t1714,t187,t1814,t197,t1914,t207,t2014,t217,t2114,t227,t2214,t237,t2314,t247,t2414,t257,t2514),byrow=TRUE,ncol=2,nrow=25)),2)
colnames(TabInc)<-c("I7","I14")
TabInc$"I7"<-round(100*TabInc$"I7",0)
TabInc$"I14"<-round(100*TabInc$"I14",0)


TabInc$meas<-c("School closed","Stop mobility","Industrial lockdown","intraregional mobility",
             "free mobility","School opened",
             "Several restrictions (including 75% DAD high school)","Regional restrictions according to Rt","Incentives for Christmas shopping",
             "No mobility between regions","No mobility but 1 visit per day to parents within municipalities","Regional restrictions according to Rt",
             "high school opening (50-75% in precence)","More restrictions on mobility (equal rules between yellow and orande regions until Easter)","put back of yellow area (with curfew 22.00-05.00) and introduction of free pass",
             "in yellow area: gym opening","in yellow area: indoor catering","in yellow area: No curfew",
             "semi-final and final of 2020 UEFA European Football Championship","compulsory green pass for many group activities","school opening",
             "back to working in presence","extension of mandatory vaccination and of green pass","mandatory ffp2 on public transport","school opening")
TabInc
################################################################## Fine Tab rappIncr Incidenza############################################################################


################################################################## Tab rappIncr Incidenza############################################################################
#rapporto incrementale prima-dopo chiusura scuole 05 Marzo (11)
m11<-(Stime1$NVarVacc1[Stime1$date1==11]*Stime1$IFRVar[Stime1$date1==11]-Stime1$NVarVacc1[Stime1$date1==4]*Stime1$IFRVar[Stime1$date1==4])/7
m12<-(Stime1$NVarVacc1[Stime1$date1==18]*Stime1$IFRVar[Stime1$date1==18]-Stime1$NVarVacc1[Stime1$date1==11]*Stime1$IFRVar[Stime1$date1==11])/7
t17<-ifelse(m12>m11,abs((m12-m11)/m11),-abs((m12-m11)/m11))
m11<-(Stime1$NVarVacc1[Stime1$date1==11]*Stime1$IFRVar[Stime1$date1==11]-Stime1$NVarVacc1[Stime1$date1==1]*Stime1$IFRVar[Stime1$date1==1])/11
m12<-(Stime1$NVarVacc1[Stime1$date1==25]*Stime1$IFRVar[Stime1$date1==25]-Stime1$NVarVacc1[Stime1$date1==11]*Stime1$IFRVar[Stime1$date1==11])/14
t114<-ifelse(m12>m11,abs((m12-m11)/m11),-abs((m12-m11)/m11))

#rapporto incrementale prima-dopo lockdown 12 Marzo (18° giorno)
m21<-(Stime1$NVarVacc1[Stime1$date1==18]*Stime1$IFRVar[Stime1$date1==18]-Stime1$NVarVacc1[Stime1$date1==11]*Stime1$IFRVar[Stime1$date1==11])/7
m22<-(Stime1$NVarVacc1[Stime1$date1==25]*Stime1$IFRVar[Stime1$date1==25]-Stime1$NVarVacc1[Stime1$date1==18]*Stime1$IFRVar[Stime1$date1==11])/7
t27<-ifelse(m22>m21,abs((m22-m21)/m21),-abs((m22-m21)/m21))
m21<-(Stime1$NVarVacc1[Stime1$date1==18]*Stime1$IFRVar[Stime1$date1==18]-Stime1$NVarVacc1[Stime1$date1==4]*Stime1$IFRVar[Stime1$date1==4])/14
m22<-(Stime1$NVarVacc1[Stime1$date1==32]*Stime1$IFRVar[Stime1$date1==32]-Stime1$NVarVacc1[Stime1$date1==18]*Stime1$IFRVar[Stime1$date1==18])/14
t214<-ifelse(m22>m21,abs((m22-m21)/m21),-abs((m22-m21)/m21))

#rapporto incrementale prima-dopo lockdown 23 Marzo (29° giorno)
m31<-(Stime1$NVarVacc[Stime1$date1==29]*Stime1$IFRVar[Stime1$date1==29]-Stime1$NVarVacc[Stime1$date1==22]*Stime1$IFRVar[Stime1$date1==22])/7
m32<-(Stime1$NVarVacc[Stime1$date1==36]*Stime1$IFRVar[Stime1$date1==36]-Stime1$NVarVacc[Stime1$date1==29]*Stime1$IFRVar[Stime1$date1==29])/7
t37<-ifelse(m32>m31,abs((m32-m31)/m31),-abs((m32-m31)/m31))
m31<-(Stime1$NVarVacc1[Stime1$date1==29]*Stime1$IFRVar[Stime1$date1==29]-Stime1$NVarVacc1[Stime1$date1==15]*Stime1$IFRVar[Stime1$date1==15])/14
m32<-(Stime1$NVarVacc1[Stime1$date1==44]*Stime1$IFRVar[Stime1$date1==44]-Stime1$NVarVacc1[Stime1$date1==29]*Stime1$IFRVar[Stime1$date1==29])/14
t314<-ifelse(m32>m31,abs((m32-m31)/m31),-abs((m32-m31)/m31))


#rapporto incrementale prima-dopo mobilità intraregionale 17 Maggio (84° giorno)
m41<-(Stime1$NVarVacc1[Stime1$date1==84]*Stime1$IFRVar[Stime1$date1==84]-Stime1$NVarVacc1[Stime1$date1==77]*Stime1$IFRVar[Stime1$date1==77])/7
m42<-(Stime1$NVarVacc1[Stime1$date1==91]*Stime1$IFRVar[Stime1$date1==91]-Stime1$NVarVacc1[Stime1$date1==84]*Stime1$IFRVar[Stime1$date1==84])/7
t47<-ifelse(m42>m41,abs((m42-m41)/m41),-abs((m42-m41)/m41))
m41<-(Stime1$NVarVacc1[Stime1$date1==84]*Stime1$IFRVar[Stime1$date1==84]-Stime1$NVarVacc1[Stime1$date1==70]*Stime1$IFRVar[Stime1$date1==70])/14
m42<-(Stime1$NVarVacc1[Stime1$date1==98]*Stime1$IFRVar[Stime1$date1==98]-Stime1$NVarVacc1[Stime1$date1==84]*Stime1$IFRVar[Stime1$date1==84])/14
t414<-ifelse(m42>m41,abs((m42-m41)/m41),-abs((m42-m41)/m41))


#rapporto incrementale prima-dopo mobilità intraregionale 04 Giugno (102° giorno)
m51<-(Stime1$NVarVacc1[Stime1$date1==102]*Stime1$IFRVar[Stime1$date1==102]-Stime1$NVarVacc1[Stime1$date1==95]*Stime1$IFRVar[Stime1$date1==95])/7
m52<-(Stime1$NVarVacc1[Stime1$date1==109]*Stime1$IFRVar[Stime1$date1==109]-Stime1$NVarVacc1[Stime1$date1==102]*Stime1$IFRVar[Stime1$date1==102])/7
t57<-ifelse(m52>m51,abs((m52-m51)/m51),-abs((m52-m51)/m51))
m51<-(Stime1$NVarVacc1[Stime1$date1==102]*Stime1$IFRVar[Stime1$date1==102]-Stime1$NVarVacc1[Stime1$date1==88]*Stime1$IFRVar[Stime1$date1==88])/14
m52<-(Stime1$NVarVacc1[Stime1$date1==116]*Stime1$IFRVar[Stime1$date1==116]-Stime1$NVarVacc1[Stime1$date1==102]*Stime1$IFRVar[Stime1$date1==102])/14
t514<-ifelse(m52>m51,abs((m52-m51)/m51),-abs((m52-m51)/m51))

#rapporto incrementale prima-dopo apertura scuole 14 Stembre (204° giorno)
m61<-(Stime1$NVarVacc1[Stime1$date1==204]*Stime1$IFRVar[Stime1$date1==204]-Stime1$NVarVacc1[Stime1$date1==197]*Stime1$IFRVar[Stime1$date1==197])/7
m62<-(Stime1$NVarVacc1[Stime1$date1==221]*Stime1$IFRVar[Stime1$date1==221]-Stime1$NVarVacc1[Stime1$date1==214]*Stime1$IFRVar[Stime1$date1==214])/7
t67<-ifelse(m62>m61,abs((m62-m61)/m61),-abs((m62-m61)/m61))
m61<-(Stime1$NVarVacc1[Stime1$date1==204]*Stime1$IFRVar[Stime1$date1==204]-Stime1$NVarVacc1[Stime1$date1==190]*Stime1$IFRVar[Stime1$date1==190])/14
m62<-(Stime1$NVarVacc1[Stime1$date1==228]*Stime1$IFRVar[Stime1$date1==228]-Stime1$NVarVacc1[Stime1$date1==214]*Stime1$IFRVar[Stime1$date1==214])/14
t614<-ifelse(m62>m61,abs((m62-m61)/m61),-abs((m62-m61)/m61))

#rapporto incrementale restrizioni con 75% DAD 24 ottobre (244° giorno)
m71<-(Stime1$NVarVacc1[Stime1$date1==244]*Stime1$IFRVar[Stime1$date1==244]-Stime1$NVarVacc1[Stime1$date1==237]*Stime1$IFRVar[Stime1$date1==237])/7
m72<-(Stime1$NVarVacc1[Stime1$date1==251]*Stime1$IFRVar[Stime1$date1==251]-Stime1$NVarVacc1[Stime1$date1==244]*Stime1$IFRVar[Stime1$date1==244])/7
t77<-ifelse(m72>m71,abs((m72-m71)/m71),-abs((m72-m71)/m71))
m71<-(Stime1$NVarVacc1[Stime1$date1==244]*Stime1$IFRVar[Stime1$date1==244]-Stime1$NVarVacc1[Stime1$date1==230]*Stime1$IFRVar[230])/14
m72<-(Stime1$NVarVacc1[Stime1$date1==258]*Stime1$IFRVar[Stime1$date1==258]-Stime1$NVarVacc1[Stime1$date1==244]*Stime1$IFRVar[Stime1$date1==244])/14
t714<-ifelse(m72>m71,abs((m72-m71)/m71),-abs((m72-m71)/m71))

#rapporto incrementale restrizioni regionali basate su Rt 05 novembre (256° giorno)
m81<-(Stime1$NVarVacc1[Stime1$date1==256]*Stime1$IFRVar[Stime1$date1==256]-Stime1$NVarVacc1[Stime1$date1==249]*Stime1$IFRVar[Stime1$date1==249])/7
m82<-(Stime1$NVarVacc1[Stime1$date1==263]*Stime1$IFRVar[Stime1$date1==263]-Stime1$NVarVacc1[Stime1$date1==256]*Stime1$IFRVar[Stime1$date1==256])/7
t87<-ifelse(m82>m81,abs((m82-m81)/m81),-abs((m82-m81)/m81))
m81<-(Stime1$NVarVacc1[Stime1$date1==256]*Stime1$IFRVar[Stime1$date1==256]-Stime1$NVarVacc1[Stime1$date1==242]*Stime1$IFRVar[Stime1$date1==242])/14
m82<-(Stime1$NVarVacc1[Stime1$date1==270]*Stime1$IFRVar[Stime1$date1==270]-Stime1$NVarVacc1[Stime1$date1==256]*Stime1$IFRVar[Stime1$date1==256])/14
t814<-ifelse(m82>m81,abs((m82-m81)/m81),-abs((m82-m81)/m81))

#rapporto incrementale prima-dopo apertura scuole 08 dicembre (189° giorno)
m91<-(Stime1$NVarVacc1[Stime1$date1==289]*Stime1$IFRVar[Stime1$date1==289]-Stime1$NVarVacc1[Stime1$date1==282]*Stime1$IFRVar[Stime1$date1==282])/7
m92<-(Stime1$NVarVacc1[Stime1$date1==296]*Stime1$IFRVar[Stime1$date1==296]-Stime1$NVarVacc1[Stime1$date1==289]*Stime1$IFRVar[Stime1$date1==289])/7
t97<-ifelse(m92>m91,abs((m92-m91)/m91),-abs((m92-m92)/m91))
m91<-(Stime1$NVarVacc1[Stime1$date1==289]*Stime1$IFRVar[Stime1$date1==289]-Stime1$NVarVacc1[Stime1$date1==277]*Stime1$IFRVar[Stime1$date1==277])/14
m92<-(Stime1$NVarVacc1[Stime1$date1==303]*Stime1$IFRVar[Stime1$date1==303]-Stime1$NVarVacc1[Stime1$date1==289]*Stime1$IFRVar[Stime1$date1==289])/14
t914<-ifelse(m92>m91,abs((m92-m91)/m91),-abs((m92-m92)/m91))

#rapporto incrementale no mobilità tra regioni 20 dicembre (301° giorno)
m101<-(Stime1$NVarVacc1[Stime1$date1==301]*Stime1$IFRVar[Stime1$date1==301]-Stime1$NVarVacc1[Stime1$date1==294]*Stime1$IFRVar[Stime1$date1==294])/7
m102<-(Stime1$NVarVacc1[Stime1$date1==308]*Stime1$IFRVar[Stime1$date1==308]-Stime1$NVarVacc1[Stime1$date1==301]*Stime1$IFRVar[Stime1$date1==301])/7
t107<-ifelse(m102>m101,abs((m102-m101)/m101),-abs((m102-m101)/m101))
m101<-(Stime1$NVarVacc1[Stime1$date1==301]*Stime1$IFRVar[Stime1$date1==301]-Stime1$NVarVacc1[Stime1$date1==287]*Stime1$IFRVar[Stime1$date1==287])/14
m102<-(Stime1$NVarVacc1[Stime1$date1==315]*Stime1$IFRVar[Stime1$date1==315]-Stime1$NVarVacc1[Stime1$date1==301]*Stime1$IFRVar[Stime1$date1==301])/14
t1014<-ifelse(m102>m101,abs((m102-m101)/m101),-abs((m102-m101)/m101))

#rapporto incrementale no mobilità tranne 1 visita al giorno a parenti 24 dicembre (305° giorno)
m111<-(Stime1$NVarVacc1[Stime1$date1==305]*Stime1$IFRVar[Stime1$date1==305]-Stime1$NVarVacc1[Stime1$date1==298]*Stime1$IFRVar[Stime1$date1==298])/7
m112<-(Stime1$NVarVacc1[Stime1$date1==312]*Stime1$IFRVar[Stime1$date1==312]-Stime1$NVarVacc1[Stime1$date1==305]*Stime1$IFRVar[Stime1$date1==305])/7
t117<-ifelse(m112>m111,abs((m112-m111)/m111),-abs((m112-m111)/m111))
m111<-(Stime1$NVarVacc1[Stime1$date1==305]*Stime1$IFRVar[Stime1$date1==305]-Stime1$NVarVacc1[Stime1$date1==291]*Stime1$IFRVar[Stime1$date1==291])/14
m112<-(Stime1$NVarVacc1[Stime1$date1==319]*Stime1$IFRVar[Stime1$date1==319]-Stime1$NVarVacc1[Stime1$date1==305]*Stime1$IFRVar[Stime1$date1==305])/14
t1114<-ifelse(m112>m111,abs((m112-m111)/m111),-abs((m112-m111)/m111))

#rapporto incrementale no mobilità tra regioni, mobilità regionale secondo Rt 7 gennaio (318° giorno)
m121<-(Stime1$NVarVacc1[Stime1$date1==319]*Stime1$IFRVar[Stime1$date1==319]-Stime1$NVarVacc1[Stime1$date1==312]*Stime1$IFRVar[Stime1$date1==312])/7
m122<-(Stime1$NVarVacc1[Stime1$date1==326]*Stime1$IFRVar[Stime1$date1==326]-Stime1$NVarVacc1[Stime1$date1==319]*Stime1$IFRVar[Stime1$date1==319])/7
t127<-ifelse(m122>m121,abs((m122-m121)/m121),-abs((m122-m121)/m121))
m121<-(Stime1$NVarVacc1[Stime1$date1==319]*Stime1$IFRVar[Stime1$date1==319]-Stime1$NVarVacc1[Stime1$date1==305]*Stime1$IFRVar[Stime1$date1==305])/14
m122<-(Stime1$NVarVacc1[Stime1$date1==333]*Stime1$IFRVar[Stime1$date1==333]-Stime1$NVarVacc1[Stime1$date1==319]*Stime1$IFRVar[Stime1$date1==319])/14
t1214<-ifelse(m122>m121,abs((m122-m121)/m121),-abs((m122-m121)/m121))

#rapporto incrementale no mobilità tra regioni, mobilità regionale secondo Rt 7 gennaio (337° giorno)
m131<-(Stime1$NVarVacc1[Stime1$date1==330]*Stime1$IFRVar[Stime1$date1==330]-Stime1$NVarVacc1[Stime1$date1==323]*Stime1$IFRVar[Stime1$date1==323])/7
m132<-(Stime1$NVarVacc1[Stime1$date1==351]*Stime1$IFRVar[Stime1$date1==351]-Stime1$NVarVacc1[Stime1$date1==344]*Stime1$IFRVar[Stime1$date1==344])/7
t137<-ifelse(m132>m131,abs((m132-m131)/m131),-abs((m132-m131)/m131))
m131<-(Stime1$NVarVacc1[Stime1$date1==330]*Stime1$IFRVar[Stime1$date1==330]-Stime1$NVarVacc1[Stime1$date1==316]*Stime1$IFRVar[Stime1$date1==316])/14
m132<-(Stime1$NVarVacc1[Stime1$date1==358]*Stime1$IFRVar[Stime1$date1==358]-Stime1$NVarVacc1[Stime1$date1==344]*Stime1$IFRVar[Stime1$date1==344])/14
t1314<-ifelse(m132>m131,abs((m132-m131)/m131),-abs((m132-m131)/m131))

#rapporto incrementale More restrictions on mobility (equal rules between yellow and orande regions until Easter) (386° giorno)
m141<-(Stime1$NVarVacc1[Stime1$date1==386]*Stime1$IFRVar[Stime1$date1==386]-Stime1$NVarVacc1[Stime1$date1==379]*Stime1$IFRVar[Stime1$date1==379])/7
m142<-(Stime1$NVarVacc1[Stime1$date1==393]*Stime1$IFRVar[Stime1$date1==393]-Stime1$NVarVacc1[Stime1$date1==386]*Stime1$IFRVar[Stime1$date1==386])/7
t147<-ifelse(m142>m141,abs((m142-m141)/m141),-abs((m142-m141)/m141))
m141<-(Stime1$NVarVacc1[Stime1$date1==386]*Stime1$IFRVar[Stime1$date1==386]-Stime1$NVarVacc1[Stime1$date1==372]*Stime1$IFRVar[Stime1$date1==372])/14
m142<-(Stime1$NVarVacc1[Stime1$date1==400]*Stime1$IFRVar[Stime1$date1==400]-Stime1$NVarVacc1[Stime1$date1==386]*Stime1$IFRVar[Stime1$date1==386])/14
t1414<-ifelse(m142>m141,abs((m142-m141)/m141),-abs((m142-m141)/m141))

#rapporto incrementale More restrictions on mobility (put back of yellow area (with curfew 22.00-05.00) and introduction of free pass) (425° giorno)
m151<-(Stime1$NVarVacc1[Stime1$date1==425]*Stime1$IFRVar[Stime1$date1==425]-Stime1$NVarVacc1[Stime1$date1==418]*Stime1$IFRVar[Stime1$date1==418])/7
m152<-(Stime1$NVarVacc1[Stime1$date1==432]*Stime1$IFRVar[Stime1$date1==432]-Stime1$NVarVacc1[Stime1$date1==425]*Stime1$IFRVar[Stime1$date1==425])/7
t157<-ifelse(m152>m151,abs((m152-m151)/m151),-abs((m152-m151)/m151))
m151<-(Stime1$NVarVacc1[Stime1$date1==425]*Stime1$IFRVar[Stime1$date1==425]-Stime1$NVarVacc1[Stime1$date1==411]*Stime1$IFRVar[Stime1$date1==411])/14
m152<-(Stime1$NVarVacc1[Stime1$date1==439]*Stime1$IFRVar[Stime1$date1==439]-Stime1$NVarVacc1[Stime1$date1==425]*Stime1$IFRVar[Stime1$date1==425])/14
t1514<-ifelse(m152>m151,abs((m152-m151)/m151),-abs((m152-m151)/m151))

#rapporto incrementale less restrictions on in yellow area: gym opening (455° giorno)
m161<-(Stime1$NVarVacc1[Stime1$date1==455]*Stime1$IFRVar[Stime1$date1==455]-Stime1$NVarVacc1[Stime1$date1==448]*Stime1$IFRVar[Stime1$date1==448])/7
m162<-(Stime1$NVarVacc1[Stime1$date1==462]*Stime1$IFRVar[Stime1$date1==462]-Stime1$NVarVacc1[Stime1$date1==455]*Stime1$IFRVar[Stime1$date1==455])/7
t167<-ifelse(m162>m161,abs((m162-m161)/m161),-abs((m162-m162)/m161))
m161<-(Stime1$NVarVacc1[Stime1$date1==455]*Stime1$IFRVar[Stime1$date1==455]-Stime1$NVarVacc1[Stime1$date1==441]*Stime1$IFRVar[Stime1$date1==441])/14
m162<-(Stime1$NVarVacc1[Stime1$date1==469]*Stime1$IFRVar[Stime1$date1==469]-Stime1$NVarVacc1[Stime1$date1==455]*Stime1$IFRVar[Stime1$date1==455])/14
t1614<-ifelse(m162>m161,abs((m162-m161)/m161),-abs((m162-m162)/m161))

#rapporto incrementale less restrictions in yellow area: indoor catering (464° giorno)
m171<-(Stime1$NVarVacc1[Stime1$date1==464]*Stime1$IFRVar[Stime1$date1==464]-Stime1$NVarVacc1[Stime1$date1==457]*Stime1$IFRVar[Stime1$date1==457])/7
m172<-(Stime1$NVarVacc1[Stime1$date1==471]*Stime1$IFRVar[Stime1$date1==471]-Stime1$NVarVacc1[Stime1$date1==464]*Stime1$IFRVar[Stime1$date1==464])/7
t177<-ifelse(m172>m171,abs((m172-m171)/m171),-abs((m172-m171)/m171))
m171<-(Stime1$NVarVacc1[Stime1$date1==464]*Stime1$IFRVar[Stime1$date1==464]-Stime1$NVarVacc1[Stime1$date1==450]*Stime1$IFRVar[Stime1$date1==450])/7
m172<-(Stime1$NVarVacc1[Stime1$date1==478]*Stime1$IFRVar[Stime1$date1==478]-Stime1$NVarVacc1[Stime1$date1==464]*Stime1$IFRVar[Stime1$date1==464])/7
t1714<-ifelse(m172>m171,abs((m172-m171)/m171),-abs((m172-m171)/m171))

#rapporto incrementale less restrictions in yellow area: No curfew (484° giorno)
m181<-(Stime1$NVarVacc1[Stime1$date1==484]*Stime1$IFRVar[Stime1$date1==484]-Stime1$NVarVacc1[Stime1$date1==477]*Stime1$IFRVar[Stime1$date1==477])/7
m182<-(Stime1$NVarVacc1[Stime1$date1==491]*Stime1$IFRVar[Stime1$date1==491]-Stime1$NVarVacc1[Stime1$date1==484]*Stime1$IFRVar[Stime1$date1==484])/7
t187<-ifelse(m182>m181,abs((m182-m181)/m181),-abs((m182-m181)/m181))
m181<-(Stime1$NVarVacc1[Stime1$date1==484]*Stime1$IFRVar[Stime1$date1==484]-Stime1$NVarVacc1[Stime1$date1==470]*Stime1$IFRVar[Stime1$date1==470])/7
m182<-(Stime1$NVarVacc1[Stime1$date1==498]*Stime1$IFRVar[Stime1$date1==498]-Stime1$NVarVacc1[Stime1$date1==484]*Stime1$IFRVar[Stime1$date1==484])/7
t1814<-ifelse(m182>m181,abs((m182-m181)/m181),-abs((m182-m181)/m181))

#rapporto incrementale less restrictions semi-final and final of 2020 UEFA European Football Championship (502° giorno)
m191<-(Stime1$NVarVacc1[Stime1$date1==499]*Stime1$IFRVar[Stime1$date1==499]-Stime1$NVarVacc1[Stime1$date1==492]*Stime1$IFRVar[Stime1$date1==492])/7
m192<-(Stime1$NVarVacc1[Stime1$date1==511]*Stime1$IFRVar[Stime1$date1==511]-Stime1$NVarVacc1[Stime1$date1==504]*Stime1$IFRVar[Stime1$date1==504])/7
t197<-ifelse(m192>m191,abs((m192-m191)/m191),-abs((m192-m191)/m191))
m191<-(Stime1$NVarVacc1[Stime1$date1==499]*Stime1$IFRVar[Stime1$date1==499]-Stime1$NVarVacc1[Stime1$date1==485]*Stime1$IFRVar[Stime1$date1==485])/7
m192<-(Stime1$NVarVacc1[Stime1$date1==518]*Stime1$IFRVar[Stime1$date1==518]-Stime1$NVarVacc1[Stime1$date1==504]*Stime1$IFRVar[Stime1$date1==504])/7
t1914<-ifelse(m192>m191,abs((m192-m191)/m191),-abs((m192-m191)/m191))

#rapporto incrementale more restrictions mompulsory green pass for many group activities (530° giorno)
m201<-(Stime1$NVarVacc1[Stime1$date1==530]*Stime1$IFRVar[Stime1$date1==530]-Stime1$NVarVacc1[Stime1$date1==523]*Stime1$IFRVar[Stime1$date1==523])/7
m202<-(Stime1$NVarVacc1[Stime1$date1==537]*Stime1$IFRVar[Stime1$date1==537]-Stime1$NVarVacc1[Stime1$date1==530]*Stime1$IFRVar[Stime1$date1==530])/7
t207<-ifelse(m202>m201,abs((m202-m201)/m201),-abs((m202-m201)/m201))
m201<-(Stime1$NVarVacc1[Stime1$date1==530]*Stime1$IFRVar[Stime1$date1==530]-Stime1$NVarVacc1[Stime1$date1==516]*Stime1$IFRVar[Stime1$date1==516])/14
m202<-(Stime1$NVarVacc1[Stime1$date1==544]*Stime1$IFRVar[Stime1$date1==544]-Stime1$NVarVacc1[Stime1$date1==530]*Stime1$IFRVar[Stime1$date1==530])/14
t2014<-ifelse(m202>m201,abs((m202-m201)/m201),-abs((m202-m201)/m201))

#rapporto incrementale less restrictions school opening (572° giorno)
m211<-(Stime1$NVarVacc1[Stime1$date1==568]*Stime1$IFRVar[Stime1$date1==568]-Stime1$NVarVacc1[Stime1$date1==561]*Stime1$IFRVar[Stime1$date1==561])/7
m212<-(Stime1$NVarVacc1[Stime1$date1==582]*Stime1$IFRVar[Stime1$date1==582]-Stime1$NVarVacc1[Stime1$date1==575]*Stime1$IFRVar[Stime1$date1==575])/7
t217<-ifelse(m212>m211,abs((m212-m211)/m211),-abs((m212-m211)/m211))
m211<-(Stime1$NVarVacc1[Stime1$date1==568]*Stime1$IFRVar[Stime1$date1==568]-Stime1$NVarVacc1[Stime1$date1==554]*Stime1$IFRVar[Stime1$date1==554])/14
m212<-(Stime1$NVarVacc1[Stime1$date1==589]*Stime1$IFRVar[Stime1$date1==589]-Stime1$NVarVacc1[Stime1$date1==575]*Stime1$IFRVar[Stime1$date1==575])/14
t2114<-ifelse(m212>m211,abs((m212-m211)/m211),-abs((m212-m211)/m211))

#rapporto incrementale less restrictions back to working in presence (600° giorno)
m221<-(Stime1$NVarVacc1[Stime1$date1==600]*Stime1$IFRVar[Stime1$date1==600]-Stime1$NVarVacc1[Stime1$date1==593]*Stime1$IFRVar[Stime1$date1==593])/7
m222<-(Stime1$NVarVacc1[Stime1$date1==607]*Stime1$IFRVar[Stime1$date1==607]-Stime1$NVarVacc1[Stime1$date1==600]*Stime1$IFRVar[Stime1$date1==600])/7
t227<-ifelse(m222>m221,abs((m222-m221)/m221),-abs((m222-m221)/m221))
m221<-(Stime1$NVarVacc1[Stime1$date1==600]*Stime1$IFRVar[Stime1$date1==600]-Stime1$NVarVacc1[Stime1$date1==586]*Stime1$IFRVar[Stime1$date1==586])/14
m222<-(Stime1$NVarVacc1[Stime1$date1==614]*Stime1$IFRVar[Stime1$date1==614]-Stime1$NVarVacc1[Stime1$date1==600]*Stime1$IFRVar[Stime1$date1==600])/14
t2214<-ifelse(m222>m221,abs((m222-m221)/m221),-abs((m222-m221)/m221))

#rapporto incrementale more restrictions extension of mandatory vaccination and of green pass (643° giorno)
m231<-(Stime1$NVarVacc1[Stime1$date1==643]*Stime1$IFRVar[Stime1$date1==643]-Stime1$NVarVacc1[Stime1$date1==636]*Stime1$IFRVar[Stime1$date1==636])/7
m232<-(Stime1$NVarVacc1[Stime1$date1==650]*Stime1$IFRVar[Stime1$date1==650]-Stime1$NVarVacc1[Stime1$date1==643]*Stime1$IFRVar[Stime1$date1==643])/7
t237<-ifelse(m232>m231,abs((m232-m231)/m231),-abs((m232-m231)/m231))
m231<-(Stime1$NVarVacc1[Stime1$date1==643]*Stime1$IFRVar[Stime1$date1==643]-Stime1$NVarVacc1[Stime1$date1==629]*Stime1$IFRVar[Stime1$date1==629])/14
m232<-(Stime1$NVarVacc1[Stime1$date1==657]*Stime1$IFRVar[Stime1$date1==657]-Stime1$NVarVacc1[Stime1$date1==643]*Stime1$IFRVar[Stime1$date1==643])/14
t2314<-ifelse(m232>m231,abs((m232-m231)/m231),-abs((m232-m231)/m231))

#rapporto incrementale more restrictions mandatory ffp2 on public transport (673° giorno)
m241<-(Stime1$NVarVacc1[Stime1$date1==673]*Stime1$IFRVar[Stime1$date1==673]-Stime1$NVarVacc1[Stime1$date1==666]*Stime1$IFRVar[Stime1$date1==666])/7
m242<-(Stime1$NVarVacc1[Stime1$date1==680]*Stime1$IFRVar[Stime1$date1==680]-Stime1$NVarVacc1[Stime1$date1==673]*Stime1$IFRVar[Stime1$date1==673])/7
t247<-ifelse(m242>m241,abs((m242-m241)/m241),-abs((m242-m241)/m241))
m241<-(Stime1$NVarVacc1[Stime1$date1==673]*Stime1$IFRVar[Stime1$date1==673]-Stime1$NVarVacc1[Stime1$date1==659]*Stime1$IFRVar[Stime1$date1==659])/14
m242<-(Stime1$NVarVacc1[Stime1$date1==687]*Stime1$IFRVar[Stime1$date1==687]-Stime1$NVarVacc1[Stime1$date1==673]*Stime1$IFRVar[Stime1$date1==673])/14
t2414<-ifelse(m242>m241,abs((m242-m241)/m241),-abs((m242-m241)/m241))

#rapporto incrementale more school opening (686° giorno)
m251<-(Stime1$NVarVacc1[Stime1$date1==686]*Stime1$IFRVar[Stime1$date1==686]-Stime1$NVarVacc1[Stime1$date1==679]*Stime1$IFRVar[Stime1$date1==679])/7
m252<-(Stime1$NVarVacc1[Stime1$date1==693]*Stime1$IFRVar[Stime1$date1==693]-Stime1$NVarVacc1[Stime1$date1==686]*Stime1$IFRVar[Stime1$date1==686])/7
t257<-ifelse(m252>m251,abs((m252-m251)/m251),-abs((m252-m251)/m251))
m251<-(Stime1$NVarVacc1[Stime1$date1==686]*Stime1$IFRVar[Stime1$date1==686]-Stime1$NVarVacc1[Stime1$date1==672]*Stime1$IFRVar[Stime1$date1==672])/14
m252<-(Stime1$NVarVacc1[Stime1$date1==700]*Stime1$IFRVar[Stime1$date1==700]-Stime1$NVarVacc1[Stime1$date1==686]*Stime1$IFRVar[Stime1$date1==686])/14
t2514<-ifelse(m252>m251,abs((m252-m251)/m251),-abs((m252-m251)/m251))

TabDec<-round(as.data.frame(matrix(c(t17,t114,t27,t214,t37,t314,t47,t414,t57,t514,t67,t614,t77,t714,t87,t814,t97,t914,t107,t1014,t117,t1114,t127,t1214,t137,t1314,t147,t1414,t157,t1514,t167,t1614,t177,t1714,t187,t1814,t197,t1914,t207,t2014,t217,t2114,t227,t2214,t237,t2314,t247,t2414,t257,t2514),byrow=TRUE,ncol=2,nrow=25)),2)
colnames(TabDec)<-c("D7","D14")
TabDec$"D7"<-round(100*TabDec$"D7",0)
TabDec$"D14"<-round(100*TabDec$"D14",0)

TabDec$meas<-c("School closed","Stop mobility","Industrial lockdown","intraregional mobility",
               "free mobility","School opened",
               "Several restrictions (including 75% DAD high school)","Regional restrictions according to Rt","Incentives for Christmas shopping",
               "No mobility between regions","No mobility but 1 visit per day to parents within municipalities","Regional restrictions according to Rt",
               "high school opening (50-75% in precence)","More restrictions on mobility (equal rules between yellow and orande regions until Easter)","put back of yellow area (with curfew 22.00-05.00) and introduction of free pass",
               "in yellow area: gym opening","in yellow area: indoor catering","in yellow area: No curfew",
               "semi-final and final of 2020 UEFA European Football Championship","compulsory green pass for many group activities","school opening",
               "back to working in presence","extension of mandatory vaccination and of green pass","mandatory ffp2 on public transport","school opening")
Tab2<-cbind(TabInc,TabDec[,-3])
Tab2
write.csv(Tab2,file=paste(your_data_folder,"Tab2.csv",sep=""))