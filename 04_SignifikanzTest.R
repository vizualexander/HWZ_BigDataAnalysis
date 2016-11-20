
#Data1
Lohn1 <- c(40, 57, 45, 55, 58, 57, 64, 55, 62, 65)
Land1 <- factor(c(rep("CH",5), rep("FR",5)))
dataSigni1 <- data.frame(Land1, Lohn1)
tapply(Lohn1,Land1,mean) 
oneway_test(Lohn1 ~ Land1, data=dataSigni1, distribution="exact")

#Data2 (willkürliche Daten, nicht korrelierend mit vorherigen, untersch. Mittelwerte)
Lohn2 <- c(40, 57, 45, 55, 58, 57, 64, 55, 62, 65, 46, 49, 52, 55, 61, 40, 57, 55, 58, 62)
Land2 <- factor(c(rep("CH",10), rep("FR",10)))
dataSigni2 <- data.frame(Land2, Lohn2)
tapply(Lohn2,Land2,mean)
oneway_test(Lohn2 ~ Land2, data=dataSigni2, distribution="exact")

#Data3
LohnCH <- rep(c(40, 57, 45, 55, 58),10) # 10 x erste 5 Werte für CH
LohnFR <- rep(c(57, 64, 55, 62, 65),10) # 10 x letzte 5 Werte für GB
Lohn3 <- append(LohnCH,LohnFR)
Land3 <- factor(c(rep("CH",50), rep("FR",50)))
dataSigni3 <- data.frame(Land3, Lohn3)
tapply(Lohn3,Land3,mean)
oneway_test(Lohn3 ~ Land3, data=dataSigni3, distribution="exact")

#Data4
LohnCH <- rep(c(40, 57, 45, 55, 58),50) # 50 x erste 5 Werte für CH
LohnFR <- rep(c(57, 64, 55, 62, 65),50) # 50 x letzte 5 Werte für GB
Lohn4 <- append(LohnCH,LohnFR)
Land4 <- factor(c(rep("CH",250), rep("FR",250)))
dataSigni4 <- data.frame(Land4, Lohn4)
tapply(Lohn4,Land4,mean)
oneway_test(Lohn4 ~ Land4, data=dataSigni4, distribution="exact")

#Data5
#LohnCH <- rep(c(40, 57, 45, 55, 58),500) # 500 x erste 5 Werte für CH
#LohnFR <- rep(c(57, 64, 55, 62, 65),500) # 500 x letzte 5 Werte für GB
#Lohn5 <- append(LohnCH,LohnFR)
#Land5 <- factor(c(rep("CH",2500), rep("FR",2500)))
#dataSigni5 <- data.frame(Land5, Lohn5)
#tapply(Lohn5,Land5,mean)
#oneway_test(Lohn5 ~ Land5, data=dataSigni5, distribution="exact")
#Error in cSR_shift_2sample(scores, m, fact = fact) : 
#  overflow error; cannot compute exact distribution

