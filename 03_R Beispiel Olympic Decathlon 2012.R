#Alexander Klein
#22.10.2016

library("rio",plyr)

#load Dataset
dataOlympic <- import("https://github.com/vizualexander/datasets/blob/master/03_LondonOlympics2012Decathlon.csv")
str(dataOlympic)

#statistische Funktionen und Berechnungen
midLongJump <- mean(dataOlympic$LongJump)
midLongJump
medLongJum  <- median(dataOlympic$LongJump)
medLongJum  
maxLongJump <- max(dataOlympic$LongJump)
maxLongJump
minLongJum  <- min(dataOlympic$LongJump)
minLongJum  
difMedMidLongJumpValue <- round(abs(midLongJump-medLongJum),2)
difMedMidLongJumpValue 
difMedMidLongJumpPercent <- round(100 / (maxLongJump-minLongJum)*difMedMidLongJumpValue,2) 
difMedMidLongJumpPercent 

#Scatterplot
attach(dataOlympic)
plot(LongJump, HighJump, main="Scatterplot - Olympic Decathlon 2012", 
  	xlab="Long Jump ", ylab="High Jump ", pch=19)

#Scatterplot Matrix
pairs(~LongJump+HighJump+Hurdles+ShotPut,data=dataOlympic, 
   main="Scatterplot Matrix - Olympic Decathlon 2012")

#colored Scatterplot
qplot(HundredMetresSprint, FourHundredMetresRun, colour = Name, data = dataOlympic)

