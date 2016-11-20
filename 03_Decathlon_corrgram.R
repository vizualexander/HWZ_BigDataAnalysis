
library(corrgram)
library("rio")

dataOlympic <- import("C:/Temp/03_LondonOlympics2012DecathlonX.csv")
str(dataOlympic)

corrgram(dataOlympic, order=NULL, lower.panel=panel.shade,
         upper.panel=NULL, text.panel=panel.txt,
         main="Decathlon Correlation")

ls()
rm(dataOlympic)


