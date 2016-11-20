

library("rio")
#datWorld  <- import("https://github.com/vizualexander/HWZ_BigDataAnalysis/blob/master/05_world_data.csv")
datWorld  <- import("c:/Users/i328702/Documents/99/05_world_data.csv")

#Typkonvertierung
datWorld$fertility_2012 = as.numeric(datWorld$fertility_2012)
datWorld[which(datWorld$woman_right_2011==-77),"woman_right_2011"] = 0
datWorld$woman_right_2011 = as.factor(datWorld$woman_right_2011)
#Struktur
head(datWorld)
summary(datWorld)

#Hypothese 1, Berechnung übe rlm(Abhängige Variable + unabhängige Variablen)
model01 = lm(fertility_2012 ~ educ_2012, data=datWorld)
summary(model01)

#Grafische Ergebnisdarstellung
library(ggplot2)
ggplot(datWorld, aes(x=educ_2012,y=fertility_2012)) + geom_point() +
  geom_smooth(method="lm") + theme_classic() +
  ylab("Anzahl Kinder") +
  xlab("obligatorische Schulzeit")


#Hypothese 2, Berechnung übe rlm(Abhängige Variable + unabhängige Variablen)
model02 = lm(fertility_2012 ~ woman_right_2011, data=datWorld)
summary(model02)

#Grafische Ergebnisdarstellung
ggplot(datWorld, aes(x=woman_right_2011,y=fertility_2012)) + geom_point() +
  geom_smooth(method="lm") + theme_classic() +
  ylab("Anzahl Kinder") +
  xlab("politische Rechte der Frauen")


#lineare Regression mit mehreren Variablen
model03 = lm(fertility_2012 ~ educ_2012 + woman_right_2011, data=datWorld)
summary(model03)


#Bestimmung der relative Wichtigkeit der einzelnen Variablen, relaimpo
library(relaimpo)
calc.relimp(model03, type="lmg",rela=T,rank=T)
