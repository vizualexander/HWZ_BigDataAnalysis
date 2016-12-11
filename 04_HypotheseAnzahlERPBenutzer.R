

library("rio")
#datERP  <- import("https://github.com/vizualexander/HWZ_BigDataAnalysis/blob/master/04_HypotheseAnzahlERPBenutzer.csv")
datERP  <- import("c:/Users/i328702/Documents/99/04_HypotheseAnzahlERPBenutzer.csv")


#1-factor ANOVA = t-test
fit <- aov(AnzahlERPBenutzer ~ Branche + Gruendungsjahr, data=datERP)
summary(fit)
# gleiches Resultat mit Regression
anova (lm(AnzahlERPBenutzer ~ Branche + Gruendungsjahr, data=datERP))



#1-factor ANOVA = t-test
fit <- aov(AnzahlERPBenutzer ~ AnzahlMitarbeiter + Branche, data=datERP)
summary(fit)
# gleiches Resultat mit Regression
anova (lm(AnzahlERPBenutzer ~ AnzahlMitarbeiter + Branche, data=datERP))