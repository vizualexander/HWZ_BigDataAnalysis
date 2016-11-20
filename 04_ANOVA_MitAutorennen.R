################################################################################
# Beispiel 1 - Statistik Autorennen mit Fahrzeug und Anzahl Siegen pro Jahr
# Hypothese: Anzahl Siege sind abhängig von Fahrzeug
################################################################################
Fahrzeug <- c("Mercedes","Mercedes","Mercedes","Mercedes","BMW","BMW","BMW","BMW","Ferrari","Ferrari","Ferrari","Ferrari")
Jahr <- c(2012,2013,2014,2015,2012,2013,2014,2015,2012,2013,2014,2015)
Siege <- c(21,18,18,17,23,17,21,20,24,23,20,22)
motorRace1 <- data.frame(Fahrzeug,Jahr,Siege)

library(ggplot2)
ggplot(motorRace1,aes(x=Jahr,y=Siege,fill=factor(Fahrzeug)))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_discrete(name="Fahrzeug",
                      breaks=c(1,2,3),
                      labels=c("Mercedes","Ferrari","BMW"))+ scale_fill_hue(name="Fahrzeug") +
  ggtitle("Siege pro Jahr und Fahrzeug") + xlab("Jahr") + ylab("Siege")


#1-factor ANOVA = t-test
fit1 <- aov(Siege ~ Jahr + Fahrzeug, data=motorRace1)
summary(fit1)
# gleiches Resultat mit Regression
anova (lm(Siege ~Jahr + Fahrzeug, data=motorRace1))



################################################################################
# Beispie 2 - Statistik Autorennen mit Fahrzeug und Anzahl Siegen pro Jahr
# Hypothese: Anzahl Siege sind abhängig von Fahrzeug
################################################################################
Fahrzeug <- c("Mercedes","Mercedes","Mercedes","Mercedes","Mercedes","BMW","BMW","BMW","BMW","BMW","Ferrari","Ferrari","Ferrari","Ferrari","Ferrari")
Jahr <- c(2012,2013,2014,2015,2016,2012,2013,2014,2015,2016,2012,2013,2014,2015,2016)
Siege <- c(21,18,18,17,23,17,21,20,24,23,20,22,20,21,17)
motorRace2 <- data.frame(Fahrzeug,Jahr,Siege)

library(ggplot2)
ggplot(motorRace2,aes(x=Jahr,y=Siege,fill=factor(Fahrzeug)))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_discrete(name="Fahrzeug",
                      breaks=c(1,2,3),
                      labels=c("Mercedes","Ferrari","BMW"))+ scale_fill_hue(name="Fahrzeug") +
  ggtitle("Siege pro Jahr und Fahrzeug") + xlab("Jahr") + ylab("Siege")


#1-factor ANOVA = t-test
fit2 <- aov(Siege ~ Jahr + Fahrzeug, data=motorRace2)
summary(fit2)
# gleiches Resultat mit Regression
anova (lm(Siege ~Jahr + Fahrzeug, data=motorRace2))





################################################################################
# Beispie 3 - Statistik Autorennen mit Fahrzeug und Anzahl Siegen pro Jahr
# Hypothese: Anzahl Siege sind abhängig von Fahrzeug
################################################################################
Fahrzeug <- c("Mercedes","Mercedes","Mercedes","BMW","BMW","BMW","Ferrari","Ferrari","Ferrari")
Jahr <- c(2014,2015,2016,2014,2015,2016,2014,2015,2016)
Siege <- c(21,18,17,23,21,23,22,20,17)
motorRace3 <- data.frame(Fahrzeug,Jahr,Siege)

library(ggplot2)
ggplot(motorRace3,aes(x=Jahr,y=Siege,fill=factor(Fahrzeug)))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_discrete(name="Fahrzeug",
                      breaks=c(1,2,3),
                      labels=c("Mercedes","Ferrari","BMW"))+ scale_fill_hue(name="Fahrzeug") +
  ggtitle("Siege pro Jahr und Fahrzeug") + xlab("Jahr") + ylab("Siege")


#1-factor ANOVA = t-test
fit3 <- aov(Siege ~ Jahr + Fahrzeug, data=motorRace3)
summary(fit3)
# gleiches Resultat mit Regression
anova (lm(Siege ~Jahr + Fahrzeug, data=motorRace3))
