
#install.packages("rpart")

library(rpart)
data(kyphosis)
str(kyphosis)
head(kyphosis, n=10)


# Anzahl Beobachtungen
n_AnzahlBeobachtungen <- nrow(kyphosis)
# Trainingsdatensatz, zufällig mit70 % der Daten 
Training_i <- sample(nrow(kyphosis), 0.7 * n_AnzahlBeobachtungen)
# Training = 56 Beobachtungen
length(Training_i)

# Testdaten =Differenz aus Gesamtdaten und Training
Test_i <- setdiff(1:nrow(kyphosis), Training_i)
# Testdaten = 25 Beobachtungen
length(Test_i)
# Ausgabe der Vektoren, Kontrollen ob keine Doppelten
sort(Training_i); sort(Test_i)

# Extraktion der Daten für Training+Test aus der Quelle 
Training <- kyphosis[Training_i,]
str(Training)
Test <- kyphosis[Test_i,]
str(Test)

# Erlernung des Modells mit rpart
Anpassung <- rpart(Kyphosis ~ Age + Number + Start, data = Training)
print(Anpassung)
#Anzeige Baum + Text
plot(Anpassung, uniform=T,  branch = 0.7, main="Anzeige Modell mit rpart")
text(Anpassung, all=T, use.n=T, cex=0.7)

#Modell
summary(Anpassung)

# testdaten als Prediction
Vorhergesagt = predict(Anpassung, newdata=Test, type ="class")
# Testdaten und vorhergesagt zusamenführen
Kombination <- data.frame(Test, Vorhergesagt)
Kombination







