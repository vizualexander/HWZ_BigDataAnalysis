

install.packages("ROCR")
require("ROCR")

cls = c('P', 'P', 'N', 'P', 'P', 'P', 'N', 'N', 'P', 'N', 'P', 
        'N', 'P', 'N', 'N', 'N', 'P', 'N', 'P', 'N')
score = c(0.9, 0.8, 0.7, 0.6, 0.55, 0.51, 0.49, 0.43, 
          0.42, 0.39, 0.33, 0.31, 0.23, 0.22, 0.19, 
          0.15, 0.12, 0.11, 0.04, 0.01)

pred = prediction(score, cls)
gain = performance(pred, "tpr", "rpp")

plot(gain, col="orange", lwd=2)




plot(x=c(0, 1), y=c(0, 1), type="l", col="red", lwd=2,
     ylab="True Positive Rate", 
     xlab="Rate of Positive Predictions")
lines(x=c(0, 0.5, 1), y=c(0, 1, 1), col="darkgreen", lwd=2)

gain.x = unlist(slot(gain, 'x.values'))
gain.y = unlist(slot(gain, 'y.values'))

lines(x=gain.x, y=gain.y, col="orange", lwd=2)