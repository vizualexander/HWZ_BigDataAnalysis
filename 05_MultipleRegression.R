############################################################################################
# Beispiel lineare Regression, anhand Prognoser zum Häuserpreis.
# Quelle: http://dni-institute.in/blogs/machine-learning-steps-to-build-regression-model/
############################################################################################


# Read data
#house.df <-read.csv(file="housing.csv", stringsAsFactors=F)
library("rio")
#datERP  <- import("https://github.com/vizualexander/HWZ_BigDataAnalysis/blob/master/05_housing.csv")
house.df  <- import("c:/Users/i328702/Documents/99/05_housing.csv")
# Explore data
summary(house.df)

#excluding rows with target variable missing values
house_df<- house.df[!is.na(house.df$MEDV),]

# Check distribution of target variable
class(house_df$MEDV)
hist(house_df$MEDV)
hist(house_df$MEDV,
     breaks=30,
     col="blue",
     border="white",
     xlab="Median House Price",
     ylab="Counts",
     main="Histogram: Median House Prices")


#Box Plot can also be used for exploring variables for outliers.
boxplot(house_df$CRIM, 
        horizontal = T,
        main="Per Capita Crime Rate")


# explore bivariate relationship between each of the independent variables and the target variable.
# Scatter plot 
plot(house_df$CRIM,
     house_df$MEDV,
     xlab="Crime Rate",
     ylab="Median Price",
     main="Scatter Plot: Crime rate and Median House Price",
     pch=20,
     col="blue"
)



# correlation between each of the combinations
library(corrplot)
correlations <- cor(house_df, use="everything")
corrplot(correlations, method="circle")



# Create Transformed variables.
house_df<- TransVars(house_df,c("MEDV"))
names(house_df)
correlations <- cor(house_df, use="everything")
corrplot(correlations, method="circle")

library(Hmisc)
rcorr_mat <- rcorr(as.matrix(house_df))
str(rcorr_mat)
rcorr_df <- data.frame(rcorr_mat$r)
# find correction with median prices
rcorr_df1 <- rcorr_df["MEDV",]



# Create Train and Test Samples
htrain <- sample(1:nrow(house_df),0.6*nrow(house_df))
train_house <- house_df[htrain,]
test_house <- house_df[-htrain,]




# Create regression formula
varlist <- names(train_house)
varlist <- varlist[!varlist %in% c("MEDV")]
house_formula <- as.formula(paste("MEDV",paste(varlist,collapse = "+"),sep="~"))
# Regression MOdel fitting
house_reg <- lm(house_formula,
                data=train_house)



# Model Development - Regression - Stepwise
library(MASS)
help(step)
step(house_reg,
     direction = "both", k=qchisq(0.025,1,lower.tail=FALSE))
house_reg <- lm(MEDV ~ NOX + RM + DIS + LSTAT + sqr_CRIM + log_NOX + 
                  sqrt_NOX + exp_NOX + sqr_NOX + sqrt_RM + exp_RM + sqr_RM + 
                  log_DIS + sqrt_DIS + exp_DIS + sqr_DIS + sqrt_RAD + log_TAX + 
                  sqrt_PTRATIO + log_LSTAT + exp_LSTAT + sqr_LSTAT,
                data=train_house)
summary(house_reg)



#Multicolleaity Check
house_reg <- lm(MEDV ~ NOX + 
                  sqr_CRIM + 
                  sqr_RM + 
                  log_DIS + 
                  sqrt_RAD + 
                  log_TAX + 
                  sqrt_PTRATIO +
                  log_LSTAT ,
                data=train_house)

summary(house_reg)
vif(house_reg)



# qq plot for studentized resid
qqPlot(house_reg, main="QQ Plot")





# distribution of studentized residuals
library(MASS)
sresid <- studres(house_reg) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
