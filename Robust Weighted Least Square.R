# Import library
library(MASS)
library(faraway)
library(readxl)
# Import data
dataset <- read_excel("DATA2.xlsx")

# Analisis deskriptif
summary(dataset)

# Model OLS
m.ols <- lm(IG ~ SMA + PDRB + IPM + TK + IIK, data = dataset)

# Uji DFFITS
dffits(m.ols)

# find number of predictors in model
p <- length(m.ols$coefficients)-1

# find number of observations
n <- nrow(dataset)

# Hitung nilai batas DFFITS
thresh <- 2*sqrt(p/n)
print(thresh)

# plot nilai DFFITS
plot(dffits(m.ols), type = 'h',main="DFFITS Value Plot")

# tambahkan garis batas horizontal
abline(h = thresh, lty = 2)
abline(h = -thresh, lty = 2)

## Boxplot
boxplot(dataset$IG,main="IG Boxplot")
boxplot(dataset$SMA,main="SMA Boxplot")
boxplot(dataset$PDRB,main="PDRB Boxplot")
boxplot(dataset$IPM,main="IPM Boxplot")
boxplot(dataset$TK,main="TK Boxplot")
boxplot(dataset$IIK,main="IIK Boxplot")

# Uji Asumsi Klasik
#Uji Normalitas
shapiro.test(residuals(m.ols))

#Uji Heterosskedastisitas
lmtest::bptest(m.ols)

#Uji Autokorelasi
lmtest::dwtest(m.ols)

#Uji Multikolinearitas
car::vif(m.ols)

# Implementasi RWLS
# Model LTS
FitAll.rr = rlm(IG ~ SMA + PDRB + IPM + TK + IIK, data = dataset, init = "lts")
summary(FitAll.rr) # # Coefficients

hweights1 <- data.frame(IG=dataset$IG,  #Observation 
                       resid = FitAll.rr$resid, #Residual
                       weight = FitAll.rr$w, #Weight
                       predict = FitAll.rr$fitted.values) # Fitted value/predicted 
hweights1 <- hweights1[order(FitAll.rr$w), ] 

hweights1 %>% kable(digits = 2)

# Weighted Least Square dengan fungsi pembobot Huber
FitAll.rr.2 = rlm(IG ~ SMA + PDRB + IPM + TK + IIK, data = dataset,
                  psi = psi.huber)
summary(FitAll.rr.2) # Coefficients

hweights2 <- data.frame(IG=dataset$IG,  #Observation 
                       resid = FitAll.rr.2$resid, #Residual
                       weight = FitAll.rr.2$w, #Weight
                       predict = FitAll.rr.2$fitted.values) # predicted values
hweights2 <- hweights2[order(FitAll.rr.2$w), ] 

hweights2 %>% kable(digits=2)

# Model RWLS/LTS dengan pembobot Huber 
FitAll.rr.3 = rlm(IG ~ SMA + PDRB + IPM + TK + IIK, data = dataset,
                  init = "lts", psi = psi.huber)
summary(FitAll.rr.3) # Coefficients

hweights3 <- data.frame(IG=dataset$IG,  #Observation 
                       resid = FitAll.rr.3$resid, #Residual
                       weight = FitAll.rr.3$w, #Weight
                       predict = FitAll.rr.3$fitted.values) # predicted values
hweights3 <- hweights3[order(FitAll.rr.3$w), ] 

hweights3 %>% kable(digits=2)

# Pemilihan model terbaik
# Menghitung R square
calculate_r_square <- function(actual, predicted) {
  mean_actual <- mean(actual)
  ss_total <- sum((actual - mean_actual)^2)
  ss_residual <- sum((actual - predicted)^2)
  r_square <- 1 - (ss_residual/ss_total)
  return(r_square)
}

# Menghitung RMSE
calculate_rmse <- function(actual, predicted) {
  mse <- mean((actual - predicted)^2)
  rmse <- sqrt(mse)
  return(rmse)
}

# Menghitung MAPE
calculate_mape <- function(actual, predicted) {
  mape <- mean(abs((actual - predicted)/actual)) * 100
  return(mape)
}

# Implementasi
# Model R Square
LTS.Rsq <- calculate_r_square(dataset$IG,FitAll.rr$fitted.values)
WLS.Rsq <- calculate_r_square(dataset$IG,FitAll.rr.2$fitted.values)
RWLS.Rsq <- calculate_r_square(dataset$IG,FitAll.rr.3$fitted.values)

# Model RMSE
LTS.RMSE <- calculate_rmse(dataset$IG,FitAll.rr$fitted.values)
WLS.RMSE <- calculate_rmse(dataset$IG,FitAll.rr.2$fitted.values)
RWLS.RMSE <- calculate_rmse(dataset$IG,FitAll.rr.3$fitted.values)

# Model MAPE
LTS.MAPE <- calculate_mape(dataset$IG,FitAll.rr$fitted.values)
WLS.MAPE <- calculate_mape(dataset$IG,FitAll.rr.2$fitted.values)
RWLS.MAPE <- calculate_mape(dataset$IG,FitAll.rr.3$fitted.values)

# Summary
Diagnostic <- cbind(rbind(LTS.Rsq,WLS.Rsq,RWLS.Rsq),
                    rbind(LTS.RMSE,WLS.RMSE,RWLS.RMSE),
                    rbind(LTS.MAPE,WLS.MAPE,RWLS.MAPE))
colnames(Diagnostic) <- c("R Square","RMSE","MAPE")
rownames(Diagnostic) <- c("LTS","WLS","RWLS")
print(Diagnostic)
