#####################################
# BabyWeights, or a simple excuse to do an exercise on Time Series
# This project analyses the (daily) weight registered by my new babyborn to see how I can forecast her growth over time
# Done with ♥ by Alberto Frison - August 2022
####################################


######
# 00. Load Libraries
library (tidyverse)
library (fpp2)

######
# 01. Import Data
data <- data.frame(readxl::read_xlsx("./data/BabyWeights.xlsx", sheet = 'Data'))

data %>%
  ggplot(aes(x=Date, y = Weigh..gr.)) +
  geom_point(size = 2, color = "Blue") 

# interpolating missing data 
data[2,2] <- data[1,2] + 55/6
data[3,2] <- data[2,2] + 55/6
data[4,2] <- data[3,2] + 55/6
data[5,2] <- data[4,2] + 55/6
data[6,2] <- data[5,2] + 55/6
data[10,2] <- (data[9,2] + data[11,2])/2
data[16,2] <- (data[15,2] + data[17,2])/2
data[19,2] <- (data[18,2] + data[20,2])/2
data[32,2] <- (data[31,2] + data[33,2])/2

#####
# 02. Setting up the Time Series
inds <- seq (from = as.Date("2022-07-18"), to = as.Date("2022-08-13"), by = "day")
data_ts <- ts(data$Weigh..gr., c(2022, as.numeric(format(inds[1], "%j"))), frequency = 365)


#####
# 03. Some Preliminary Analysìs on the TS
PP.test(data_ts) #p-value 0.2348 --> has unit root
Acf(data_ts)
Pacf(data_ts)



fit_arima <- auto.arima (data_ts, stepwise = FALSE, approximation = FALSE, trace = TRUE)
print(summary(fit_arima))   # 
checkresiduals(fit_arima)

fcst_arima <- forecast(fit_arima,h=21)

plot(fcst_arima)
