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
any(is.na(data))
data[which(is.na(data$Weigh..gr.)),]
data <- mutate (data, type = "")
# calculate the week - starting from 01 - up to last day entered
n <- nrow(data)
m <- n %/% 7
o <- n %% 7 
data <- mutate (data, week = c(rep(1:m, each = 7, times = 1), rep(m+1, each = o)))

# interpolating missing data with averages
data$type <- ifelse (is.na(data$Weigh..gr.), data$type <- "interpolated", data$type <- "original")
data[2,2] <- data[1,2] + 55/6
data[3,2] <- data[2,2] + 55/6
data[4,2] <- data[3,2] + 55/6
data[5,2] <- data[4,2] + 55/6
data[6,2] <- data[5,2] + 55/6
data[10,2] <- (data[9,2] + data[11,2])/2
data[16,2] <- (data[15,2] + data[17,2])/2
data[19,2] <- (data[18,2] + data[20,2])/2
data[32,2] <- (data[31,2] + data[33,2])/2
data[35,2] <- (data[34,2] + data[36,2])/2
data[38,2] <- (data[37,2] + data[39,2])/2

# 01a. Printing
data %>%
  ggplot(aes(x = Date, y = Weigh..gr., color = type)) +
  geom_point(size = 2) +
  labs(title = "Daily Weight Report", subtitle = "Daily Weight Record in gramms from week 1") +
  xlab ("Date") +
  ylab ("Weight gr.")

# 01b. Average Weekly Gain
data %>%
  group_by (week) %>%
  mutate (avg_gain = (last (Weigh..gr.) - first(Weigh..gr.)) / n()) %>%
  select (week, avg_gain) %>%
  distinct() %>%
  ggplot (aes(x = week, y = avg_gain)) +
  geom_point(size = 4, color = "Red") +
  labs(title = "Average Daily Weight Increase", subtitle = "Average Daily Weight Increase in gramms from week 1") +
  xlab ("Week Number") +
  ylab ("Average Gain")


#####
# 02. Setting up the Time Series
inds <- seq (from = as.Date("2022-07-18"), to = as.Date("2022-08-21"), by = "day")
data_ts <- ts(data$Weigh..gr., c(2022, as.numeric(format(inds[1], "%j"))), frequency = 365)


#####
# 03. Some Preliminary analysìs on the TS
PP.test(data_ts) #p-value 0.05299 --> has unit root
Acf(data_ts)
Pacf(data_ts)

#####
# 04. Forecasting - still under development
fit_arima <- auto.arima (data_ts, stepwise = FALSE, approximation = FALSE, trace = TRUE) #Sigma^2 860.9
print(summary(fit_arima))   # 
checkresiduals(fit_arima) # Ljung Box Test - p-value 0.12 ==> Residuals are independent [OK] - https://www.statology.org/ljung-box-test/

fcst_arima <- forecast(fit_arima,h=21)

plot(fcst_arima)
