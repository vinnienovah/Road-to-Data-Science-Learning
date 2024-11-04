# Load tidyverse
library(tidyverse)

# url where the data is located
url <- "https://dax-cdn.cdn.appdomain.cloud/dax-airline/1.0.1/lax_to_jfk.tar.gz"

# download the file
download.file(url, destfile = "lax_to_jfk.tar.gz")

# if you run this on your local machine, then can remove tar = "internal" 
untar("lax_to_jfk.tar.gz") #, tar = "internal")

# read_csv only 
sub_airline <- read_csv("lax_to_jfk/lax_to_jfk.csv",
                        col_types = cols('DivDistance' = col_number(), 
                                         'DivArrDelay' = col_number()))
#1. Simple Linear Regression
#Fit the data into a linear regression model
# Define dataset with just AA as the Reporting_Airline
aa_delays <- sub_airline %>%
  filter(CarrierDelay != "NA", Reporting_Airline == "AA")

head(aa_delays)

linear_model <- lm(ArrDelayMinutes ~ DepDelayMinutes, data = aa_delays)
summary(linear_model)

# Input data we use to predict
new_depdelay <- data.frame(
  DepDelayMinutes = c(12, 19, 24))

# Predict the data points
pred <- predict(linear_model, newdata = new_depdelay, interval = "confidence")
pred

linear_model$coefficients

#Create a linear function with "CarrierDelay" as the predictor variable and the "ArrDelayMinutes" as the response variable.
lin_model <- lm(ArrDelayMinutes ~ CarrierDelay, data = aa_delays)
summary(lin_model)

#Find the coefficients (intercept and slope) of the model.
lin_model$coefficients


#Multiple Linear Regression
mlr <- lm(ArrDelayMinutes ~ DepDelayMinutes + LateAircraftDelay, data = aa_delays)

summary(mlr)


#coefficients
mlr$coefficients

#Create and train a Multiple Linear Regression model "mlr2" where the response variable is ArrDelayMinutes, and the predictor variable is 'DepDelayMinutes', 'LateAircraftDelay' and 'CarrierDelay'.
mlr2 <- lm(
  ArrDelayMinutes ~ DepDelayMinutes + 
    LateAircraftDelay + CarrierDelay, 
  data = aa_delays)

summary(mlr2)      

#Find the coefficients of the model?
mlr2$coefficients

# New data points
DepDelayMinutes <- c(10, 20, 30)
LateAircraftDelay <- c(20, 60, 30)
new_multidelay <- data.frame(DepDelayMinutes, LateAircraftDelay)

predd<-predict(mlr, newdata = new_multidelay, interval = 'confidence')
pred

#Assessing Models Visually
#Regression Plot
ggplot(aa_delays, aes(x = DepDelayMinutes, y = ArrDelayMinutes)) +
  geom_point() + 
  stat_smooth(method = "lm", col = "red")

#Create a regression plot of "CarrierDelay" and "ArrDelayMinutes" using "aa_delays" dataset
ggplot(aa_delays, aes(x = CarrierDelay, y = ArrDelayMinutes))+
  geom_point()+
  stat_smooth(method = 'lm', col = 'red')


# The variable "DepDelayMinutes" has a stronger correlation with "ArrDelayMinutes", it is approximately 0.871  compared to "CarrierDelay" which is approximately 0.624. You can verify it using the following commands:
cor(aa_delays$DepDelayMinutes, 
    aa_delays$ArrDelayMinutes)
cor(aa_delays$CarrierDelay, 
    aa_delays$ArrDelayMinutes)

#Residual Plot
aa_delays <- sub_airline %>%
  filter(CarrierDelay != "NA", Reporting_Airline == "AA")
score_model <- lm(ArrDelayMinutes ~ DepDelayMinutes, data = aa_delays)
aa_delays$predicted <- predict(score_model)

ggplot(aa_delays, aes(x = DepDelayMinutes, y = ArrDelayMinutes)) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Plot regression slope
  geom_segment(aes(xend = DepDelayMinutes, yend = predicted), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()  # Add theme for cleaner look


ggplot(lm(ArrDelayMinutes ~ DepDelayMinutes, data = aa_delays)) +
  geom_point(aes(x=DepDelayMinutes, y=.resid))

#Other Diagnostic Plots
linear_model <- lm(ArrDelayMinutes ~ DepDelayMinutes, data = aa_delays)
plot(linear_model)

#Polynomial Regression
set.seed(20)
x <- seq(from=0, to=20, by=0.1)

# value to predict (y):
y <- 500 + 0.4 * (x-10)^3

# some noise is generated and added to the real signal (y):
noise <- rnorm(length(x), mean=10, sd=80)
noisy.y <- y + noise


# fit linear model
ggplot(data=NULL,aes(x, noisy.y)) + 
  geom_point() + 
  geom_smooth(method = "lm")

ggplot(data=NULL,aes(x, noisy.y)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 5))

#Polynomial 2nd Order
time <- 6:19
temp <- c(4,6,7,9,10,11,11.5,12,12,11.5,11,10,9,8)

ggplot(data = NULL, aes(time, temp)) + 
  geom_point() +
  geom_smooth()


polyfit2 <- lm(temp ~ poly(time, 2, raw = TRUE))

summary(polyfit2)
polyfit2$coefficients

ggplot(data = NULL, aes(time, temp)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2)) 

#Create a 4th order polynomial model with the variables time and temp from above and display the summary of the model.
polyfit4<-lm(temp~poly(time, 4, raw =  TRUE))
summary(polyfit4)

polyfit4$coefficients


#Assessing the Model
linear_model <- lm(ArrDelayMinutes ~ DepDelayMinutes, aa_delays)
mse <- mean(linear_model$residuals^2)
mse
rmse <- sqrt(mse)
rmse

summary(linear_model)$r.squared



mlr <- lm(ArrDelayMinutes ~ DepDelayMinutes + LateAircraftDelay, data = aa_delays)
mse_mlr <- mean(mlr$residuals^2)
mse_mlr

rmse_mlr <- sqrt(mse_mlr)
rmse_mlr

summary(mlr)$r.squared

poly_reg <- lm(ArrDelayMinutes ~ poly(DepDelayMinutes, 3), data = aa_delays)
mse_poly <- mean(poly_reg$residuals^2)
mse_poly
rmse<-sqrt(mse_poly)
rmse
summary(poly_reg)$r.squared


#Prediction and Decision Making
# For example we want to predict the score model we created in a previous section
head(predict(score_model))
