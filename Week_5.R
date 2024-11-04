# Install tidymodels if you haven't done so
# install.packages("rlang")
# install.packages("tidymodels")


R.Version()

# Library for modeling
library(tidymodels)

library(rlang)# Load tidyverse
library(tidyverse)


# url where the data is located
url <- "https://dax-cdn.cdn.appdomain.cloud/dax-airline/1.0.1/lax_to_jfk.tar.gz"

# download the file
download.file(url, destfile = "lax_to_jfk.tar.gz")

# untar the file so we can get the csv only
# if you run this on your local machine, then can remove tar = "internal" 
untar("lax_to_jfk.tar.gz", tar = "internal")

# read_csv only 
sub_airline <- read_csv("lax_to_jfk/lax_to_jfk.csv",
                        col_types = cols('DivDistance' = col_number(), 
                                         'DivArrDelay' = col_number()))

###Model Evaluation
##1.1 Training and Testing Data
flight_delays <- sub_airline %>% 
  replace_na(list(CarrierDelay = 0,
                  WeatherDelay = 0,
                  NASDelay = 0,
                  SecurityDelay = 0,
                  LateAircraftDelay = 0)) %>%
  select(c(ArrDelayMinutes, DepDelayMinutes, CarrierDelay, WeatherDelay, NASDelay, SecurityDelay, LateAircraftDelay, DayOfWeek, Month))

#splitting the data
set.seed(1234)
flight_split <- initial_split(flight_delays)
train_data <- training(flight_split)
test_data <- testing(flight_split)
test_data

#Use the function "initial_split" to split up the data set such that 80% of the data samples will be utilized for training. 
flight_split2 <- initial_split(flight_delays, prop = 0.8)
train_data2 <- training(flight_split2)
test_data2 <- testing(flight_split2)
test_data2

##1.2 Training a Model
#Pick a linear regression model
lm_spec <- linear_reg() %>%
  #set engine
  set_engine(engine = "lm")

#Print the linear function
lm_spec


#Now, we use fit() to fit the model we just specified in lm_spec.
train_fit <- lm_spec %>%
  fit(ArrDelayMinutes ~ DepDelayMinutes, data = train_data)

train_fit

#Now, lets look at how well the above model is predicting the original training data
train_results <- train_fit %>%
  #make the predictions and save the predicted values
  predict(new_data = train_data) %>%
  #create a new column to save the true values
  mutate(truth = train_data$ArrDelayMinutes)
head(train_results)

#we can do the same thing on the test_data
test_results <- train_fit %>%
  predict(new_data = test_data) %>%
  mutate(truth = test_data$ArrDelayMinutes)
head(test_results)

#1.3 Evaluating the Model
#let's use RMSE
rmse(train_results, truth = truth, estimate = .pred)

rmse(test_results, truth = truth, estimate = .pred)

#now let's use R_squared
rsq(train_results, truth = truth, estimate = .pred)

rsq(test_results, truth= truth, estimate = .pred)

windows()
#plot to visualize how well we predicted the Arrival Delay Minutes.
test_results %>%
  mutate(train = "testing") %>%
  bind_rows(train_results %>% mutate(train = "training")) %>%
  ggplot(aes(truth, .pred)) +
  geom_abline(lty = 2, color = "orange", 
              size = 1.5) +
  geom_point(color = '#006EA1', 
             alpha = 0.5) +
  facet_wrap(~train) +
  labs(x = "Truth", 
       y = "Predicted Arrival Delays (min)")


#Using "ArrDelayMinutes" as the response variable and "DepDelayMinutes" as the predictor variable, find the R^2 on the test data using 80% of the data for training data.
#use train_data2 from question 1.
train_fit2 <- lm_spec %>% 
  fit(ArrDelayMinutes ~ DepDelayMinutes, 
      data = train_data2)
test_results2 <- train_fit2 %>%
  # Make the predictions and save the predicted values
  predict(new_data = test_data2) %>%
  # Create a new column to save the true values
  mutate(truth = test_data2$ArrDelayMinutes)
rsq(test_results2, truth = truth,
    estimate = .pred)

#1.4 Cross validation
#To perform cross validation, you can use vfold_cv(). Setting v = 10 means that it will use 10 folds
set.seed(1234)
cv_folds <- vfold_cv(train_data, v = 10)
results <- fit_resamples(lm_spec, 
                         ArrDelayMinutes ~ DepDelayMinutes,
                         resamples = cv_folds)
results %>% collect_metrics()

#Calculate the average RMSE and R-squared using three folds utilizing DepDelayMinutes as a feature :

set.seed(1234)
cv_folds3 <- vfold_cv(train_data, v = 3)
results <- fit_resamples(lm_spec, 
                         ArrDelayMinutes ~ DepDelayMinutes,
                         resamples = cv_folds3)
results %>% collect_metrics()

#2. Overfitting, Underfitting and Model Selection
# the model is defined a line set to the mean of the car's stopping distance
windows()
ggplot(cars, aes(x = speed, y = dist)) + 
  geom_point() + 
  geom_hline(yintercept = mean(cars$dist), 
             col = "red") 

#overfittin
ggplot(cars, aes(x = speed, y = dist)) + 
  geom_point() + 
  geom_smooth(method = "lm", 
              formula = y ~ poly(x, 8), 
              col = "red", se = FALSE) 


#In this example, we demonstrated how you can prevent overfitting and underfitting models by changing the model complexity.
ggplot(cars, aes(x = speed, y = dist)) + 
  geom_point() + 
  geom_smooth(method = "lm", 
              formula = y ~ x, 
              col = "red", 
              se = FALSE) 

#3. Regularization
#Regularization is a way to handle the problem of overfitting.
# There are a few methods of regularizing linear models including
# 
# Ridge (L2) regularization
# Lasso (L1) regularization
# Elastic net (mix of L1 and L2) regularization
#
# install.packages("glmnet")
# library(glmnet)

#Ridge (L2) regularization
flight_recipe <-
  recipe(ArrDelayMinutes ~ ., data = train_data)
ridge_spec <- linear_reg(penalty = 0.1, mixture = 0) %>%
  set_engine("glmnet")
ridge_wf <- workflow() %>%
  add_recipe(flight_recipe)
# add the ridge model and fit the model.
ridge_fit <- ridge_wf %>%
  add_model(ridge_spec) %>%
  fit(data = train_data)
ridge_fit %>%
  pull_workflow_fit() %>%
  tidy()

#Lasso (L1) regularization
lasso_spec<-linear_reg(penalty = 0.1, mixture = 1)%>%
  set_engine("glmnet")
lasso_wf <- workflow()%>%
  add_recipe(flight_recipe)
lasso_fit <- lasso_wf %>%
  add_model(lasso_spec)%>%
  fit(data = train_data)
lasso_fit %>%
  pull_workflow_fit() %>%
  tidy()

#Elastic Net (L1 and L2) Regularization
elasticnet_spec <- linear_reg(penalty = 0.1, mixture = 0.3) %>%
  set_engine("glmnet")

elasticnet_wf <- workflow() %>%
  add_recipe(flight_recipe)

elasticnet_fit <- elasticnet_wf %>%
  add_model(elasticnet_spec) %>%
  fit(data = train_data)
elasticnet_fit %>%
  pull_workflow_fit() %>%
  tidy()


#Perform elastic net regression with "mixture = 0.5" and "penalty = 0.2" using all features (variables) in the training data
elasticnet_spec2 <- linear_reg(penalty = 0.2, mixture = 0.5) %>%
  set_engine("glmnet")

elasticnet_wf2 <- workflow() %>%
  add_recipe(flight_recipe)

elasticnet_fit2 <- elasticnet_wf2 %>%
  add_model(elasticnet_spec2) %>%
  fit(data = train_data)
elasticnet_fit2 %>%
  pull_workflow_fit() %>%
  tidy()


####
flight_recipe <-
  recipe(ArrDelayMinutes ~ ., data = train_data)

el_spec <- linear_reg(penalty = 0.5, mixture = 0.2) %>%
  set_engine("glmnet")

el_wf <- workflow() %>%
  add_recipe(flight_recipe)

el_fit <- el_wf %>%
  add_model(el_spec) %>%
  fit(data = train_data)

el_fit %>%
  pull_workflow_fit() %>%
  tidy()

#####
#Part 4: Grid Search
#First, define the lasso model. In this example, we will be tuning a lasso model so mixture = 1. We will tune lambda, which is penalty in the function.
tune_spec <- linear_reg(penalty = tune(), mixture = 1) %>% 
  set_engine("glmnet")

lasso_wf <- workflow() %>%
  add_recipe(flight_recipe)

#Next, define cross validation to resample the data:
flight_cvfolds <- vfold_cv(train_data)

#Now, you can set up the grid using grid_regular()
lambda_grid <- grid_regular(levels = 50,
                            penalty(range = c(-3, 0.3)))

#To tune the grid, use tune_grid() and include the lambda grid just specified.
lasso_grid <- tune_grid(
  lasso_wf %>% add_model(tune_spec), 
  resamples = flight_cvfolds, 
  grid = lambda_grid)

show_best(lasso_grid, metric = "rmse")

#From the table and using RMSE as the metric, using lambda (penalty) equal to 1.46 gives the best result.


#Additionally, to visualize the RMSE results:
lasso_grid %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  ggplot(aes(penalty, mean)) +
  geom_line(size=1, color="red") +
  scale_x_log10() +
  ggtitle("RMSE")


#Perform a grid search for the lambda (penalty) parameter on ridge regression, then find the best values of the parameter.
tune_spec <- linear_reg(
  penalty = tune(), 
  mixture = 0) %>% 
  set_engine("glmnet")

ridge_grid <- tune_grid(ridge_wf %>% 
                          add_model(tune_spec), 
                        resamples = flight_cvfolds, 
                        grid = lambda_grid)

show_best(ridge_grid, metric = "rmse")

