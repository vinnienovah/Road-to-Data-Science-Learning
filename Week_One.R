###################
#The first datasets::
####################

#  install tidyverse if running locally
# %>% install.packages("tidyverse")

# Load tidyverse
library(tidyverse)

#we are going to use the loaidng of data into R using the url
# below is the url where the data is loacted
url <- "https://dax-cdn.cdn.appdomain.cloud/dax-airline/1.0.1/lax_to_jfk.tar.gz"

#we can now download the file
download.file(url,destfile = "lax_to_jfk.tar.gz")

#we can now untar/unzip the file so that we can get the csv only
untar("lax_to_jfk.tar.gz", tar = "internal")

#read_csv only 
sub_airline <- read_csv("lax_to_jfk/lax_to_jfk.csv", 
                        col_types = cols(
                          'DivDistance' = col_number(),
                          'DivArrDelay' = col_number()))
#Viewing the dataset
View(sub_airline)

#show the first n = 6 rows
head(sub_airline)

#show the first n = 3 rows
head(sub_airline, 3)

#show the last n = 6 rows
tail(sub_airline)

#show the last n = 3 rows
tail(sub_airline, 3)

#show the last n = 10 rows
tail(sub_airline, 10)

glimpse(sub_airline)

dim(sub_airline)

########
#Data preprocessing
#######

################
#Dataset two
################

# Download 2 million dataset

# url where the data is located
url <- "https://dax-cdn.cdn.appdomain.cloud/dax-airline/1.0.1/airline_2m.tar.gz"

# download the file
download.file(url, destfile = "airline_2m.tar.gz")

# untar the file so we can get the csv only
untar("airline_2m.tar.gz")

# read_csv only 
airlines <- read_csv("airline_2m.csv", 
                     col_types = cols(
                       'DivDistance' = col_number(),
                       'Div1Airport' = col_character(),
                       'Div1AirportID' = col_character(),
                       'Div1AirportSeqID' = col_character(),
                       'Div1WheelsOn' = col_character(),
                       'Div1TotalGTime' = col_number(),
                       'Div1LongestGTime' = col_number(),
                       'DivReachedDest' = col_number(),
                       'DivActualElapsedTime' = col_number(),
                       'DivArrDelay' = col_number(),
                       'Div1WheelsOff' = col_character(),
                       'Div1TailNum' = col_character(),
                       'Div2Airport' = col_character(),
                       'Div2AirportID' = col_character(),
                       'Div2AirportSeqID' = col_character(),
                       'Div2WheelsOn'= col_character(),
                       'Div2TotalGtime' = col_number(),
                       'Div2LongestGTime' = col_number(),
                       'Div2WheelsOff' = col_character(),
                       'Div2TailNum' = col_character()
                     ))

# We are going to be focusing on flights from  LAX to JFK and we will exclude
# flights that got cancelled or diverted
# we are also going to get only useful columns
sub_airline2 <- airlines %>%
  filter(Origin == "LAX", Dest == "JFK", 
         Cancelled != 1) %>%
  select(Month, DayOfWeek, FlightDate,
         Reporting_Airline, Origin, Dest,
         CRSDepTime, CRSArrTime, DepTime,
         ArrTime, ArrDelay, ArrDelayMinutes,
         CarrierDelay, WeatherDelay, NASDelay,
         SecurityDelay, LateAircraftDelay, DepDelay,
         DepDelayMinutes, DivDistance, DivArrDelay)
View(sub_airline2)
glimpse(sub_airline2)
# Check dimensions of the dataset
dim(sub_airline2)

#Find the name of the columns of the dataframe
colnames(sub_airline2)

#save the dataframe sub_airline as lax_to_jfk.csv
write_csv(sub_airline2, "lax_to_jfk.csv")

#########
#Basic Insights of the Dataset
#########
#checking the data types of sub_airline2
sapply(sub_airline2, typeof)

sapply(sub_airline2, class)
#Numeric and Integer Types
x = 10.5       # assign a decimal value 
class(x)       # print the class name of x, which should be numeric

#here by chekcing type we should get a double
typeof(x)

y = as.integer(3)        # assign a integer value 
class(y) 
#will print integer becasue we changed its default assignment to an integr not numeric
typeof(y)

#Complex Type
z = 0i
class(z)

#let's try typeof() function
typeof(z)

#Logical Type
logical_values = c(TRUE, T, FALSE, F)
class(logical_values)

#let's try typeof() function
typeof(logical_values)


#Character Type
k = 'this is a character'
class(k)
typeof(k)



#############
#Dplyr for Data Wrangling and Transformation
#############

#Pipe
#The pipe operator allows us to chain together dplyr data wrangling functions.
#lets start with this code,
summarize(group_by(filter(sub_airline2, Month == 1), 
                   Reporting_Airline), avg_carrier_delay = 
                    mean(CarrierDelay, na.rm = TRUE))


#The same code can be written using pipe and makes it much easier to understand:
summary_Carr <- sub_airline2 %>%
  filter(Month == 1) %>%
  group_by(Reporting_Airline) %>%
  summarize(avg_carrier_delay = mean(CarrierDelay, na.rm = TRUE))


View(summary_Carr)

#lets continue with one more example

summary_Carr2 <- sub_airline2 %>%
  group_by(Reporting_Airline) %>%
  summarise(Avg_Carrier_Delay = mean(CarrierDelay, na.rm = TRUE))
View(summary_Carr2)

summary_Carr3 <- sub_airline2 %>%
  group_by(Reporting_Airline) %>%
  summarise(Avg_Carrier_Delay = sd(CarrierDelay, na.rm = TRUE))
View(summary_Carr3)

#Question to answer
summ_ArrDelay <- sub_airline2 %>%
  group_by(Reporting_Airline) %>%
  summarise(Avg_Arr_Delay = mean(ArrDelay, na.rm = TRUE))

View(summ_ArrDelay)




#######################################################
##trying with the in_built data
starwars %>% 
  filter(species == "Droid")

starwars %>% 
  select(name, ends_with("color"))

starwars %>% 
  mutate(name, bmi = mass / ((height / 100)  ^ 2)) %>%
  select(name, height, mass, bmi)

starwars %>% 
  mutate(name, bmi = mass / ((height / 100)  ^ 2)) %>%
  select(name:mass, bmi)


starwars %>%
  group_by(species) %>%
  summarise(
    n = n(),
    mass = mean(mass, na.rm = TRUE)
  ) %>%
  filter(
    n > 1,
    mass > 50
  )
#########################################################


