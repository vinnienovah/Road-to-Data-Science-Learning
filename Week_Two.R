# Load tidyverse
library(tidyverse)
setwd("C:/Users/Administrator/Desktop/Self Learning/Data Analysis with R__Coursera/Week 2")

#we are going to load data into R using the url
# below is the url where the data is located
# Download 2 million dataset

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

head(sub_airline)

#Identify Missing Malues
is.na(c(1, NA))        #> FALSE  TRUE
is.na(paste(c(1, NA))) #> FALSE FALSE

anyNA(c(1, NA))  

#counting missing values
sub_airline %>% 
  summarize(count = sum(is.na(CarrierDelay)))

#using the map() function which acts a a formula hence the use of tilda
sub_airline %>%
  map(~sum(is.na(.)))
# Check dimensions of the dataset
dim(sub_airline)

######
#Handle Missing Data
#Drop the whole column:

drop_na_cols <- sub_airline %>%
  select(-DivDistance, -DivArrDelay)
dim(drop_na_cols)
head(drop_na_cols)

#Drop the whole row:
drop_na_rows <- drop_na_cols %>%
  drop_na(CarrierDelay)
dim(drop_na_rows)

#Convert NA to 0
replace_na_0 <-drop_na_rows %>%
  replace_na(list(CarrierDelay = 0,
                  WeatherDelay = 0,
                  NASDelay = 0,
                  SecurityDelay = 0,
                  LateAircraftDelay = 0))
View(replace_na_0)

#replace NA in "CarrierDelay" column by the mean value.
#carrier_mean <- mean(sub_airline$CarrierDelay)
replace_na_mean <- drop_na_rows %>%
  replace_na(list(CarrierDelay = mean(sub_airline$CarrierDelay)))
View(replace_na_mean)

# Correct Data Format
sapply(sub_airline, class)

sub_airline %>%
  summarise_all(class) %>%
  gather(variable, class)

date_airline <- replace_na_mean %>%
  separate(FlightDate, sep ="-", into = c("Year", "Month", "Day"))
head(date_airline)

#checking the data type for year, month and day
sapply(date_airline, typeof)
date_airline %>%
  summarise_all(class) %>%
  gather(variable, class)
#we need to change the data type for the dates to numeric
#we do the following
date_airline %>%
  select(Year, Month, Day) %>%
  mutate_all(type.convert) %>%
  mutate_if(is.character, as.numeric)

#check if the data type has been converted
date_airline %>%
  summarise_all(class) %>%
  gather(variable, class)

#Data Normalization
# 1. Simple scaling
#Would like to Normalize those variables so their value ranges from 0 to 1.

simple_scale <- date_airline$ArrDelay/max(date_airline$ArrDelay)
head(simple_scale)

#normalize the column "DepDelay" using the simple scaling technique.
simple_scale_Dep <- date_airline$DepDelay/max(date_airline$DepDelay)
head(simple_scale_Dep)

#2. Min-max
#Using "ArrDelay" as an example again, you can transform this column using the min-max technique:
minmax_scale <- (date_airline$ArrDelay-min(date_airline$ArrDelay))/(max(date_airline$ArrDelay)-min(date_airline$ArrDelay))
head(minmax_scale)

#3. Data Standardization (Z-score)
#use "ArrDelay" again. We can use mean() to find the mean of the feature and we can use sd() to find the standard deviation of the feature.
z_score_scale <- (date_airline$ArrDelay - mean(date_airline$ArrDelay))/sd(date_airline$ArrDelay)
head(z_score_scale)


########
#Binning
########
ggplot(data =  sub_airline, mapping = aes(x = ArrDelay))+
  geom_histogram(bins = 100, color = "black", fill = 'red')+
  coord_cartesian(xlim = c(-73,682))
windows()

#First we use the dplyr function ntile to break "ArrDelay" into 4 buckets, which have equal amount of observations of flight arrival delays.
#We then create a list "quantile_rank" that contains 4 bins, which are respectively labeled "1", "2", "3", "4".
binning <- sub_airline %>%
  mutate(quantile_rank = ntile(sub_airline$ArrDelay, 4))
head(binning)

#Now if we look at a histogram of the bins, you can see that all bins are equal
ggplot(data = binning, mapping = aes(x = quantile_rank))+
  geom_histogram(bins = 4, color = 'black', fill = 'green')

####
#Indicator variable
####
#An indicator variable (or dummy variable) is a numerical variable used to label categories
Indicator_variable <-sub_airline %>%
  mutate(dummy = 1) %>% #column with single value
  spread(key = Reporting_Airline, # column to spread
         value = dummy,
         fill = 0) %>%
  slice(1:10)
View(Indicator_variable)

#Alternatively
Alternative <- sub_airline %>%
  spread(Reporting_Airline, ArrDelay) %>%
  slice(1:8)
View(Alternative)

#Let's visualize how many data points in each airline category.
sub_airline %>%
  mutate(Reporting_Airline = factor(Reporting_Airline, 
                                    labels = c('AA','AS','DL', 'UA','B6','PA (1)','HP', 'TW', 'VX'))) %>%
  ggplot(aes(Reporting_Airline))+
  stat_count(width = 0.5) +
  labs(x = 'Number of data points in each airline')
  

###
#create indicator variable to the column of "Month"
Indicator_month <- sub_airline %>%
  mutate(dummy = 1) %>%
  spread(key = Month,
         value = dummy,
         fill = 0) %>%
  slice(1:7)
View(Indicator_month)

#Now, create indicator variable to the column of "Month" by applying departure delay values
Altern <- sub_airline %>%
  spread(Month, DepDelay) %>%
  slice(1:8)
View(Altern)

sub_airline %>% map(~sum(is.na(.)))
