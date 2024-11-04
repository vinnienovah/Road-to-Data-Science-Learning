# Load tidyverse
library(tidyverse)

# url where the data is located
url <- "https://dax-cdn.cdn.appdomain.cloud/dax-airline/1.0.1/lax_to_jfk.tar.gz"

# download the file
download.file(url, destfile = "lax_to_jfk.tar.gz")

# untar the file so we can get the csv only
# if you run this on your local machine, then can remove tar = "internal" 
untar("lax_to_jfk.tar.gz")

# read_csv only 
sub_airline <- read_csv("lax_to_jfk/lax_to_jfk.csv",
                        col_types = cols('DivDistance' = col_number(), 
                                         'DivArrDelay' = col_number()))
View(sub_airline)

#Analyzing Individual Feature Patterns using Visualization
ggplot(data = sub_airline, mapping =  aes(x = Reporting_Airline, y = ArrDelay))+
  geom_boxplot(fill = "bisque", color = 'black', alpha =0.3) +
  geom_jitter(aes(color = 'blue'), alpha = 0.2)+
  labs(x = "Airline")+
  ggtitle("Arrival Delays by Airline")+
  guides(color = FALSE) +
  theme_minimal()+
  coord_cartesian(ylim = quantile(sub_airline$ArrDelay, c(0,0.99)))
windows()


# Load Alaska data, deleting rows that have missing departure delay or arrival delay data
alaska_flights <- sub_airline %>%
  filter(Reporting_Airline == "AS") %>%
  filter(!is.na(DepDelay) & !is.na(ArrDelay)) %>%
  filter(DepDelay < 40)

ggplot(data = alaska_flights, mapping = aes(x = DepDelay, y = ArrDelay)) +
  geom_point() +
  ggtitle("Alaska Flight Depature Delays vs Arrival Delays")

# list the data types for each column
str(sub_airline)

#What is the data type of the column "ArrDelayMinutes"?
class(sub_airline$ArrDelayMinutes)

#Find the correlation between the following columns: DepDelayMinutes and ArrDelayMinutes.
cor(sub_airline$DepDelayMinutes, sub_airline$ArrDelayMinutes)

##OR

sub_airline %>%
  select(DepDelayMinutes, ArrDelayMinutes) %>%
  cor(method = "pearson")
sub_airline %>%
  cor.test(~DepDelayMinutes + ArrDelayMinutes, data = .)

#Continuous numerical variables
#Positive linear relationship
# DepDelayMinutes as potential predictor variable of ArrDelayMinutes
ggplot(data = sub_airline, mapping = aes(x = DepDelayMinutes, y = ArrDelayMinutes)) +
  geom_point() + 
  geom_smooth(method = "lm", na.rm = TRUE)

#Next, we can examine the correlation between "DepDelayMinutes" and "ArrDelayMinutes"
cor(sub_airline$DepDelayMinutes, sub_airline$ArrDelayMinutes)

#Weak Linear Relationship
#Let's now look at if "WeatherDelay" is a good predictor variable of "ArrDelayMinutes"
ggplot(data = sub_airline, mapping = aes(x = WeatherDelay, y = ArrDelayMinutes)) +
  geom_point() + 
  geom_smooth(method = "lm", na.rm = TRUE)

cor(sub_airline$WeatherDelay, sub_airline$ArrDelayMinutes, use = "complete.obs")
#see that it's a very weak relationship since the value is close to 0.

#Find the correlation between x="CarrierDelay", y="ArrDelayMinutes".
cor(sub_airline$CarrierDelay, sub_airline$ArrDelayMinutes, use ='complete.obs')

#oR
sub_airline %>%
  select(CarrierDelay, ArrDelayMinutes) %>%
  cor(method = "pearson", use = 'complete.obs')

#Given the correlation results between x="CarrierDelay", y="ArrDelayMinutes", do you expect a linear relationship?

#Verify your results using the function of ggplot.

ggplot(data = sub_airline, mapping = aes(x = CarrierDelay, y = ArrDelayMinutes)) +
  geom_point() +
  geom_smooth(method = 'lm', na.rm = TRUE)

# Descriptive Statistical Analysis
summary_airline_delays <- sub_airline %>%
  group_by(Reporting_Airline) %>%
  summarize(count = n(), 
            mean = mean(ArrDelayMinutes, na.rm = TRUE),
            std_dev = sd(ArrDelayMinutes, na.rm = TRUE), 
            min = min(ArrDelayMinutes, na.rm = TRUE), 
            median = median(ArrDelayMinutes, na.rm=TRUE),
            iqr = IQR(ArrDelayMinutes, na.rm = TRUE), 
            max = max(ArrDelayMinutes, na.rm = TRUE))

summary_airline_delays

#checking data types
sapply(summary_airline_delays, typeof)

#Value Counts
sub_airline %>%
  count(Reporting_Airline)

#Basics of Grouping
avg_delays <- sub_airline %>%
  group_by(Reporting_Airline, DayOfWeek) %>%
  summarize(mean_delays = mean(ArrDelayMinutes), .groups = 'keep')
#Setting .groups = 'keep' keeps the grouping structure, if you instead were to set it to "drop" then the output would be ungrouped and essentially be a regualr tibble (dataframe).

head(avg_delays)

# sort the dataframe in R using multiple variables with Dplyr
sorted <- avg_delays %>% 
  arrange(desc(mean_delays))

head(sorted)


# The color is still hard to see and identify,  let's change the color
avg_delays %>% 
  ggplot(aes(x = Reporting_Airline, 
             y = DayOfWeek, 
             fill = mean_delays)) +
  # set the tile's borders to be white with size 0.2
  geom_tile(color = "white", size = 0.2) +
  # define gradient color scales
  scale_fill_gradient(low = "yellow", high = "red")


# This visualization will use lubridate package
library(lubridate)
# Let's take a simple average across Reporting_Airline and DayOfWeek
avg_delays <- sub_airline %>%
  group_by(Reporting_Airline, DayOfWeek) %>%
  summarize(mean_delays = mean(ArrDelayMinutes), .groups = 'keep') %>%
  # create a new variable "bins" from mean_delays
  # make the first range -0.1 to 0.1 to include zero values
  mutate(bins = cut(mean_delays,breaks = c(-0.1,0.1,10,20,30,50, max(mean_delays)),
                    labels = c("0","0-10","10-20","20-30","30-50",">50"))) %>%
  mutate(bins = factor(as.character(bins),levels = rev(levels(bins))))


ggplot(avg_delays, aes(x = Reporting_Airline, 
                       y = lubridate::wday(DayOfWeek, label = TRUE), 
                       fill = bins)) +
  geom_tile(colour = "white", size = 0.2) +
  geom_text(aes(label = round(mean_delays, 3))) +
  guides(fill = guide_legend(title = "Delays Time Scale"))+
  labs(x = "Reporting Airline",y = "Day of Week",title = "Average Arrival Delays")+
  # Define color palette for the scale
  scale_fill_manual(values = c("#d53e4f", "#f46d43", "#fdae61", "#fee08b", "#e6f598", "#abdda4"))

aveg<-sub_airline %>%
  group_by(Reporting_Airline)%>%
  summarise(mean_delays = mean(ArrDelayMinutes))

sorttted<-aveg %>%
  arrange(desc(mean_delays))
head(sorttted)

#correlations
sub_airline %>%
  select(DepDelayMinutes, ArrDelayMinutes)%>%
  cor(method = 'pearson')

#significance using p-value
sub_airline %>%
  cor.test(~DepDelayMinutes + ArrDelayMinutes, data = .)
  
#Correlations between multiple variables
correlation <- sub_airline %>%
  select(ArrDelayMinutes, DepDelayMinutes, 
         CarrierDelay, WeatherDelay, NASDelay, SecurityDelay, LateAircraftDelay) %>% 
  cor(use = "pairwise.complete.obs", method = "pearson")

correlation


# Download the corrplot library first if you have not already.
#install.packages("corrplot")

library(corrplot)


numerics_airline <- sub_airline %>%
  select(ArrDelayMinutes, DepDelayMinutes, CarrierDelay,
         WeatherDelay, NASDelay, SecurityDelay, LateAircraftDelay)

airlines_cor <- cor(numerics_airline, method = "pearson", use='pairwise.complete.obs')

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

window()
corrplot(airlines_cor, method = "color", col = col(200),  
         type = "upper", order = "hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 45, #Text label color and rotation
)


### ANOVA (Analysis of Variance)
summary_airline_delays <- sub_airline %>%
  group_by(Reporting_Airline) %>%
  summarize(Average_Delays = mean(ArrDelayMinutes, na.rm = TRUE))

summary_airline_delays %>%  
  ggplot(aes(x = Reporting_Airline, y = Average_Delays)) + 
  geom_bar(stat = "identity") +
  ggtitle("Average Arrival Delays by Airline")


#compare American Airline and Alaska Airline
aa_as_subset <- sub_airline %>%
  select(ArrDelay, Reporting_Airline) %>%
  filter(Reporting_Airline == 'AA' | Reporting_Airline == 'AS')
head(aa_as_subset)
ad_aov <- aov(ArrDelay ~ Reporting_Airline, data = aa_as_subset)
summary(ad_aov)

#American Airline (AA) and Pan Am Airline (PA (1))
aa_pa_subset <- sub_airline %>%
  select(ArrDelay, Reporting_Airline) %>%
  filter(Reporting_Airline == 'AA' | Reporting_Airline == 'PA (1)')

ad_aov <- aov(ArrDelay ~ Reporting_Airline, data = aa_pa_subset)
summary(ad_aov)
