# /code/02_preprocess_data/wx_st_paul/summarize_daily_st_paul_wx_[version date].R

# data source for St. Paul airport daily weather:
# https://www.ncdc.noaa.gov/cdo-web/search?datasetid=GHCND

# RStudio base dir "spei_abund_cross-seasonal"

# This code summarizes daily weather variables from the St. Paul Island, AK airport as per Petersen & Douglas (2004)
# 1. Data wrangling
# 2. Count number of "Extreme temp" days
# 3. Calculate "Winter temp": mean daily min temp Nov to Mar
# 4. Calculate "Spring temp": mean min daily temp in April
# 5. Count number of "Extreme wind" days
# 6. Calculate "Winter winds": mean average daily wind speed Dec to Mar
# 7. Calculate "Spring winds": mean average wind speed in Apr
# 8. Compile St. Paul weather variables
# 9. Save dataframe as csv "wx_st_paul_1993_2019.csv"

# got packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)

# load data ## Make sure to set working directory to the right place
wx<-read.csv("weather_st_paul/data/wx_stpaul_01121987_03012022.csv", header=TRUE)
summary(wx)

# 1. Data wrangling ####
# format date
wx$DATE<-as.POSIXct(wx$DATE, format="%Y-%m-%d")
# numeric calendar year
wx$calYR<-as.numeric(format(as.Date(wx$DATE, format="%Y-%m-%d"), "%Y"))
# numeric month
wx$month<-as.numeric(format(as.Date(wx$DATE, format="%Y-%m-%d"), "%m")) 
# create winter_year variable: year of Nov in period Nov-Apr
wx$winter_year<-ifelse(wx$month>10, wx$calYR, wx$calYR-1) 
# reduce data set to only winter-spring months (Nov through April)
winter_wx<-wx %>% filter(!month %in% (5:10))
table (winter_wx$month)
# Subset years relavent to SPEI data: first breeding year is 1994, therefore first winter_year is 1993
winter_wx<-subset(winter_wx, winter_year>1992)
table(winter_wx$winter_year)

# 2. Count number of "Extreme temp" days ####
# "Extreme temp" days = days colder than 5th percentile of min daily temp for the time series winter_year 1993-2019 for each winter (Nov-April)
# NB Petersen and Douglas (2004) used the period Dec-April, here I use Nov-April to stay concistent with sea ice variables

# Determine the 5th percentile value for daily min temperature
# Petersen and Douglas state the 5th percentile of data for the period 1950-2002 was -13C, update that percentile value with the longer time series of data
pct5th_tmin<-quantile(winter_wx$TMIN, 0.05, na.rm=T) # outputs 5th percentile TMIN 

# Create indicator for days with TMIN below 5th percentile 
winter_wx$extreme_temp<-ifelse(winter_wx$TMIN <= pct5th_tmin & !is.na(winter_wx$TMIN),1,0) # NOTE NA days are removed here

# Count number of extreme low temp days per winter_year
extreme_temp<-aggregate(winter_wx$extreme_temp, by=list(as.character(winter_wx$winter_year)), 
                        FUN=sum)
# Rename columns
colnames(extreme_temp)<-c("year_winter","days_tmin_below_15")
# Make year_winter numberic
extreme_temp$year_winter<-as.numeric(extreme_temp$year_winter)
# Plot
p.xtemp<-ggplot(extreme_temp)+
  geom_line(aes(year_winter, days_tmin_below_15)) 
p.xtemp

# 3. Calculate "Winter temp": mean daily min temp Nov to Mar ####
# Note Petersen & Douglas (2004) use the period Dec-Mar, here I use Nov-Mar to stay consitent with sea ice data
# Remove April data
winterT<-subset(winter_wx, month != 4) 
table (winterT$month)

# Calculate mean TMIN for Nov-Mar in each year_winter
winter_temp<-aggregate(winterT$TMIN, by=list(as.character(winterT$winter_year)), FUN=mean, na.rm=TRUE)
colnames(winter_temp)<-c("winter_year", "winter_meanTmin")
winter_temp$winter_year<-as.numeric(winter_temp$winter_year)

# Plot it
p.winterT<-ggplot(winter_temp, aes(winter_year, winter_meanTmin))+
  geom_line()
p.winterT

# 4. Calculate "Spring temp": mean min daily temp in April ####
# Subset April data
springT<-subset(winter_wx, month==4)
table(springT$month)
# Calculate mean daily min temp in April for each winter_year
spring_temp<-aggregate(springT$TMIN, by=list(as.character(springT$winter_year)), FUN=mean, na.rm=TRUE)
colnames(spring_temp)<-c("winter_year", "spring_meanTmin")
spring_temp$winter_year<-as.numeric(spring_temp$winter_year)
# Plot it
p.springT<-ggplot(spring_temp, aes(winter_year, spring_meanTmin))+
  geom_line()
p.springT

# 5. Count number of "Extreme wind" days ####
# Determine the 95th percentile value for daily max wind speed
# Petersen and Douglas (2004) specify the 95% pctl AWND for period 1950-2002 as >= 15.0 m/sec
# Update that value to the period relevant to spec eider nest data: winter years 1993 to present
pct95th_wind<-quantile(winter_wx$AWND, 0.95, na.rm=T) # outputs 5th percentile TMIN 

# Create indicator for days with TMIN below 5th percentile 
winter_wx$extreme_wind<-ifelse(winter_wx$AWND >= pct95th_wind & !is.na(winter_wx$AWND),1,0) # NOTE NA days are removed here

# Count number of extreme low temp days per winter_year
extreme_wind<-aggregate(winter_wx$extreme_wind, by=list(as.character(winter_wx$winter_year)), 
                        FUN=sum)
# Rename columns
colnames(extreme_wind)<-c("year_winter","days_awnd_above_13_6")
# Make year_winter numberic
extreme_wind$year_winter<-as.numeric(extreme_temp$year_winter)
# Plot it
p.xwind<-ggplot(extreme_wind)+
  geom_line(aes(year_winter, days_awnd_above_13_6)) 
p.xwind

# 6. Calculate "Winter winds": mean average daily wind speed Dec to Mar ####
winter_wind<-aggregate(winterT$AWND, by=list(as.character(winterT$winter_year)), 
                       FUN=mean, na.rm=TRUE)
colnames(winter_wind)<-c("winter_year", "winter_meanWind")
winter_wind$winter_year<-as.numeric(winter_wind$winter_year)
p.winterwind<-ggplot(winter_wind, aes(winter_year, winter_meanWind))+
  geom_line()
p.winterwind

# 7. Calculate "Spring winds": mean average wind speed in Apr ####
spring_wind<-aggregate(springT$AWND, by=list(as.character(springT$winter_year)), FUN=mean, na.rm=TRUE)
colnames(spring_wind)<-c("winter_year", "spring_meanWind")
spring_wind$winter_year<-as.numeric(spring_wind$winter_year)

# Plot it
p.springwind<-ggplot(spring_wind, aes(winter_year, spring_meanWind))+
  geom_line()
p.springwind

# 8. Compile St. Paul weather variables
wx_st_paul_1993_2019<-cbind(extreme_temp,
                            winter_temp[2],
                            spring_temp[2],
                            extreme_wind[2],
                            winter_wind[2],
                            spring_wind[2])


# 9. Save dataframe as csv
write.csv(wx_st_paul_1993_2019, "weather_st_paul/output/wx_st_paul_1993_2019.csv")
