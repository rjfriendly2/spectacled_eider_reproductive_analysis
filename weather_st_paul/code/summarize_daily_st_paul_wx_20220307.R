# /code/02_preprocess_data/wx_st_paul/summarize_daily_st_paul_wx_[version date].R

# data source for St. Paul airport daily weather:
# https://www.ncdc.noaa.gov/cdo-web/search?datasetid=GHCND

# RStudio base dir "spei_abund_cross-seasonal"

# This code summarizes daily weather variables from the St. Paul Island, AK airport as per Petersen & Douglas (2004)
# 1. Data wrangling
# 2. Count number of "Extreme temp" days in Winter-spring: Nov-Apr
# 3. Count number of extreme temp days in winter: Nov-Mar
# 4. Count number of extreme temp days in spring: Apr
# 5. Calculate "Winter temp": mean daily min temp Nov to Mar
# 6. Calculate "Spring temp": mean min daily temp in April
# 7. Count number of "Extreme wind" days
# 8. Calculate "Winter winds": mean average daily wind speed Dec to Mar
# 9. Calculate "Spring winds": mean average wind speed in Apr
# 10. Compile St. Paul weather variables
# 11. Save data frame as csv "wx_st_paul_1993_2019.csv"

# got packages
library(tidyverse)

# load data
wx<-read.csv("weather_st_paul/data/wx_stpaul_19871201_20190730.csv", header=TRUE)
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
ws_wx<-wx %>% filter(!month %in% (5:10))
table (ws_wx$month)
# Subset years relevant to SPEI data: first breeding year is 1994, therefore first winter_year is 1993
ws_wx<-subset(ws_wx, winter_year>1992)
table(ws_wx$winter_year)

# 2. Count number of "Extreme temp" days winter-spring ####
# "Extreme temp" days = days colder than 5th percentile of min daily temp for the time series winter_year 1993-2019 for each winter (Nov-April)
# NB Petersen and Douglas (2004) used the period Dec-April, here I use Nov-April to stay consistent with sea ice variables

# Determine the 5th percentile value for daily min temperature
# Petersen and Douglas state the 5th percentile of data for the period 1950-2002 was -13C, update that percentile value with the longer time series of data
ws_pct5th_tmin<-quantile(ws_wx$TMIN, 0.05, na.rm=T) # outputs 5th percentile TMIN of all winter months 1993-2019 

# Create indicator for days with TMIN below 5th percentile 
ws_wx$extreme_temp<-ifelse(ws_wx$TMIN <= ws_pct5th_tmin & !is.na(ws_wx$TMIN),1,0) # NOTE NA days are removed here

# Count number of extreme low temp days Nov-Apr per winter_year
ws_extreme_temp<-aggregate(ws_wx$extreme_temp, by=list(as.character(ws_wx$winter_year)), 
                        FUN=sum)
# Rename columns
colnames(ws_extreme_temp)<-c("year_winter","ws_days_tmin_below_15")
# Make year_winter numeric
ws_extreme_temp$year_winter<-as.numeric(ws_extreme_temp$year_winter)
# Plot
p.ws_xtemp<-ggplot(ws_extreme_temp)+
  geom_line(aes(year_winter, ws_days_tmin_below_15)) 
p.ws_xtemp

# 3. Count number of extreme low temp days during winter, Nov-March
# Limit months to Nov-March
w_wx<-subset(ws_wx, month!=4)
table(w_wx$month)
# Calc 5th percentile TMIN for winter  months (Nov-Mar)
w_pct5th_tmin<-quantile(w_wx$TMIN, 0.05, na.rm=T) # outputs 5th percentile TMIN of all winter months 1993-2019 
# Create indicator variable for winter extreme low temp days
w_wx$extreme_temp<-ifelse(w_wx$TMIN <= w_pct5th_tmin & !is.na(w_wx$TMIN),1,0) # NOTE NA days are removed here
# Count extreme temp days by year
w_extreme_temp<-aggregate(w_wx$extreme_temp, by=list(as.character(w_wx$winter_year)), 
                        FUN=sum)
# Rename columns
colnames(w_extreme_temp)<-c("year_winter","w_days_tmin_below_15")
# Plot
p.w_xtemp<-ggplot(w_extreme_temp, aes(x=as.numeric(year_winter), y=w_days_tmin_below_15))+
  geom_line() 
p.w_xtemp

# 4. Count number of extreme low temp days April ####
sp_wx<-subset(ws_wx, month==4)
# Calc 5th percentile TMIN for winter  months (Apr)
sp_pct5th_tmin<-quantile(sp_wx$TMIN, 0.05, na.rm=T) # outputs 5th percentile TMIN of all winter months 1993-2019 
# Create indicator variable for winter extreme low temp days
sp_wx$extreme_temp<-ifelse(sp_wx$TMIN <= sp_pct5th_tmin & !is.na(sp_wx$TMIN),1,0) # NOTE NA days are removed here
# Count extreme temp days by year
sp_extreme_temp<-aggregate(sp_wx$extreme_temp, by=list(as.character(sp_wx$winter_year)), 
                          FUN=sum)
# Rename columns
colnames(sp_extreme_temp)<-c("year_winter","sp_days_tmin_below_11")
# Plot
sp.ws_xtemp<-ggplot(sp_extreme_temp)+
  geom_line(aes(as.numeric(year_winter), sp_days_tmin_below_11)) 
sp.ws_xtemp

# 5. Calculate "Winter temp": mean daily min temp Nov to Mar ####
# Note Petersen & Douglas (2004) use the period Dec-Mar, here I use Nov-Mar to stay consistent with sea ice data
# Remove April data
winterT<-subset(ws_wx, month != 4) 
table (winterT$month)

# Calculate mean TMIN for Nov-Mar in each year_winter
winter_temp<-aggregate(winterT$TMIN, by=list(as.character(winterT$winter_year)), FUN=mean, na.rm=TRUE)
colnames(winter_temp)<-c("winter_year", "winter_meanTmin")
winter_temp$winter_year<-as.numeric(winter_temp$winter_year)

# Plot it
p.winterT<-ggplot(winter_temp, aes(winter_year, winter_meanTmin))+
  geom_line()
p.winterT

# 6. Calculate "Spring temp": mean min daily temp in April ####
# Subset April data
springT<-subset(ws_wx, month==4)
table(springT$month)
# Calculate mean daily min temp in April for each winter_year
spring_temp<-aggregate(springT$TMIN, by=list(as.character(springT$winter_year)), FUN=mean, na.rm=TRUE)
colnames(spring_temp)<-c("winter_year", "spring_meanTmin")
spring_temp$winter_year<-as.numeric(spring_temp$winter_year)
# Plot it
p.springT<-ggplot(spring_temp, aes(winter_year, spring_meanTmin))+
  geom_line()
p.springT

# 7. Count number of "Extreme wind" days ####
# Determine the 95th percentile value for daily max wind speed
# Petersen and Douglas (2004) specify the 95% pctl AWND for period 1950-2002 as >= 15.0 m/sec
# Update that value to the period relevant to spec eider nest data: winter years 1993 to present
ws_pct95th_wind<-quantile(ws_wx$AWND, 0.95, na.rm=T) # outputs 5th percentile  

# Create indicator for days with AWND above 95th percentile 
ws_wx$extreme_wind<-ifelse(ws_wx$AWND >= ws_pct95th_wind & !is.na(ws_wx$AWND),1,0) # NOTE NA days are removed here

# Count number of extreme high wind days per winter_year
ws_extreme_wind<-aggregate(ws_wx$extreme_wind, by=list(as.character(ws_wx$winter_year)), 
                        FUN=sum)
# Rename columns
colnames(ws_extreme_wind)<-c("year_winter","ws_days_awnd_above_13_6")
# Make year_winter numeric
ws_extreme_wind$year_winter<-as.numeric(ws_extreme_temp$year_winter)
# Plot it
p.ws_xwind<-ggplot(ws_extreme_wind)+
  geom_line(aes(year_winter, ws_days_awnd_above_13_6)) 
p.ws_xwind


# 8. Count number of "Extreme wind" days winter, Nov-Mar ####
w_pct95th_wind<-quantile(w_wx$AWND, 0.95, na.rm=T) # outputs 5th percentile  

# Create indicator for days with AWND above 95th percentile 
w_wx$extreme_wind<-ifelse(w_wx$AWND >= w_pct95th_wind & !is.na(w_wx$AWND),1,0) # NOTE NA days are removed here

# Count number of extreme high wind days per winter_year
w_extreme_wind<-aggregate(w_wx$extreme_wind, by=list(as.character(w_wx$winter_year)), 
                           FUN=sum)
# Rename columns
colnames(w_extreme_wind)<-c("year_winter","w_days_awnd_above_13_8")
# Make year_winter numeric
w_extreme_wind$year_winter<-as.numeric(w_extreme_wind$year_winter)
# Plot it
p.w_xwind<-ggplot(w_extreme_wind)+
  geom_line(aes(year_winter, w_days_awnd_above_13_8)) 
p.w_xwind

# 9. Count number of "Extreme wind" days spring, April ####
sp_pct95th_wind<-quantile(sp_wx$AWND, 0.95, na.rm=T) # outputs 5th percentile  

# Create indicator for days with AWND above 95th percentile 
sp_wx$extreme_wind<-ifelse(sp_wx$AWND >= sp_pct95th_wind & !is.na(sp_wx$AWND),1,0) # NOTE NA days are removed here

# Count number of extreme high wind days per winter_year
sp_extreme_wind<-aggregate(sp_wx$extreme_wind, by=list(as.character(sp_wx$winter_year)), 
                          FUN=sum)
# Rename columns
colnames(sp_extreme_wind)<-c("year_winter","sp_days_awnd_above_12")
# Make year_winter numeric
sp_extreme_wind$year_winter<-as.numeric(sp_extreme_wind$year_winter)
# Plot it
p.sp_xwind<-ggplot(sp_extreme_wind)+
  geom_line(aes(year_winter, sp_days_awnd_above_12)) 
p.sp_xwind

# 10. Calculate "Winter winds": mean average daily wind speed Dec to Mar ####
winter_wind<-aggregate(winterT$AWND, by=list(as.character(winterT$winter_year)), 
                       FUN=mean, na.rm=TRUE)
colnames(winter_wind)<-c("winter_year", "winter_meanWind")
winter_wind$winter_year<-as.numeric(winter_wind$winter_year)
p.winterwind<-ggplot(winter_wind, aes(winter_year, winter_meanWind))+
  geom_line()
p.winterwind

# 11. Calculate "Spring winds": mean average wind speed in Apr ####
spring_wind<-aggregate(springT$AWND, by=list(as.character(springT$winter_year)), FUN=mean, na.rm=TRUE)
colnames(spring_wind)<-c("winter_year", "spring_meanWind")
spring_wind$winter_year<-as.numeric(spring_wind$winter_year)

# Plot it
p.springwind<-ggplot(spring_wind, aes(winter_year, spring_meanWind))+
  geom_line()
p.springwind

# 12. Compile St. Paul weather variables
wx_st_paul_1993_2019<-cbind(ws_extreme_temp,
                            w_extreme_temp[2],
                            sp_extreme_temp[2],
                            winter_temp[2],
                            spring_temp[2],
                            ws_extreme_wind[2],
                            w_extreme_wind[2],
                            sp_extreme_wind[2],
                            winter_wind[2],
                            spring_wind[2])


# 13. Save data frame as csv
write.csv(wx_st_paul_1993_2019, "weather_st_paul/output/wx_st_paul_1993_2019.csv")
