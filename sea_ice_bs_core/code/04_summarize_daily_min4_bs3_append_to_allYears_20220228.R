# 04_summarize_daily_min_bs3_sic_[version date]

# code by Dan Rizzolo
# last edit 28 Feb 2022

# RStudio root dir: sea_ice_bs_core/

# Code to create and complile Bering Sea sea ice concentration (sic) summary variables for the spec eider "core" area (Petersen and Douglas 2004)

# Required input dataset: 1979_2020_winterSpecEdier_SICbootstrapV31_16pixels.csv"
# Input data created by: "02_extract_specEider_winterice_bootstrap_sic_geotif.R"

# This code (as indexed in RStudio):
# 1. Change sic from permil to percent 
# 2. Select 4 core pixels of the 16 pixels in the original dataset
# 3. Aggregate the minimum pixel sic value across the 4 core pixels for each day
# 4. Some data wrangling 
# 5. Count high (>= 95%) sic days for WINTER+SPRING (01 Nov - 30 Apr): ws_hi 
# 6. Count high sic days for WINTER (01 Nov - 31 March): winter_hi
# 7. Count high sic days for SPRING (01-30 Apr): spring_hi
# 8. Count low (<15%) sic days for WINTER+SPRING: ws_lo
# 9. Count low sic days for WINTER: winter_lo
# 10. Count low sic days for SPRING: spring_lo
# 11. Calc heavy sic index (Flint et a. 2016, Christie et al. 2018) for WINTER+SPRING: ws_index
# 12. Calc heavy sic index for WINTER: winter_index
# 13. Calc heavy sic index for spring: spring_index
# 14. Compile variables into dataframe and save
# 15. Save compiled sic variables as csv ####

# got packages
library(anytime) # wrestle date as string into hyphen seperated date
library(ggplot2) # plot data

# Input data ####
# Daily sea ice concentration in 16 pixels, pixels 1 to 4 are SPEI "core" winter area (see Christie et al. 2018 supplementary data)
# File created with "02_extract_specEider_winterice_bootstrap_sic_geotif.R"
all.ice<-read.csv("output/1979_2020_winterSpecEdier_SICbootstrapV31_16pixels.csv", header=T)
str(all.ice)
head(all.ice)

# 1. Change sic from permil to percent ####
all.ice$bs_sic<-all.ice$bs_sic_x10/10

# 2. Select 4 core pixels ####
# subset to the pixels in the core area (pixels 1 to 4, as per "specEider_winter_pixel_sample_map.png", see supplementary data from Christie et al. (2018)
core_data<-subset(all.ice, pixel < 5)
table(core_data$pixel)

# 3. Aggregate the minimum pixel sic value across the 4 core pixels for each day #####
daily_min_sic_core<-aggregate(core_data$bs_sic~as.factor(core_data$sicDate), FUN=min)
colnames(daily_min_sic_core)<-c("date", "min4_bs3")
head(daily_min_sic_core)

# 4. Some data wrangling ####

# format date from string to %Y-%m-%d with anytime package
daily_min_sic_core$date<-anytime::anydate(daily_min_sic_core$date)
summary(daily_min_sic_core)

# remove May data 
daily_min_sic_core$month<-as.numeric(format(daily_min_sic_core$date, "%m"))
table(daily_min_sic_core$month)

# create winter-year variable for year at start of winter season
daily_min_sic_core$year_cal<-as.numeric(format(daily_min_sic_core$date, "%Y")) # calendar year
daily_min_sic_core$year_winter<-ifelse(daily_min_sic_core$month > 10,daily_min_sic_core$year,daily_min_sic_core$year-1) # winter year
table (daily_min_sic_core$year_winter)

# remove years with incomplete winter data: 1978 (start year, incomplete; data were every-other-day 1978-part way through 1987), 2020 (data for 2021 - winter year 2020 - not yet available as of 2022 feb 23)
daily_min_sic_core<-subset(daily_min_sic_core, year_winter != 1978 & year_winter != 2020)
table (daily_min_sic_core$year_winter)

# create indicator variable for days with min sic >= 95%

# summarize count of days >= 95% by year 
# analyses by Flint et al. (2016) and Petersen and Douglas (2004) used >=95%
# Christie et al. (2018) used > 95%
# this code uses >= 95%

daily_min_sic_core$hi_ice<-ifelse(daily_min_sic_core$min4_bs3 >= 95,1,0)

# create indicator variable for days with min sic < 15% (Christie et al. 2018; Fig. 4)

daily_min_sic_core$lo_ice<-ifelse(daily_min_sic_core$min4_bs3 < 15,1,0)

# 5. Count high (>= 95%) sic days for WINTER+SPRING (01 Nov - 30 Apr): ws_hi ####

# Seasons as defined previously: Petersen & Douglas winter=Dec-Mar, spring=April; Flint et al. 2016 & Christie et al. 2018: winter=01 Nov-30 Apr
# Here, I use these timeframes of interest:
# Winter-Spring: 01 Nov - 30 Apr (although typically no ice in Nov, except 1983, 1987, 1999)
# Winter: 01 Nov-30 Mar
# Spring: 01-30 April

# subset data for all of Winter-Spring (ws: 01 Nov-30 Apr)
table(daily_min_sic_core$month)
# remove data from May
ws_sic_core<-subset(daily_min_sic_core, month != 5)
summary(ws_sic_core)
table(ws_sic_core$month)

# count total days with >= 95% ice cover for each year_winter
ws_hi<-aggregate(ws_sic_core$hi_ice~ws_sic_core$year_winter, FUN=sum)
colnames(ws_hi)<-c("year_winter", "ws_count_sic_ge95")
hist(ws_hi$ws_count_sic_ge95)

p.ws_hi<-ggplot(ws_hi, aes(x=year_winter, y=ws_count_sic_ge95))+
  geom_line()
p.ws_hi

# 6. Count high sic days for WINTER (01 Nov - 31 March): winter_hi ####

# subset data winter (winter: 01 Nov-31 March)
summary(daily_min_sic_core)
# remove data from April & May
winter_sic_core<-subset(daily_min_sic_core, month != 4 & month != 5 )
table(winter_sic_core$month)

# count total days with >= 95% ice cover for each year_winter
winter_hi<-aggregate(winter_sic_core$hi_ice~winter_sic_core$year_winter, FUN=sum)
colnames(winter_hi)<-c("year_winter", "winter_count_sic_ge95")
hist(winter_hi$winter_count_sic_ge95)

p.winter_hi<-ggplot(winter_hi, aes(x=year_winter, y=winter_count_sic_ge95))+
  geom_line()
p.winter_hi

# 7. Count high sic days for SPRING (01-30 Apr): spring_hi ####

# subset data spring
summary(daily_min_sic_core)
# remove data other than Apr
spring_sic_core<-subset(daily_min_sic_core, month == 4 )
table(spring_sic_core$month)

# count total days with >= 95% ice cover for each year_winter
spring_hi<-aggregate(spring_sic_core$hi_ice~spring_sic_core$year_winter, FUN=sum)
colnames(spring_hi)<-c("year_winter", "spring_count_sic_ge95")
hist(spring_hi$spring_count_sic_ge95)
summary(spring_hi)

p.spring_hi<-ggplot(spring_hi, aes(x=year_winter, y=spring_count_sic_ge95))+
  geom_line()
p.spring_hi

# 8. Count low (<15%) sic days for WINTER+SPRING: ws_lo ####

# count total WINTER-SPRING days with < 15% ice cover for each year_winter
ws_lo<-aggregate(ws_sic_core$lo_ice~ws_sic_core$year_winter, FUN=sum)
colnames(ws_lo)<-c("year_winter", "ws_count_sic_le15")
lost(ws_lo$ws_count_sic_ge95)

p.ws_lo<-ggplot(ws_lo, aes(x=year_winter, y=ws_count_sic_le15))+
  geom_line()
p.ws_lo

# 9. Count low sic days for WINTER: winter_lo ####

# count total WINTER days with < 15% ice cover for each year_winter
winter_lo<-aggregate(winter_sic_core$lo_ice~winter_sic_core$year_winter, FUN=sum)
colnames(winter_lo)<-c("year_winter", "winter_count_sic_le15")
hist(winter_lo$winter_count_sic_le15)

p.winter_lo<-ggplot(winter_lo, aes(x=year_winter, y=winter_count_sic_le15))+
  geom_line()
p.winter_lo

# 10. Count low sic days for SPRING: spring_lo ####

# count total SPRING days with < 15% ice cover for each year_winter
spring_lo<-aggregate(spring_sic_core$lo_ice~spring_sic_core$year_winter, FUN=sum)
colnames(spring_lo)<-c("year_winter", "spring_count_sic_le15")
hist(spring_lo$spring_count_sic_le15)
summary(spring_lo)

p.spring_lo<-ggplot(spring_lo, aes(x=year_winter, y=spring_count_sic_le15))+
  geom_line()
p.spring_lo

# 11. Calc heavy sic index (Flint et a. 2016, Christie et al. 2018) for WINTER+SPRING: ws_index ####
# heavy sic index: runs of days >= 95% concentration, runs separated by 2 or more days >= 95% or lower
# apply sic index calcs by year: 

year<-seq(1979,2019,1) # create dataframe with sequence of years to feed into the function, "year" here is winter year (year in Nov)
year.file<-data.frame(year) # convert to dataframe

# function to calcuate high ice index for each year of data
# NB CHANGE function to include input dataset as variable so the function is repeated for each season
heavy.ice<-function(YR){
  
  # subset year during which to summarize heavy ice
  year.one<-subset(ws_sic_core, year_winter==YR)
  #print(YR)
  subice<-year.one$min4_bs3 # sensor data on ice concentration
  #table(ice$month) #ice data are from Nov, Dec, Jan, Feb, Mar, and Apr
  
  # indicator for days >= 95%
  hiIce<-ifelse(subice>=95,1,0)
  
  # rle function to summarize runs in ice indicator variable
  temp.data<-rle(hiIce) # NB rle function output is 2 lists: "values" with value (in this case, 1 or 0) in the run, and "length" with length of the run
  
  # convert zeros with a 1 on each side to 1 to include one-day breaks in the run, as per Flint et al. 2016
  temp.data$values[temp.data$values==0 & temp.data$lengths==1]<-1
  
  # convert this modified run length data set back to a data set of runs, but with single zeros within runs of 1 converted to 1's
  y<-inverse.rle(temp.data)
  
  # re-apply rle function to summarize new runs that were modified to include breaks of 1 zero
  final.runs<-rle(y)
  
  high.ice<-data.frame(unclass(final.runs)) # convert rle output to dataframe with 2 columns (values, lengths)
  high.ice<-subset(high.ice, values==1) # select only runs of 1's (remove runs of 0, where 0=days < 95% ice cover)
  high.ice<-subset(high.ice, lengths > 1) # select runs of 1's longer than 1 day
  sub.I<-high.ice$lengths*log(high.ice$lengths) # calculate subcomponent of the ice index, where I=sum(D*ln(D)), where D=run of 1's as defined in Flint et al. 2016
  
  I<-sum(sub.I) # sum the subcomponents of I to get I for the year
  #print(I)
  
}
# call function for each year with lapply
ws.out.ice<-lapply(year.file$year,heavy.ice)
# unlist
ws_index<-unlist(ws.out.ice)
# bind with year
ws_index<-cbind(year.file, ws_index)
# plot
ggplot(ws_index, aes(year, ws_index))+
  geom_line()

# 12. Calc heavy sic index for WINTER: winter_index ####
# function to calcuate high ice index for each year of data
heavy.ice<-function(YR){
  
  # subset year during which to summarize heavy ice
  year.one<-subset(winter_sic_core, year_winter==YR)
  #print(YR)
  subice<-year.one$min4_bs3 # sensor data on ice concentration
  #table(ice$month) #ice data are from Nov, Dec, Jan, Feb, Mar, and Apr
  
  # indicator for days >= 95%
  hiIce<-ifelse(subice>=95,1,0)
  
  # rle function to summarize runs in ice indicator variable
  temp.data<-rle(hiIce) # NB rle function output is 2 lists: "values" with value (in this case, 1 or 0) in the run, and "length" with length of the run
  
  # convert zeros with a 1 on each side to 1 to include one-day breaks in the run, as per Flint et al. 2016
  temp.data$values[temp.data$values==0 & temp.data$lengths==1]<-1
  
  # convert this modified run length data set back to a data set of runs, but with single zeros within runs of 1 converted to 1's
  y<-inverse.rle(temp.data)
  
  # re-apply rle function to summarize new runs that were modified to include breaks of 1 zero
  final.runs<-rle(y)
  
  high.ice<-data.frame(unclass(final.runs)) # convert rle output to dataframe with 2 columns (values, lengths)
  high.ice<-subset(high.ice, values==1) # select only runs of 1's (remove runs of 0, where 0=days < 95% ice cover)
  high.ice<-subset(high.ice, lengths > 1) # select runs of 1's longer than 1 day
  sub.I<-high.ice$lengths*log(high.ice$lengths) # calculate subcomponent of the ice index, where I=sum(D*ln(D)), where D=run of 1's as defined in Flint et al. 2016
  
  I<-sum(sub.I) # sum the subcomponents of I to get I for the year
  #print(I)
  
}
# call function for each year with lapply
winter.out.ice<-lapply(year.file$year,heavy.ice)
# unlist
winter_index<-unlist(winter.out.ice)
# bind with year
winter_index<-cbind(year.file, winter_index)
# plot
ggplot(winter_index, aes(year, winter_index))+
  geom_line()

# 13. Calc heavy sic index for spring: spring_index #####
heavy.ice<-function(YR){
  
  # subset year during which to summarize heavy ice
  year.one<-subset(ws_sic_core, year_winter==YR)
  #print(YR)
  subice<-year.one$min4_bs3 # sensor data on ice concentration
  #table(ice$month) #ice data are from Nov, Dec, Jan, Feb, Mar, and Apr
  
  # indicator for days >= 95%
  hiIce<-ifelse(subice>=95,1,0)
  
  # rle function to summarize runs in ice indicator variable
  temp.data<-rle(hiIce) # NB rle function output is 2 lists: "values" with value (in this case, 1 or 0) in the run, and "length" with length of the run
  
  # convert zeros with a 1 on each side to 1 to include one-day breaks in the run, as per Flint et al. 2016
  temp.data$values[temp.data$values==0 & temp.data$lengths==1]<-1
  
  # convert this modified run length data set back to a data set of runs, but with single zeros within runs of 1 converted to 1's
  y<-inverse.rle(temp.data)
  
  # re-apply rle function to summarize new runs that were modified to include breaks of 1 zero
  final.runs<-rle(y)
  
  high.ice<-data.frame(unclass(final.runs)) # convert rle output to dataframe with 2 columns (values, lengths)
  high.ice<-subset(high.ice, values==1) # select only runs of 1's (remove runs of 0, where 0=days < 95% ice cover)
  high.ice<-subset(high.ice, lengths > 1) # select runs of 1's longer than 1 day
  sub.I<-high.ice$lengths*log(high.ice$lengths) # calculate subcomponent of the ice index, where I=sum(D*ln(D)), where D=run of 1's as defined in Flint et al. 2016
  
  I<-sum(sub.I) # sum the subcomponents of I to get I for the year
  #print(I)
  
}
# call function for each year with lapply
spring.out.ice<-lapply(year.file$year,heavy.ice)
# unlist
spring_index<-unlist(spring.out.ice)
# bind with year
spring_index<-cbind(year.file, spring_index)
# plot
ggplot(spring_index, aes(year, spring_index))+
  geom_line()

# 14. Compile variables into dataframe and save ####
sea_ice_vars_1979_2019<-cbind(ws_hi,
                              winter_hi[2],
                              spring_hi[2],
                              ws_lo[2],
                              winter_lo[2],
                              spring_lo[2],
                              ws_index[2],
                              winter_index[2],
                              spring_index[2])

# 15. Save compiled sic variables as csv ####
write.csv(sea_ice_vars_1979_2019, "output/sea_ice_vars_1979_2019.csv")
