# code to average daily sea ice concentration values from 4 pixels in the Petersen and Douglas 2004 core SPEI wintering area
# AND....

# code by Dan Rizzolo
# last edit 18 Feb 2022

# RStudio project base directory: 

# got packages?
library (ggplot2)
library(lubridate)
library(tidyverse)

# input data: daily sea ice concentration in 16 pixels, pixels 1 to 4 are SPEI "core" winter area (Petersen and Douglas 2004)
# file created with "02_extract_specEider_winterice_bootstrap_sic_geotif.R"
all.ice<-read.csv("output/1979_2020_winterSpecEdier_SICbootstrapV31_16pixels.csv", header=T)
str(all.ice)

# format date
all.ice$date<-as.Date(as.character(all.ice$sicDate),"%Y%m%d")

# create year variable
all.ice$year<-format(all.ice$date, "%Y")
table(all.ice$year)

# subset data that includes data from 16 pixels to 4 core pixels Petersen and Douglas call the "core" area, number 1 to 4
core.ice<-subset(all.ice, pixel < 5)

# convert ice coverage to percent, divide values by 10
core.ice$bs_sic_pct<-core.ice$bs_sic_x10/10
hist(core.ice$bs_sic_pct)

# get daily mean across the 4 pixels
mean.daily.sic<-aggregate(core.ice$bs_sic_pct~as.factor(core.ice$sicDate), FUN=mean)
colnames(mean.daily.sic)<-c("sicDate", "mean.ice.pct")
str(mean.daily.sic)

# format date in various repetitive and inefficent ways
mean.daily.sic$date<-as.Date(as.character(mean.daily.sic$sicDate),"%Y%m%d")
mean.daily.sic$jday<-format(mean.daily.sic$date, "%j")
mean.daily.sic$n.jday<-as.numeric(mean.daily.sic$jday)
mean.daily.sic$year<-format(mean.daily.sic$date, "%Y")
mean.daily.sic$n.year<-as.numeric(mean.daily.sic$year)
mean.daily.sic$month<-format(mean.daily.sic$date, "%m")
mean.daily.sic$n.month<-as.numeric(mean.daily.sic$month)
mean.daily.sic$short.date<-format(mean.daily.sic$date, format="%m-%d")

# remove 29 Feb when it occurs (at least until I figure out a better way to deal with leap years)
mean.daily.sic<-subset(mean.daily.sic, short.date != "02-29")

# subset to winter years: Dec to April
winter.ice<-subset(mean.daily.sic, n.month < 5 | n.month > 11)

# create sequential winter day starting with day 1 = 1 Dec
#winter.ice$winter.day<-ifelse(winter.ice$n.jday < 335, winter.ice$n.jday +366, winter.ice$n.jday)
#winter.ice$winter.day<-winter.ice$winter.day-334
#summary(winter.ice$winter.day)

# long-term mean of daily sea ice concentration in each pixel over all years
#mean.ice<-aggregate(winter.ice$mean.ice.pct~winter.ice$short.date, FUN=mean)
mean.ice<-aggregate(winter.ice$mean.ice.pct~winter.ice$short.date, FUN=mean)
colnames(mean.ice)<-c("short.date", "pct.ice")
str(mean.ice)

# extract month as numeric and make a factor with order 12, 1, 2, 3, 4
##mean.ice$date<-as.Date(mean.ice$s.date, format="%m-%d")
##mean.ice$n.month<-format(mean.ice$date, "%m")
##mean.ice$f.month<-factor(month, levels=c("12", "01", "02", "03", "04"))

# make days of winter sequential starting 1 December as day 1 and running through April 30
##mean.ice$winter.day<-ifelse(mean.ice$jday<335, mean.ice$jday+366, mean.ice$jday)
##mean.ice$winter.day<-mean.ice$winter.day-334

#plot(mean.ice$winter.day, mean.ice$pct.ice)

# create winter day for mean.ice dataframe
# make days of winter sequential starting 1 December as day 1 and running through April 30
# make julian day variable
temp.date<-as.Date(mean.ice$short.date, format=("%m-%d"))
mean.ice$jday<-as.numeric(format(temp.date, "%j"))
mean.ice$winter.day<-ifelse(mean.ice$jday<335, mean.ice$jday+366, mean.ice$jday)
mean.ice$winter.day<-mean.ice$winter.day-334
summary(mean.ice$winter.day)

# create winter day for winter.ice dataframe
winter.ice$winter.day<-ifelse(winter.ice$n.jday<335, winter.ice$n.jday+366, winter.ice$n.jday)
winter.ice$winter.day<-winter.ice$winter.day-334
summary(mean.ice$winter.day)

# table that matches winter.day to short date and jday
dates.suck<-cbind.data.frame(mean.ice$short.date, mean.ice$jday, mean.ice$winter.day)
colnames(dates.suck)<-c("short.date", "jday", "winter.day")
dates.suck<-dates.suck[!duplicated(dates.suck),]

# winter 2018-2019 means
winter18<-subset(winter.ice, n.year==2018 & n.month > 5 |
                   n.year==2019 & n.month<12)
#winter18$f.month<-factor(month, levels=c("12", "01", "02", "03", "04"))

# winter 2019-2020 means
winter19<-subset(winter.ice, n.year==2019 & n.month > 5 |
                   n.year==2020 & n.month<12)

# tack it all together into one dataframe for plotting
winter2018<-cbind.data.frame(winter18$short.date, winter18$mean.ice.pct, winter18$n.jday, winter18$winter.day)
winter2018$line<-"Winter 2018"
colnames(winter2018)<-c("short.date", "pct.ice", "jday", "winter.day", "line")
winter2019<-cbind.data.frame(winter19$short.date, winter19$mean.ice.pct, winter19$n.jday, winter19$winter.day)
winter2019$line<-"Winter 2019"
colnames(winter2019)<-c("short.date", "pct.ice", "jday", "winter.day", "line")
mean.ice$line<-"Mean 1979-2020"

plot.data<-rbind.data.frame(mean.ice, winter2018, winter2019)

# plot sea ice by date and year
p.ice<-ggplot(data=plot.data, aes(x=winter.day, y=pct.ice)) +
  geom_line(aes(color=line), size=0.5)+
  scale_x_continuous(breaks=c(1, 33, 64, 92, 123), 
                     label=c("Dec", "Jan", "Feb", "Mar", "Apr")) +
  scale_color_manual(values=c("#636363","#e6550d","#3182bd"))+
  labs(color=" ")+
  xlab(" ")+
  ylab("Percent Ice Cover")+
  theme_bw()+
  theme(legend.position="top",
        legend.box="horizontal",
        legend.text=element_text(size=6))
p.ice

ggsave(paste("output/", "ice_cover_pct_core_mean_2018_2020.png", sep=""),
       width=10, height=10, units="cm", dpi=300)


# first shot
#p.ice<-ggplot() +
#  geom_line(data=mean.ice, aes(x=winter.day, y=pct.ice), color="black")+
#  geom_line(data=winter18, aes(x=winter.day, y=mean.ice.pct), color="red")+
#  geom_line(data=winter19, aes(x=winter.day, y=mean.ice.pct), color="blue")+
#  scale_x_continuous(breaks=c(1, 33, 64, 92, 123), 
#                     label=c("Dec", "Jan", "Feb", "Mar", "Apr")) +
#  theme_bw()
#p.ice
