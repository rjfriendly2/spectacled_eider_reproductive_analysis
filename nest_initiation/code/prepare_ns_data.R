# Goal of this R script is to prepare the data for nest initiation date analysis
# The data is from the nest survival data INP data and will remove columns.

# Activate packages
library(data.table) #for data manipulation and visuals
library(tidyverse) #helps to handle and transform data

# Load the SPEI INP data
spei_data<- read.csv("nest_survival/data/INP_20220921.csv", header = TRUE)

# Remove columns by name to keep what is only needed
initiation_date <- spei_data[,!names(spei_data) %in% c("FirstFound",
                                                       "LastPresent",
                                                       "LastChecked",
                                                       "Fate",
                                                       "Freq",
                                                       "AgeDay1",
                                                       "Win_Lo",
                                                       "Spr_Lo")]

# Remove first column
initiation_date <- initiation_date[,-1]

# Write the csv file int he data folder
write.csv(initiation_date, "nest_initiation/data/initiation_wx.csv")
