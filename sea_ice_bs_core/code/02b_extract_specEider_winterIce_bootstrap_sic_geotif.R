# 02_extract_specEider_winterice_bootstrap_sic_geotif.R - extract the area of interest
# Adapted from code posted by A. Fischbach, 25 February 2020, and then adapted by G. Durner
# This program processes *.bin (i.e., binary) files of sea ice concentration from the NSIDC
# Specifically, SMMR and SSM/I Passive Microwave SIC from the Bootstrap Alagorithm.
# See https://nsidc.org/data/nsidc-0079, version 3.1, downloaded May 2020.
# See the following publication:
# Comiso, J. C. 2017. Bootstrap Sea Ice Concentrations from Nimbus-7 SMMR and DMSP SSM/I-SSMIS, Version 3. Northern Hemisphere. Boulder, Colorado USA. 
#   NASA National Snow and Ice Data Center Distributed Active Archive Center. doi: https://doi.org/10.5067/7Q8HCCWS4I0R. [Date Accessed].

# requires files:
# specEider_winter_pixel_samples_xy_geo_SAVE.csv: contains reference lat/long for each SIC grid cell (16 cells from Petersen & Douglas 2004 wintering area)
# input are geotif (*tif) files created from SIC *.bin files downloaded from NSIDC

# currently setup to process 1 year of data (most recently available)
# and append that new data to the data set with all years of SIC data

library(rgdal)
library(raster)
library(sp)

# EPSG:3408: NSIDC EASE-Grid North
# EPSG:3409: NSIDC EASE-Grid South
# EPSG:3410: NSIDC EASE-Grid Global
# EPSG:3411: NSIDC Sea Ice Polar Stereographic North
# EPSG:3412: NSIDC Sea Ice Polar Stereographic South
# EPSG:3413: WGS 84 / NSIDC Sea Ice Polar Stereographic North
# EPSG:4326: WGS 84 Geographic

# make SIC grid cell reference coordinates spatial with spTransform from package sp
#eider16.geo=read.csv("C:/_thumb/eider_winter_icedat/demo_4fws_20200519/specEider_winter_pixel_samples_xy_geo_SAVE.csv")
eider16.geo=read.csv("lib/specEider_winter_pixel_samples_xy_geo_SAVE.csv")
coordinates(eider16.geo)= ~ lon + lat
proj4string(eider16.geo)<-CRS('+init=epsg:4326') #geo WGS84
eider16.ps<-spTransform(eider16.geo, CRS('+init=epsg:3411')) #into ice prj

# get a list of the geotif files to interrogate....
#tif.list <- as.list(list.files(path = "C:/_thumb/eider_winter_icedat/demo_4fws_20200519/demo_geotif", pattern = "bs201802"))
tif.list <- as.list(list.files(path = "geotif/", pattern = "*.tif"))

if(exists('eider16p')) rm(eider16p) ## Clear final results if lingering...

# see note on code origin (below) [get value for each pixel]
for (i in tif.list){ 
        month<- as.numeric(substr(i, 7, 8)) ## extract month from filename, filename format="bs%Y%m%d.tif"
          if(month >= 11 | month <= 5){  ## just process Dec-Apr...
          #filename <- paste0("C:/_thumb/eider_winter_icedat/demo_4fws_20200519/demo_geotif/", i)  
          filename <- paste0("geotif/", i) ## get filename
          rr <- raster(filename) # create raster
          eiderValues=extract(rr, eider16.ps) # extract SIC values from raster
          xycoords <- as.data.frame(coordinates(eider16.ps)) # spatial ref data for 16 winter area pixels
          names(xycoords)[1:2] <- c("x","y")
          eiderOne <- as.data.frame(cbind(eider16.geo,eiderValues)) # bind raster SIC with grid cell ref
          names(eiderOne)[2] <- "bs_sic_x10" # name the SIC values, which are in format percent*10
          eiderOne$sicDate <- substr(i,3,10) # extract the date corresponding to SIC values
          eiderOne <- cbind(eiderOne,xycoords) # bind date with SIC and ref coordinate
          eiderOne
          ## append current to final results...
          if(!exists('eider16p')){ 
            eider16p<-eiderOne 
          }else{
            eider16p<-rbind(eider16p, eiderOne)
          }
        }        
}
# output is eider16p
str(eider16p)

# save output
# year of data just processed (assuming one year of data processed)
year<- as.numeric(substr(tif.list[1], 3, 6))
# save it
write.csv(eider16p, paste("output/", year, "_winterSpecEider_SICbootstrapv31_16pixels.csv", sep=""))                   
# append it to full data set
# load full data set
all_previous<-read.csv("output/1979_2019_winterSpecEdier_SICbootstrapV31_16pixels.csv")
# rbind most recent data to full data set
bind<-rbind.data.frame(all_previous, eider16p)
# save it
write.csv(bind, paste("output/","1979_2020_winterSpecEdier_SICbootstrapV31_16pixels.csv", sep=""))

#outfile1 <- paste0("C:/_thumb/eider_winter_icedat/demo_4fws_20200519/_winterSpecEdier_SICbootstrapV31_16pixels.csv", sep="")
#outfile1 <- paste0("output/1979_2020_winterSpecEdier_SICbootstrapV31_16pixels.csv", sep="")
#write.table(eider16p,file=outfile1, append=FALSE, quote=FALSE, col.names=TRUE, row.names=FALSE, sep=",", na = "NA") 
#write.table(eider16p,file=outfile1, append=FALSE, quote=FALSE, col.names=TRUE, row.names=FALSE, sep=",", na = "NA") 

