# 01_convert_binary_bootstrap_to_geotif.R
# Adapted from code posted by A. Fischbach, 25 February 2020, and then adapted by G. Durner
# This program processes *.bin (i.e., binary) files of sea ice concentration from the NSIDC
# Specifically, SMMR and SSM/I Passive Microwave SIC from the Bootstrap Alagorithm.
# See https://nsidc.org/data/nsidc-0079, version 3.1, downloaded May 2020.
# See the following publication:
# Comiso, J. C. 2017. Bootstrap Sea Ice Concentrations from Nimbus-7 SMMR and DMSP SSM/I-SSMIS, Version 3. Northern Hemisphere. Boulder, Colorado USA. 
#   NASA National Snow and Ice Data Center Distributed Active Archive Center. doi: https://doi.org/10.5067/7Q8HCCWS4I0R. [Date Accessed].

#RStudio root dir: sea_ice_bs

#rm(list=ls())

library(rgdal)
library(raster)
library(sp)

# EPSG:3408: NSIDC EASE-Grid North
# EPSG:3409: NSIDC EASE-Grid South
# EPSG:3410: NSIDC EASE-Grid Global
# EPSG:3411: NSIDC Sea Ice Polar Stereographic North [bootstrap data, in meta data]
# EPSG:3412: NSIDC Sea Ice Polar Stereographic South
# EPSG:3413: WGS 84 / NSIDC Sea Ice Polar Stereographic North
# EPSG:4326: WGS 84 Geographic

# Read a list of bin files to process. Files were downloaded with program
# wget and saved with this naming convention:  bsYYYYMMDD, e.g. bs20080617
raw.list <- as.list(list.files(path = "raw_nsidc_binary/", pattern="*.bin"))

# iterate creation of a geotif file for each raw binary image file in raw.list...
# save coverted geotif files in /geotif
for (i in raw.list){ 
year <- substr(i, 3, 6) 

pixel <- 25000 #pixel dimension in meters for both x and y
xMin <- -3837500 #From NSIDC: ulxmap -3837500 projected map coord
xMax <- xMin + (pixel*304) #from meta data
yMax <- 5837500  #From NSIDC: ulymap  5837500
yMin <- yMax - (pixel*448)
r <- raster(nrow=448, ncol=304, xmn=xMin, xmx=xMax, ymn=yMin, ymx=yMax)
projection(r) <- '+init=epsg:3411'
        #filename <- paste0("C:/_thumb/eider_winter_icedat/demo_4fws_20200519/demo_download/", substr(i, 1, 10), ".bin")       
        filename <- paste0("raw_nsidc_binary/", substr(i, 1, 10), ".bin")       
        con <- file(filename, 'rb')
        x <- readBin(con, "int", size=2, endian="little", signed=FALSE, 300000) #binary integer as little edian format, unsigned - from meta data
        close(con)
        rr <- setValues(r, x) # place result in raster
		# set all non-ocean cells as NA        
		r1000 <- rr > 1000 #these are no data
		rr[r1000] <- NA
		# save the raster as a geotif
		#writeRaster(rr, paste0("C:/_thumb/eider_winter_icedat/demo_4fws_20200519/demo_geotif/", substr(i, 1, 10), ".tif"), "GTiff", overwrite=TRUE)		
		writeRaster(rr, paste0("geotif/", substr(i, 1, 10), ".tif"), "GTiff", overwrite=TRUE)		
		
}

# check output dir
geotif.list <- as.list(list.files(path = "geotif/", pattern="*.tif"))