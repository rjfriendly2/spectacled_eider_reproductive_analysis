01(01b)_convert_binary_bootstrap_to_geotif.R 
important script
converts *.bin files downloaded from NSIDC to geotif files
output geotifs are saved in "geotif" directory under root directory of the RStudio project "/sea_ice_bs_core/"
b version of the file has adjusted file directories, non-b version is the original file provided by Dave Douglas


02(02b)_extract_specEider_winterIce_bootstrap_sic_geotif.R
important script
requires: "specEider_winter_pixel_samples_xy_geo_SAVE.csv" file with a reference coordinate (lot, lon) for each of the 16 grid cells from which NSIDC sea ice concentration data are obtained using the wget script written by Dave Douglas
This code converts sea ice concentration in geotif files (extracted from *bin files with 01 script above)
Output is a *.csv file ([date range]_winterSpecEdier_SICbootstrapV31_16pixels.csv) with sea ice concentration (bs_sic_x10) scaled by 10 (so divide values by 10 to get percent values) with pixel number, date, and pixel reference coordinate


03_daily_means_pixels_1to4_[date]
Calculates daily mean sea ice concentration in the 4 pixels (mean.ice) during winter months
output is figure "ice_cover_pct_core_mean_2018_2019.png"


04_summarize_daily_min_bs3_sic_[version date]
Code to create and complile Bering Sea sea ice concentration (sic) summary variables for the spec eider "core" area (Petersen and Douglas 2004), including: 
days of ice >=95%, 
days of ice < 15%, 
heavy ice index (Flint et al. 2016) days; 
all summarized each winter-year (1979-2019) for 3 seasons (winter-spring [01 Nov-30 Apr], winter [01 nov-31 Mar], spring (1-30 Apr])