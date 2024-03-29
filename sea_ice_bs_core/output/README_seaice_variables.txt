README_seaice_variables

Bering Sea sea ice concentration (sic) variables are created with code in the "sea_ice_bs_core/code" project folder
In that folder, there is code that:
1. sic binary files downloaded from NSIDC to geotifs: 01_convert_binary_bootstrap_to_geotif.R
2. extracts the 16 pixels of data relevant to the spectacled eider wintering area (Petersen & Douglas 2004): 02_extract_specEider_winterIce_bootstrap_sic_geotif.R
3. creates a dataset with sic summary variables by year of data for years of high ice (>=95%), low ice (< 15%), and the Flint extreme hi ice index and compiles them into 1 dataset: 04_summarize_daily_min4_bs3_append_to_allYears_20220228.R

That output dataset of summary sic variables is saved in: sea_ice_bs_core/output.
The relevant output dataset is: sea_ice_vars_1979_2019.csv
