library(dplyr)
library(datapkg)
library(tidyr)

##################################################################
#
# Processing Script for Non-working Parents
# Created by Jenna Daly
# On 08/24/2017
#
##################################################################

source('./scripts/getData.R')

#Setup environment
sub_folders <- list.files()
raw_location <- grep("raw", sub_folders, value=T)
path_to_raw <- (paste0(getwd(), "/", raw_location))

#Merge in FIPS
town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
fips <- (town_fips_dp$data[[1]])

nw_parents <- merge(get_data, fips, by = "FIPS", all=T)


