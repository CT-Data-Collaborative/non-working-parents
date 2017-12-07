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

nw_parents <- merge(get_data, fips, by = "FIPS", all.y=T)

#Convert from long to wide to calculate rates
nw_parents_value <- nw_parents %>% select(-MOE)
nw_parents_value <- spread(nw_parents_value, Group, Value)

nw_parents_moe <- nw_parents %>% select(-Value)
nw_parents_moe <- spread(nw_parents_moe, Group, MOE)

#Append either Value or MOE to columns so we can merge columns
names(nw_parents_value)[4:8] <- gsub("$", " Value", names(nw_parents_value)[4:8])
names(nw_parents_moe)[4:8] <- gsub("$", " MOE", names(nw_parents_moe)[4:8])

nw_parent_wide <- merge(nw_parents_value, nw_parents_moe, by = c("FIPS", "Year", "Town"))

#############################################################################################
# Helper function for MOE
calcMOE <- function(x, y, moex, moey) {
  moex2 <- moex^2
  moey2 <- moey^2
  d <- x/y
  d2 <- d^2
  
  radicand <- ifelse(
    moex2 < (d2 * moey2),
    moex2 + (d2 * moey2),
    moex2 - (d2 * moey2)
  )
  
  return(sqrt(radicand)/y)
}

# calculate group rates with total denominators,
# keep MOES, calculating appropriately
nw_parent_wide_calc <- nw_parent_wide %>% 
  mutate(`% Living with One Parent` = ((`Total Living with One Parent Value` / `Total Children Value`)*100), 
         `MOE of % Living with One Parent` = (calcMOE(`Total Living with One Parent Value`, 
                                                      `Total Children Value`, 
                                                      `Total Living with One Parent MOE`, 
                                                      `Total Children MOE`)), 
         
         `% Living with Two Parents` = ((`Total Living with Two Parents Value` / `Total Children Value`)*100), 
         `MOE of % Living with Two Parents` = (calcMOE(`Total Living with Two Parents Value`, 
                                                       `Total Children Value`, 
                                                       `Total Living with Two Parents MOE`, 
                                                       `Total Children MOE`)), 
         
         `% Living with One Parent No Work` = ((`Total Living with One Parent, Not in Work Force Value` / `Total Children Value`)*100), 
         `MOE of % Living with One Parent No Work` = (calcMOE(`Total Living with One Parent, Not in Work Force Value`, 
                                                              `Total Children Value`, 
                                                              `Total Living with One Parent, Not in Work Force MOE`, 
                                                              `Total Children MOE`)),     
         
         `% Living with Two Parents No Work` = ((`Total Living with Two Parents, Neither in Work Force Value` / `Total Children Value`)*100), 
         `MOE of % Living with Two Parents No Work` = (calcMOE(`Total Living with Two Parents, Neither in Work Force Value`, 
                                                               `Total Children Value`, 
                                                               `Total Living with Two Parents, Neither in Work Force MOE`, 
                                                               `Total Children MOE`)))

options(scipen=999)

nw_parents_long <- gather(nw_parent_wide_calc, Group, Value, 4:21, factor_key = FALSE)

nw_parents_long$`Measure Type` <- "Number"
nw_parents_long$`Measure Type`[which(grepl("%", nw_parents_long$Group))] <- "Percent"

nw_parents_long$Variable <- "Non-Working Parents"
nw_parents_long$Variable[which(grepl("MOE", nw_parents_long$Group))] <- "Margins of Error"

nw_parents_long$`Family Type` <- "All"
nw_parents_long$`Family Type`[which(grepl("One Parent", nw_parents_long$Group))] <- "Living with One Parent"
nw_parents_long$`Family Type`[which(grepl("Two Parent", nw_parents_long$Group))] <- "Living with Two Parents"

nw_parents_long$`Parent Employment Status` <- "All"
nw_parents_long$`Parent Employment Status`[which(grepl("Work", nw_parents_long$Group))] <- "Not in Work Force"

nw_parents_long$Group <- NULL

nw_parents_long$Value <- round(nw_parents_long$Value, 2)

nw_parents_long <- nw_parents_long %>% 
  select(Town, FIPS, Year, `Family Type`, `Parent Employment Status`, `Measure Type`, Variable, Value) %>% 
  arrange(Town, Year, `Measure Type`, Variable, `Family Type`, `Parent Employment Status`)

# Write to File
write.table(
  nw_parents_long,
  file.path(getwd(), "data", "non-working-parents_2016.csv"),
  sep = ",",
  row.names = F
)
