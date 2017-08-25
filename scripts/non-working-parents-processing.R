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
nw_parent_wide_calc2 <- nw_parent_wide %>% 
  mutate(`Percent Living with One Parent` = round((`Total Living with One Parent Value` / `Total Children Value`)*100,2), 
         `MOE of Percent Living with One Parent` = round((calcMOE(`Total Living with One Parent Value`, 
                                                                  `Total Children Value`, 
                                                                  `Total Living with One Parent MOE`, 
                                                                  `Total Children MOE`)), 2), 
         
         `Percent Living with Two Parents` = round((`Total Living with Two Parents Value` / `Total Children Value`)*100,2), 
         `MOE of Percent Living with Two Parents` = round((calcMOE(`Total Living with Two Parents Value`, 
                                                                   `Total Children Value`, 
                                                                   `Total Living with Two Parents MOE`, 
                                                                   `Total Children MOE`)), 2), 
         
         `Percent Living with One Parent Not in Work Force` = round((`Total Living with One Parent, Not in Work Force Value` / `Total Children Value`)*100,2), 
         `MOE of Percent Living with One Parent Not in Work Force` = round((calcMOE(`Total Living with One Parent, Not in Work Force Value`, 
                                                                                    `Total Children Value`, 
                                                                                    `Total Living with One Parent, Not in Work Force MOE`, 
                                                                                    `Total Children MOE`)), 2),     
         
         `Percent Living with Two Parents Neither in Work Force` = round((`Total Living with Two Parents, Neither in Work Force Value` / `Total Children Value`)*100,2), 
         `MOE of Percent Living with Two Parents Neither in Work Force` = round((calcMOE(`Total Living with Two Parents, Neither in Work Force Value`, 
                                                                                         `Total Children Value`, 
                                                                                         `Total Living with Two Parents, Neither in Work Force MOE`, 
                                                                                         `Total Children MOE`)), 2))

options(scipen=999)

nw_parents_long <- gather(nw_parent_wide_calc, Group, Value, 4:21, factor_key = FALSE)

nw_parents_long$`Measure Type` <- "Number"
nw_parents_long$`Measure Type`[which(grepl("Percent", nw_parents_long$Group))] <- "Percent"


nw_parents_long$Variable <- "Non-Working Parents"
nw_parents_long$Variable[which(grepl("MOE", nw_parents_long$Group))] <- "Margin of Error"

nw_parents_long$`Family Type` <- "All"
nw_parents_long$`Family Type`[which(grepl("Living with One Parent", nw_parents_long$Group))] <- "Living with One Parent"
nw_parents_long$`Family Type`[which(grepl("Living with Two Parents", nw_parents_long$Group))] <- "Living with Two Parents"

omit <- c("Total Living with Two Parents Value", "Total Living with Two Parents MOE", "Total Living with One Parent Value", "Total Living with One Parent MOE", 
          "Percent Living with One Parent", "Percent Living with Two Parents", "MOE of Percent Living with One Parent", "MOE of Percent Living with Two Parents")
nw_parents_long <- nw_parents_long[!nw_parents_long$Group %in% omit,]

case1 <- nw_parents_long[nw_parents_long$Year == "2015" & nw_parents_long$Town == "Connecticut",]
case1 <- case1 %>% arrange(`Family Type`, `Measure Type`)

nw_parents_long2 <- gather(nw_parent_wide_calc2, Group, Value, 4:21, factor_key = FALSE)

nw_parents_long2$`Measure Type` <- "Number"
nw_parents_long2$`Measure Type`[which(grepl("Percent", nw_parents_long2$Group))] <- "Percent"


nw_parents_long2$Variable <- "Non-Working Parents"
nw_parents_long2$Variable[which(grepl("MOE", nw_parents_long2$Group))] <- "Margin of Error"

nw_parents_long2$`Family Type` <- "All"
nw_parents_long2$`Family Type`[which(grepl("Living with One Parent", nw_parents_long2$Group))] <- "Living with One Parent"
nw_parents_long2$`Family Type`[which(grepl("Living with Two Parents", nw_parents_long2$Group))] <- "Living with Two Parents"

omit <- c("Total Living with Two Parents Value", "Total Living with Two Parents MOE", "Total Living with One Parent Value", "Total Living with One Parent MOE", 
          "Percent Living with One Parent", "Percent Living with Two Parents", "MOE of Percent Living with One Parent", "MOE of Percent Living with Two Parents")
nw_parents_long2 <- nw_parents_long2[!nw_parents_long2$Group %in% omit,]

case2 <- nw_parents_long2[nw_parents_long2$Year == "2015" & nw_parents_long2$Town == "Connecticut",]
case2 <- case2 %>% arrange(`Family Type`, `Measure Type`)

case1$Method <- "Denom: Sub-Group"
case2$Method <- "Denom: Total"

both_cases <- rbind(case1, case2)

names(both_cases)[names(both_cases) == "Group"] <- "Description"

write.table(
  both_cases,
  file.path(getwd(), "data", "denominator_case_study.csv"),
  sep = ",",
  row.names = F
)
