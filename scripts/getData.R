library(acs)
source('./scripts/acsHelpers.R')

# ACS B23008
# Get geography object for CT and subcounty divisions
acsdata <- getACSData(
  getCTGeos("town"),
  yearList = 2010:2019,
  get_this_one = "B23008"
)

get_data <- data.table()
for (data in acsdata) {
  year <- as.numeric(data@endyear)
  year <- paste(year-4, year, sep="-")
  
  total_children <- acsSum(data, 1, "Total Children")
  total_children_2parents <- acsSum(data, c(3,16), "Total Living with Two Parents")
  total_children_2parents_no_work <- acsSum(data, c(7,20), "Total Living with Two Parents, Neither in Work Force")
  total_children_1parent <- acsSum(data, c(8,21), "Total Living with One Parent")
  total_children_1parent_no_work <- acsSum(data, c(11, 14, 24, 27), "Total Living with One Parent, Not in Work Force")
  
  #based on the VD0x <- the x is what you want as the range, signifies which columns you want to sum
  # total_under6_living_with_2_parents, total_6to17_living_with_2_parents = "Total living with 2 parents"
  # total_under6_living_with_2_parents_neither_in_labor_force, total_6to17_living_with_2_parents_neither_in_labor_force = "Total living with 2 parents, neither in work force"
  # total_under6_living_with_1_parent, total_6to17_living_with_1_parent = "Total living with 1 parent"
  # total_under6_living_with_1_parent_living_with_father_not_in_labor_force, total_under6_living_with_1_parent_living_with_mother_not_in_labor_force, 
  # total_6to17_living_with_1_parent_living_with_father_not_in_labor_force, total_6to17_living_with_1_parent_living_with_mother_not_in_labor_force = "Total living with 1 parent, not in work force"

  datafips <- data.table(fips = getACSFips(data))
  
  
  estimates <- data.table(
    FIPS = datafips$fips,
    Year = year,
    estimate(total_children),
    estimate(total_children_2parents),
    estimate(total_children_2parents_no_work),
    estimate(total_children_1parent),
    estimate(total_children_1parent_no_work)
  )
  
  names(estimates)[names(estimates) == "HD01_VD01.Estimate; Total:"] <- "Total Children"
  names(estimates)[names(estimates) == "B23008_001E.Estimate!!Total"] <- "Total Children"
  names(estimates)[names(estimates) == "B23008_001E.Estimate!!Total:"] <- "Total Children"
  
  estimates <- melt(
    estimates,
    id.vars = c("FIPS", "Year"),
    variable.name = "Group",
    variable.factor = F,
    value.name = "Value",
    value.factor = F
  )
  
  moes <- data.table(
    FIPS = datafips$fips,
    Year = year,
    #multiplying by 1.645 assigns confidence level to 90%
    # SE = MOE / 1.645
    standard.error(total_children) * 1.645,
    standard.error(total_children_2parents) * 1.645,
    standard.error(total_children_2parents_no_work) * 1.645,
    standard.error(total_children_1parent) * 1.645,
    standard.error(total_children_1parent_no_work) * 1.645
  )
  
  names(moes)[names(moes) == "HD01_VD01.Estimate; Total:"] <- "Total Children"
  names(moes)[names(moes) == "B23008_001E.Estimate!!Total"] <- "Total Children"
  names(moes)[names(moes) == "B23008_001E.Estimate!!Total:"] <- "Total Children"
  
  
  moes <- melt(
    moes,
    id.vars = c("FIPS", "Year"),
    variable.name = "Group",
    variable.factor = F,
    value.name = "MOE",
    value.factor = F
  )
  
  setkey(estimates, FIPS, Year, Group)
  setkey(moes, FIPS, Year, Group)
  
  get_data <- rbind(get_data, estimates[moes])
}

get_data <- get_data[get_data$FIPS != "0900100000",]

# Write to File
write.table(
  get_data,
  file.path(getwd(), "raw", "group_totals.csv"),
  sep = ",",
  row.names = F
)