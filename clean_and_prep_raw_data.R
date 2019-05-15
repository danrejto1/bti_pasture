### Title: Creating tidy (i.e. long) consistently labeled data files for pasture
### Original Author: Dan Rejto
### Last Update: May 9, 2019, Dan Rejto

######## Setup -------------------------------------------------------------------
### CLEAR ENVIRONMENT BEFORE RUNNING. IF YOU DONT, YOU'LL NEED TO PLAY WITH the "Rename Variables" section on making variable names lowercase

#load packages   
require('tidyverse')
require('scales')
require('plotly')
require('rlang')

######## Load Raw Data -----------------------------------------------------------
###FAO Data
# I downloaded csv from FAOSTAT from the noted datasets with following selections:
# Regions > World > (List)  
# All years 
# Output Formattiong Options set to "Units" only
# Output type: Table
# Thousand seperator: none
# Selected Elements & Items of interest for each dataset as described in codebook file.


## land area / pasture data
land_area_country <- read_csv("data/raw_data/fao_country_level_land_area.csv") #from land use. selected Land area, Agricultural land, land under perm. meadows and pastures, forest land

# japan (unlike other countres) stops reporting permanent pasture data in 2000 and starts reporting temporary pasture. I download the data for "-- Land under temp. meadows and pastures" only for Japan and merge it in
japan_temp_pasture <- read_csv("data/raw_data/fao_japan_pasture.csv")
japan_temp_pasture$Item <- recode(japan_temp_pasture$Item, "Land under temp. meadows and pastures" = "Land under perm. meadows and pastures")

land_area_country <- bind_rows(land_area_country, japan_temp_pasture)  #merge in japan temp. pasture values

# Global grazing land data from Hyde 3.2, accessed 12-5-18 from https://easy.dans.knaw.nl/ui/datasets/id/easy-dataset:74467/tab/2 File name: grazing_r.txt
hyde <- read_delim(file = "data/raw_data/hyde_v3-2_grazing_by_region.txt", delim = " ")


## consumption and production data
# I selected as many of the following as available: ass, buffalo, camel, cattle, goat, horse, mule, other camelids, pig and sheep

# country level
cattle_stocks_country <- read_csv("data/raw_data/fao_country_level_cattle_stocks.csv") #from live animals, in number of animal head. just downloaded for cattle
cons_country <- read_csv("data/raw_data/fao_country_level_meat_milk_food_supply.csv") #from food balance sheets
meat_prod_country <- read_csv("data/raw_data/fao_country_level_meat_production.csv") #from livestock primary
milk_prod_country <- read_csv("data/raw_data/fao_country_level_milk_production.csv") #from livestock primary

# global-level - For following datasets I selected Regions > World + (Total)
cattle_stocks_global<- read_csv("data/raw_data/fao_global_cattle_stocks.csv")  #from live animals, in numbers of animal head
cons_global <- read_csv("data/raw_data/fao_global_consumption.csv") #from food balance sheets
meat_prod_global <- read_csv("data/raw_data/fao_global_meat_production_yields_all_herd_animals.csv") #from livestock primary
milk_prod_global <- read_csv("data/raw_data/fao_global_milk_production_yields_all_herd_animals.csv") #from livestock primary


## country to region crosswalk
crosswalk <- read_csv("data/raw_data/fao_country_region_groups.csv") #from Definitions and Standards

## country GDP/income data
# World Bank COuntry Historical Income Classification, 
# accessed 12-20-18 from http://databank.worldbank.org/data/download/site-content/OGHIST.xls 
# or https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups

# you may need to open excel file and change last column name to X__1. It renders differently depending on the version of the readxl package 
income_country <- readxl::read_xls(path = "data/raw_data/OGHIST.xls", sheet = 2, range = "A5:AF229")
income_country <- income_country[c(7:nrow(income_country)),] #remove extra rows
income_country <- select(income_country, "country" = `Bank's fiscal year:`,"inc_group_16"=FY16, "iso_code" = X__1) #select columns


## World Bank Country GDP Data
#accessed 1-18-19 from https://databank.worldbank.org/data/pasture-variables/id/5535337d
# if link above doenst work or load variables listed below, follow these instructions
# Under Country panel, deselect anything already selected, click "Countries" (Not All or Aggregates), and Select All
#select years 1961 to 2016
#select GDP curent (US$), GDP growth (annual %), GDP per capita (current US$),GDP per capita growth (annual %), Population, total
#download as csv
#after downloading, extract zip file and use larger file, with "Data" in the filename
gdp_pop_country <- read_csv("data/raw_data/world_bank_gdp_pop_country.csv", na = "..")
gdp_pop_country <- filter(gdp_pop_country, is.na(`Country Name`)==F, `Country Name`!="") #remove extra blank rows for metadata


## FAO consumption forecasts for 2050
# Accessed 05-14-19 from from FAO’s “Food and agriculture projections to 2050” website http://www.fao.org/global-perspectives-studies/food-agriculture-projections-to-2050/en/
# Downloaded “Country data for all indicators” zip file  http://www.fao.org/fileadmin/user_upload/global-perspective/csv/FOFA2050CountryData_all.zip
# Citation: FAO. 2018. The future of food and agriculture – Alternative pathways to 2050. Rome.
fao_forecast <- read_csv("data/raw_data/fao_all_2050_forecasts_by_country.csv") %>% 
  filter(Scenario == "Business As Usual",   #just look at BAU forecast
         Indicator == "Commodity balances, volume",
         Item %in% c("Beef and veal", "Raw milk"),
         Element == "Food use") %>% 
  select(-Element, -Indicator, -Domain, -Scenario) %>% 
  rename(country = CountryName, "iso_code" = CountryCode, "tot_cons" = Value)
# note: this dataset incldues only 145 countries/entities while the historical FAO ones include 239 countries. Some of the entities int his dataset are not individual countries, but instead are groups like "Rest of SSA" or "Rest of SAS"



## Climate categorizations (and income grouping over time)
# shared by Cecille Godde. I manually added NA in row 1366, which was empty, and changed Grenada's 2014 income status to Upper Middle Income to be consistent with its current World Bank income classification
climate_income_categories <- read_csv(file = "data/raw_data/climate_income_categories_from_godde.csv") #climate & income groups over time
climate_income_categories <- unique(select(climate_income_categories, -year))


#rename production variables so the variable for tonnes & head have different names
meat_prod_country$Element <- paste0(meat_prod_country$Element, "_", meat_prod_country$Unit)
meat_prod_global$Element <- paste0(meat_prod_global$Element, "_", meat_prod_global$Unit)

######## Select Data of interest from All Files  ---------------------------------
#select variables of interest for country data and rename area to country for consistency as well
cattle_stocks_country <- select(cattle_stocks_country,Country = Area, Element, Item, Year,  Value)
cons_country <- select(cons_country, Country, Element, Item, Year,  Value)
land_area_country<- select(land_area_country, Country=Area, Element, Item, Year,  Value) 
meat_prod_country <- select(meat_prod_country, Country = Area, Element, Item, Year,  Value)
milk_prod_country <- select(milk_prod_country, Country = Area, Element, Item, Year,  Value)

#select variables of interest for global data
cattle_stocks_global <- select(cattle_stocks_global, Element, Item, Year, Value)
cons_global <-  select(cons_global, Element, Item, Year,  Value)
meat_prod_global <- select(meat_prod_global, Element, Item, Year, Value)
milk_prod_global <- select(milk_prod_global, Element, Item, Year, Value)

hyde <- filter(hyde, region=="Total")


######## Reshape variables --------------------------------------------------------
# When there are multiple variables currently in 1 column, like yield and nubmer of milk animals, spread across multiple columns
# Animal types are not a variable, but instead a subgroup of observation, so all animals remain under 1 "Item" column
cattle_stocks_country <- spread(data = cattle_stocks_country, key = Element, value = Value)
cons_country <- spread(data = cons_country, key = Element, value = Value)
land_area_country <- spread(data = land_area_country, key = Element, value = Value)
meat_prod_country <- spread(data = meat_prod_country, key = Element, value = Value)
milk_prod_country <- spread(data = milk_prod_country, key = Element, value = Value)

cattle_stocks_global <- spread(data = cattle_stocks_global, key = Element, value = Value)
cons_global <-  spread(data = cons_global, key = Element, value = Value)
meat_prod_global <- spread(data = meat_prod_global, key = Element, value = Value)
milk_prod_global <- spread(data = milk_prod_global, key = Element, value = Value)
#meat_yields_global<-  spread(data = meat_yields_global, key = Element, value = Value)
#milk_yields_global<-  spread(data = milk_yields_global, key = Element, value = Value)

#gather multiple columns for year in HYDE and GDP into 1 column like other datasets
hyde <- gather(hyde, key = "year", value="area", -region)
hyde$year <- as.integer(hyde$year)

gdp_pop_country <- select(gdp_pop_country, -`Country Code`, -`Series Code`) %>%
  gather(key = "year", value="values", -`Country Name`, -`Series Name`) %>%
  spread(key = `Series Name`, value = values)


######## Rename variables ----------------
#make all var names lowercase for consistency
dfs <- ls()

for(df in dfs) {
  df.tmp <- get(df)
  colnames(df.tmp) <- tolower(colnames(df.tmp))
  assign(df, df.tmp)
}
rm(df.tmp, df, dfs)


#give vars short names 
cons_country <- rename(cons_country, cons_total = `domestic supply quantity`,
                       protein_pc = `protein supply quantity (g/capita/day)`,  
                       cons_pc_kg = `food supply quantity (kg/capita/yr)`,   
                       cons_pc_cal =`food supply (kcal/capita/day)`)

cons_global <-  rename(cons_global, cons_total = `food supply quantity (tonnes)`,
                       protein_pc = `protein supply quantity (g/capita/day)`,  
                       cons_pc_kg = `food supply quantity (kg/capita/yr)`,   
                       cons_pc_cal =`food supply (kcal/capita/day)`,
                       fat_pc = `fat supply quantity (g/capita/day)`)

cons_global <- select(cons_global, -`food supply quantity (g/capita/day)`) #remove food quantity/day in g

gdp_pop_country <- rename(gdp_pop_country, country = `country name`, 
                          gdp_pc = `gdp per capita (current us$)`, 
                          gdp_pc_ann_pct_chg = `gdp per capita growth (annual %)`, 
                          pop = `population, total`,
                          gdp_ann_pct_chg = `gdp growth (annual %)`,
                          gdp = `gdp (current us$)`)

meat_prod_country <- rename(meat_prod_country, m_head = `production_head`, m_prod = production_tonnes, m_yld = `yield/carcass weight_hg/an`)
meat_prod_global <- rename(meat_prod_global, m_head = `production_head`, m_prod = production_tonnes, m_yld = `yield/carcass weight_hg/an`)

milk_prod_country <- rename(milk_prod_country, d_head = `milk animals`, d_prod = production, d_yld = `yield`)
milk_prod_global <- rename(milk_prod_global, d_head = `milk animals`, d_prod = production, d_yld = `yield`)



######## Recode factors ------------------

# Recode factor names for consistency and combine camels with other camelids (for which theres only data on Peru and Bolivia)
level_key <- c("Meat indigenous, camel" = "camelids",
               "Meat indigenous, cattle" = "cattle",
               "Meat indigenous, goat" = "goat",
               "Meat indigenous, sheep" = "sheep",
               "Meat indigenous, pig" = "pig",
               "Meat indigenous, horse" ="horse",
               "Meat indigenous, buffalo" = "buffalo",
               "Meat indigenous, other camelids" = "camelids",
               "Meat indigenous, ass" = "ass",
               "Meat indigenous, mule" = "mule",
               "Milk, whole fresh buffalo" = "buffalo", 
               "Milk, whole fresh camel"   ="camel",
               "Milk, whole fresh cow"     ="cattle",
               "Milk, whole fresh goat"    = "goat",
               "Milk, whole fresh sheep" ="sheep")

meat_prod_country$item <- recode(meat_prod_country$item, !!!level_key)
meat_prod_global$item <- recode(meat_prod_global$item, !!!level_key)
milk_prod_country$item <- recode(milk_prod_country$item, !!!level_key)
milk_prod_global$item <- recode(milk_prod_global$item, !!!level_key)

land_area_country$item <- recode(land_area_country$item, `Agricultural land` = "ag_land", `Forest land`="forest", `Land area`="total_land", `Land under perm. meadows and pastures`="pasture")

cattle_stocks_global$item <- recode(cattle_stocks_global$item, Cattle = "cattle")
cattle_stocks_country$item <- recode(cattle_stocks_country$item, Cattle = "cattle")

#recode/rename years in gdp data
gdp_pop_country$year <- gsub("\\[.*?\\]", "", gdp_pop_country$year) #replace [text] with nothing

# Reclassify variables
 gdp_pop_country$year <- as.numeric(gdp_pop_country$year)

 
######## Restructure and Rename Crosswalk  ---------------------------

# prepare minor/major group merge
# filter country-region crosswalk to create 2 groupings, major and minor regions 
minor_crosswalk <- filter(crosswalk, `country group` %in% c("Eastern Africa", "Northern Africa", "Southern Africa", "Middle Africa","Western Africa",
                                                            "Central America", "Northern America", "South America","Caribbean",
                                                            "Central Asia","Western Asia","Southern Asia","Eastern Asia","South-Eastern Asia",
                                                            "Eastern Europe","Northern Europe","Western Europe","Southern Europe",
                                                            "Oceania")) %>%
  select(minor_group = `country group`, country, "iso_code" = `iso3 code`) #rename group column

major_crosswalk <- filter(crosswalk, `country group` %in% c("Africa", "Americas", "Europe", "Asia", "Oceania")) %>%
  select(major_group = `country group`, country, "iso_code"=`iso3 code`) #rename group column & add ISO code

# customize crosswalk to separate former Soviet Union countries as their own major and minor region
fsu_countries <- c("Armenia", "Azerbaijan","Belarus","Estonia","Georgia","Kazakhstan", "Kazachstan",
                   "Kyrgyzstan", "Kyrgystan","Latvia", "Lithuania", "Republic of Moldova", "Moldova", "Tajikistan",
                   "Turkmenistan", "Ukraine", "Uzbekistan", "USSR", "Russia", "Russian Federation")


major_crosswalk$major_group[which(major_crosswalk$country %in% fsu_countries)] <- "Former Soviet Union"
minor_crosswalk$minor_group[which(minor_crosswalk$country %in% fsu_countries)] <- "Former Soviet Union"

#customize crosswalk to place South Sudan in Northern Africa, where all of former Sudan was categorized
minor_crosswalk$minor_group[which(minor_crosswalk$country == "South Sudan")] <- "Northern Africa"

# customize crosswalk to separate former Yugoslavian countries as their own minor region
fyug <- c("Yugoslav SFR", "Bosnia and Herzegovina", "Croatia", "Macedonia", 
          "The former Yugoslav Republic of Macedonia", "Montenegro", "Serbia and Montenegro",
          "Serbia", "Slovenia")

minor_crosswalk$minor_group[which(minor_crosswalk$country %in% fyug)] <- "Former Yugoslavia"


######## Address outliers for meat and milk production --------

# meat yields, productionn & head
# Hong Kong, Belgium, Bosnia, Bulgaria, Czechia, Malaysia, Eswatini, Malta, Mauritius, MOntenegro, Réunion, Singapore, Slovakia, Suriname, and Saudi Arabaia 
# report a value of 10000 for beef, sheep, goat or horse yields a few years. This is almost more than 2x any other observation. 
# Kazhakstan also reports yields of 7450 for horses one year, over 2x yields that of any other year. 
# I assume these are data reporting errors or are meant to represent NA.

# There are no similar high outliers for production or head
# but there are 1 and 0 values for prod & head that must represent missing values. 
# I conduct a  simple type of interpolation setting these values to the previously observed value 

#create function to replace NAs with the last observation (last observation carried forward or LOCF)
na.locf2 <- function(x) zoo::na.locf(x, na.rm = FALSE)

#set outlier values to NA
meat_prod_country$m_yld[meat_prod_country$m_yld>7000] <- NA
meat_prod_country[meat_prod_country %in% c(1,0)] <- NA

# apply the LOCF function to each column, grouping by country and item so that if the first obs is NA, 
# the function doesn't replace with observation from different country or item
meat_prod_country <- transform(meat_prod_country, m_yld = ave(m_yld, country, item, FUN = na.locf2))
meat_prod_country <- transform(meat_prod_country, m_head = ave(m_head, country, item, FUN = na.locf2))
meat_prod_country <- transform(meat_prod_country, m_prod = ave(m_prod, country, item, FUN = na.locf2))

# milk yields
# Iran & Greece's buffalo yields stand out as being ~50% higher (>21k) than for any other country starting in 2010 for Iran & in the years 2012/13 for Greece
# The netherlands, Kuwait, Kazhakstan, and Ethiopia PDR have 0 or 1 values for prod
# The netherlands, Hong Kong, and Kuwait have 0 or 1 values for head
# Kuwait, Kazhakstan, and Ethiopia PDR and Mali have 0 values for yields some years
# I do the same type of interpolation as before for these high outliers and low-liers that I assume are missing values

# set outlier values to NA
milk_prod_country$d_yld[milk_prod_country$d_yld>20200 & milk_prod_country$item=="buffalo"] <- NA
milk_prod_country[milk_prod_country %in% c(1,0)] <- NA

# apply the LOCF function to each column, grouping by country and item so that if the first obs is NA, the function doesn't replace with observation from different country or item
milk_prod_country <- transform(milk_prod_country, d_yld = ave(d_yld, country, item, FUN = na.locf2))
milk_prod_country <- transform(milk_prod_country, d_head = ave(d_head, country, item, FUN = na.locf2))
milk_prod_country <- transform(milk_prod_country, d_prod = ave(d_prod, country, item, FUN = na.locf2))



######## Address outliers for pasture --------
# Create convenience pasture dataset
pasture_country <- filter(land_area_country, item == "pasture") %>% #filter land area to just pasture
  select(-item) #remove item column 

pasture_country$area <- pasture_country$area*1000  #CHANGE UNITS FOR PASTURE AREA FROM THOUSANDS OF HECTARES TO HECTARES

# # explore pasture data visually
#  p <- ggplot(pasture_country, aes(x=year, y=area, color=country)) + geom_line()
#  ggplotly(p)
# 
# #calculate year by year % change
# pasture_country <- pasture_country %>%
#   group_by(country) %>% 
#   arrange(year, .by_group=T) %>% #order from lowest year to highest for each country
#   mutate(pct_chg = (area/dplyr::lag(area)-1)*100) #calculate pct change from 1 year to year before
# 
# #examine pct change for countries with large pasture area (defined as >=100,000 ha)
# temp <- filter(pasture_country, area >=500000)
# 
# ## determine percent change values that are outliers, defined as outside the 0.1 - 99.9% percentile range 
# pasture_cutoff <- quantile(temp$pct_chg, probs = c(0.001,0.999), na.rm=T)
# 
# #filter to just outlier observations 
# pasture_outliers <- filter(temp, pct_chg > pasture_cutoff[2] |  pct_chg < pasture_cutoff[1])
# 
# unique(pasture_outliers$country) #look at outlier countries
# 
# p <- ggplot(filter(pasture_country, country %in% pasture_outliers$country | 
#                      str_detect(country, "Sudan") |
#                      country=="Australia"),
#                    aes(x=year, y=area, color=country))+
#   geom_line()
# ggplotly(p)


## Adjust outlier values when possible
# Japan outlier value in 2001 is likely due to change from reporting permantent to reporting temporary pasture. Don't know what drives 2013 increase.
# I make not adjustment to it

# FAO has no relevant notes for following countries w/ outlier values
# Ireland
# Iran. Only note is "Data on "Land under permanent meadows and pastures" refer to rangelands in "good" and "fair" conditions, thus excluding "poor" condition rangelands. "
# Guatemala 
# Lithuania
# Viet Nam
# I do not make any adjustments to these in the primary analysis.

# FAO Notes that some of these outlier values are due to methodlogy changes here: http://fenixservices.fao.org/faostat/static/documents/RL/RL_EN_Country_Notes.pdf
# Ecuador,  their methodology changed in 2014 when pasture area dropped 35%
# Hungary, "Since 2010, the data relating to “Land under permanent meadows and pastures” exclude the unutilized grassland area
# Japan:  Since 2013,"Land under temporary meadows and pastures" category has changed to include all herbaceous forage crops in addition to pastures. 

# Australia also has a steep pasture decline from 2014-2015. 
# Sudan split into Sudan and South Sudan from 2010-2011. There was a steep drop in pasture area from 2008-2009 preceding this

# I adjust these values in the primary analysis.
# For ecuador, hungary, japan and Sudan I assume their outlier changes were entirely due to reporting changes.
# For each year where there is an outlier change value, I assign the previous year's value. I then adjust future values so the year-to-year changes remain the same as originally reported in FAOSTAT
adjusted_countries <- data_frame(country = c("Ecuador", "Hungary", "Japan", "Sudan, Former"),
                                 year = c(2014, 2010, 2013, 2009))
pasture_country[pasture_country$country=="Ecuador" & pasture_country$year>=2014,]$area <- 
  pasture_country[pasture_country$country=="Ecuador" & pasture_country$year>=2014,]$area + 
  (pasture_country[pasture_country$country=="Ecuador" & pasture_country$year==2013,]$area - 
     pasture_country[pasture_country$country=="Ecuador" & pasture_country$year==2014,]$area)

pasture_country[pasture_country$country=="Hungary" & pasture_country$year>=2010,]$area <- 
  pasture_country[pasture_country$country=="Hungary" & pasture_country$year>=2010,]$area + 
  (pasture_country[pasture_country$country=="Hungary" & pasture_country$year==2009,]$area - 
     pasture_country[pasture_country$country=="Hungary" & pasture_country$year==2010,]$area)

pasture_country[pasture_country$country=="Japan" & pasture_country$year>=2013,]$area <- 
  pasture_country[pasture_country$country=="Japan" & pasture_country$year>=2013,]$area + 
  (pasture_country[pasture_country$country=="Japan" & pasture_country$year==2012,]$area - 
     pasture_country[pasture_country$country=="Japan" & pasture_country$year==2013,]$area)

sudan_adjustment <- pasture_country[pasture_country$country=="Sudan (former)" & pasture_country$year==2008,]$area - 
  pasture_country[pasture_country$country=="Sudan (former)" & pasture_country$year==2009,]$area

pasture_country[pasture_country$country=="Sudan (former)" & pasture_country$year>=2009,]$area <- 
  pasture_country[pasture_country$country=="Sudan (former)" & pasture_country$year>=2009,]$area +
  sudan_adjustment

#for south sudan and sudan (post split) I allocate the Sudan former adjustment in proportion to their current pasture area
# this maintains the drop in pasture of ~6 Mha from Sudan splitting intot he 2 countries, but eliminates the 08-09 drop of ~35Mha
s_sudan_area <- pasture_country[pasture_country$country=="South Sudan" & pasture_country$year==2016,]$area
sudan_area <- pasture_country[pasture_country$country=="Sudan" & pasture_country$year==2016,]$area
total_new_sudan_area <- s_sudan_area+sudan_area

pasture_country[pasture_country$country=="South Sudan",]$area <- 
  pasture_country[pasture_country$country=="South Sudan",]$area + 
  (sudan_adjustment * s_sudan_area/total_new_sudan_area)

pasture_country[pasture_country$country=="Sudan" & pasture_country$year>=2009,]$area <- 
  pasture_country[pasture_country$country=="Sudan" & pasture_country$year>=2009,]$area + 
  (sudan_adjustment * sudan_area/total_new_sudan_area)


######## ADjust Australia Pasture --------
#For Australia, personal communication with FAO staff indicates that:

#The drop between 2014 and 2015 was at a "rate more than 20 times faster than the previous 30-year average."
#"The latter acceleration is not 'real'. It is likely linked to a change of reporting methodology in Australia"
# Starting in 2015, as FAOSTAT Country Notes explain, Australia began to report to FAO only land owned by agricultural businesses and over a certain value. 

# I assume that Australian pasture declined from 2014-2015 at the 30 year rate prior to 2015.

#calculate 30 yr rate of change in ha
australia_30_yr_rate <- (pasture_country[pasture_country$country=="Australia" & pasture_country$year==2014,]$area -
                           pasture_country[pasture_country$country=="Australia" & pasture_country$year==(2014-30),]$area)/30

#set 2015 value to 2014 value, adjusting for 30 yr rate of change 
pasture_country[pasture_country$country=="Australia" & pasture_country$year==2015,]$area <- 
  pasture_country[pasture_country$country=="Australia" & pasture_country$year==2014,]$area +
  australia_30_yr_rate

#set 2016 value to new 2015 value, adjusting for 30 yr rate of change 
pasture_country[pasture_country$country=="Australia" & pasture_country$year==2016,]$area <- 
  pasture_country[pasture_country$country=="Australia" & pasture_country$year==2015,]$area +
  australia_30_yr_rate

#some analyses also suggest that a portion of the the pasture decline in Australia was due to 
# transfer of pastoral land from non-indigenous leases that are included in FAO reporting to 
# indigenous pastoral leases & conservation reserves that arent included in FAO reporting. 

# According to van Etten, E. J. (2013). Changes to land tenure and pastoral lease ownership in Western Australia’s central rangelands: implications for co-operative, landscape-scale management. The Rangeland Journal, 35(1), 37-46.
# there was an increase in land under indigenous leases & conservation reserves of 12.54 Mha between 1955 & 2008
# This is calculated by multiplying the study area in the paper (76 m ha) 
# by the % under indigenous leases (5%) and the % under conservation reserves (11.5%). 
# I assume area under these land types before 1955 was 0.
# I calculate an annual conversion rate by dividing 12.54 across the years in the time period (1955-2008)
# then I add that conversion rate to each Australia observation from 1961-2016

aus_conv_rate <- (12.54*1000000)/(2008-1955) #annual conversion rate
aus_cumulative_conversion <- aus_conv_rate * seq(1,2017-1961,1) #create vector of new indigenous or conservation land each year

#add new indigenous or conservation land to existing Australia pasture data
pasture_country[pasture_country$country=="Australia",]$area <- 
  pasture_country[pasture_country$country=="Australia",]$area +
  aus_cumulative_conversion


######## Adjust USSR and Former USSR Pasture --------

#pasture area for former soviet countries jumps 10% from 326500000 in 1991 to 360407400 in 1992 post-dissolution. 
# Although in % terms, this is less than in some other countries, the timing is worrying and the absolute change is very large
# I therefore replace the pasture areas for USSR and former soviet countries with the values from HYDE 3.2. 
# Some of these values are spaced 10 years apart. I assume linear change between each of these points
 

#load country level data of historical pasture area as modeled by HYDE v3.2 available at ftp://ftp.pbl.nl/hyde/hyde3.2/baseline/txt/
# the units are km^2
km2_to_ha <- 100
hyde_pasture_country <- read_csv("data/raw_data/hyde_v3-2_pasture_by_country.csv", skip = 1, na = "#N/A")
hyde_pasture_country <- hyde_pasture_country %>%
  filter(is.na(country_name)==F) %>% #remove rows not representing countries. these have no area either
  gather(key = year, value=area, -country_code,	-country_name_formula, -country_name) %>%
  mutate(year=as.integer(year),
         area = area*km2_to_ha)%>%
  filter(year>=1960, #filter to years of interest 
         country_name %in% c(fsu_countries, "Russia", "Moldova", "Kazachstan","Kyrgystan")) #filter to former soviet countries

hyde_fsu_names <- unique(hyde_pasture_country$country_name)
years <- 1960:2016

#create empty tibble with rows for all years and each country
new_fsu_pasture <- tibble(country = rep(hyde_fsu_names,length(years)),
                          year = rep(years, each=length(hyde_fsu_names)), 
                          area = NA)

#transfer values from hyde to empty tibbl for years with values in hyde
for (i in hyde_fsu_names) {
     for (j in years) {
       if(j %in% unique(hyde_pasture_country$year)){
       new_fsu_pasture[new_fsu_pasture$country == i &
                           new_fsu_pasture$year == j,]$area <- 
         hyde_pasture_country[hyde_pasture_country$country_name == i & 
                                hyde_pasture_country$year == j,]$area
       }
     }
}

#linearly interpolate between decades for missing values
decades <- c(1960, 1970, 1980, 1990, 2000)
for (i in hyde_fsu_names) {   #for each country
  for (d in decades) {  #for each decade except 2000+ where we have annual data
    rate <- (new_fsu_pasture[new_fsu_pasture$country == i &   #calculate growth rate for decade
                               new_fsu_pasture$year == (d+10),]$area - 
               new_fsu_pasture[new_fsu_pasture$country == i & 
                                 new_fsu_pasture$year == d,]$area)/10
    for (y in (d+1):(d+9)) {   #for each year between decades we already have values for 
      new_fsu_pasture[new_fsu_pasture$country == i &    
                        new_fsu_pasture$year == y,]$area <- #assign the year to be ...
        (new_fsu_pasture[new_fsu_pasture$country == i & 
                          new_fsu_pasture$year == y-1,]$area + rate)  #equal to the previous year plus growth rate for decade
    }
  }
}
    

##verify it worked
# new_fsu_pasture %>% 
#     ggplot(aes(x=year, y=area, color=country)) +geom_line()
# 
# summarize(group_by(new_fsu_pasture, year), tot=sum(area)) %>% ggplot(aes(x=year, y=tot))+geom_line()
# pasture_country %>% filter(country %in% fsu_countries) %>% group_by(year) %>% summarize(tot=sum(area)) %>% ggplot(aes(x=year, y=tot)) + geom_line()

#make country names of new fsu data match FAO names
# pasture_country$country <- recode(pasture_country$country,
#                                    "Republic of Moldova" = "Moldova") 
new_fsu_pasture$country <- recode(new_fsu_pasture$country,
                                  "Kazachstan"="Kazakhstan", 
                                  "Kyrgystan" = "Kyrgyzstan",
                                  "Moldova"="Republic of Moldova", 
                                  "Russia" ="Russian Federation")

# new_fsu_to_join_with_fulldata <- pasture_country %>% 
#   filter(country %in% fsu_countries, country!="USSR") %>% #limit to FSU data from pasture data
#   select(-area) %>% #remove old area data
#   complete(country, year) %>%   #create new empty observations for all years to enable joining
#   left_join(new_fsu_pasture) #add new area data

pasture_country <- pasture_country %>%  
  filter(!country %in% fsu_countries) %>% #filter to all countries except FSU 
  bind_rows(filter(new_fsu_pasture, year!=1960)) %>%   #bind new FSU data except for 1961 from pasture
  bind_rows(tibble(country = rep("USSR",56), year = c(1961:2016), area = rep(NA, 56))) #add NA USSR rows to join later with USSR production data



######## Adjust Ethiopia and Eritrea --------

# When Ethiopia PDR splits into Ethio[ia and Eritrea in 1993, total perm. pasture area across the countries declines more than 50%
# I adopt the same approach as for Sudan, assuming that pasture area remains the same post-split and allocate the additional area across the countries proportionate to their reported pasture area post-split

ethiopia_adjustment <- pasture_country[pasture_country$country=="Ethiopia PDR" & pasture_country$year==1992,]$area - 
  summarize(group_by(filter(pasture_country, country %in% c("Ethiopia", "Eritrea"), year==1993), year), area = sum(area))$area

# I allocate the adjustment in proportion to the 2 country's current pasture area
eritrea_area <- pasture_country[pasture_country$country=="Eritrea" & pasture_country$year==2016,]$area
ethiopia_area <- pasture_country[pasture_country$country=="Ethiopia" & pasture_country$year==2016,]$area
total_new_area <- eritrea_area+ethiopia_area

pasture_country[pasture_country$country=="Eritrea",]$area <- 
  pasture_country[pasture_country$country=="Eritrea",]$area + 
  (ethiopia_adjustment * eritrea_area/total_new_area)

pasture_country[pasture_country$country=="Ethiopia",]$area <- 
  pasture_country[pasture_country$country=="Ethiopia",]$area + 
  (ethiopia_adjustment * ethiopia_area/total_new_area)

pasture_country %>% filter(country %in% c("Ethiopia","Ethiopia PDR", "Eritrea")) %>% ggplot(aes(x=year, y=area, color=country))+geom_line()


######## Join country-level datasets in master datasets  -----------------------

#create one country-level dataset for beef and dairy with all variables except income and gdp
combined_country <- 
  full_join(pasture_country,  #merge pasture area with...
            select(filter(meat_prod_country, item=="cattle"), -item)) %>% #beef production vars
  full_join(select(filter(milk_prod_country, item=="cattle"),  -item)) %>% #dairy production vars
  full_join(select(filter(cons_country, item=="Bovine Meat"),  -item)) %>% #beef consumption vars
  rename("m_protein_pc"=protein_pc, "m_cons_total"=cons_total, "m_cons_pc_kg"=cons_pc_kg, "m_cons_pc_cal"=cons_pc_cal) %>%  #rename beef cons variables
  full_join(select(filter(cons_country, item=="Milk - Excluding Butter"),  -item)) %>% #merge with dairy consumption vars
  rename("d_protein_pc"=protein_pc, "d_cons_total"=cons_total, "d_cons_pc_kg"=cons_pc_kg, "d_cons_pc_cal"=cons_pc_cal) %>%  #rename dairy cons variables
  full_join(select(filter(cattle_stocks_country, item=="cattle"), -item)) %>% #merge with cattle stocks
  full_join(minor_crosswalk) %>% #merge with minor groups
  full_join(major_crosswalk) %>% #merge with major groups
  full_join(climate_income_categories) #merge with cliamte categories
  
#join World Bank GDP to World bank income data since the WDI Databank doesnt have ISO codes
gdp_pop_country <- left_join(gdp_pop_country, select(income_country, country, "iso_code"))

combined_country <- left_join(combined_country, 
                               select(filter(gdp_pop_country, is.na(iso_code)==F), -country),
                                      by=c("iso_code", "year"))

land_area_country <- left_join(land_area_country, major_crosswalk)

cons_country <- left_join(cons_country, minor_crosswalk)
cons_country <- left_join(cons_country, major_crosswalk)

meat_prod_country <- left_join(meat_prod_country, minor_crosswalk)
meat_prod_country <- left_join(meat_prod_country, major_crosswalk)

milk_prod_country <- left_join(milk_prod_country, minor_crosswalk)
milk_prod_country <- left_join(milk_prod_country, major_crosswalk)

pasture_country <- left_join(pasture_country, minor_crosswalk)
pasture_country <- left_join(pasture_country, major_crosswalk)

cattle_stocks_country <- left_join(cattle_stocks_country, minor_crosswalk)
cattle_stocks_country <- left_join(cattle_stocks_country, major_crosswalk)


############## Recode   ##############
### Recode country names for to make names consistent over time, despite historical country name changes
combined_country$country <- recode(combined_country$country,
                                   "Russian Federation"="Russia", 
                                   "China, mainland" = "China", 
                                   "Ethiopia PDR" = "Ethiopia")



############## Merge In Country Income Class Data  ---------------------------
#countries that don't merge using iso
fao_missing <- unique(anti_join(combined_country, income_country, by="iso_code")$country)
income_missing <- unique(anti_join(income_country, combined_country, by="iso_code")$country)

# Countries in 2015-16 FAO data not given an income by World Bank:
# British Virgin Islands, Falkland islands, French Guiana, Guadeloupe, Martinique, Mayotte, Montserrat,  Niue, Norfolk Island, Réunion, Saint Helena, Western Sahara

# Older countries not given income data include Belgium-Luxembourg and Ethiopia PDR and USSR

#Most of these are either very small countries or no longer exist. Since we are interested in change over time, we need to assign several of these countries that dont exist anymore or that don't exist in World Bank data with income categorizations.  

#Also, the channell islands exist in both datasets, but with differetn ISO codes. I fix this here. 
income_country$iso_code <- recode(income_country$iso_code, "CHI" = "CHA") #chanell islands

#merge on ISO 3 Code column in crosswalk, excluding country column from income data in order to not duplicate column
combined_country <- left_join(combined_country, select(income_country, -country), by="iso_code")

#add income categorizations to countries that no longer exist
combined_country[combined_country$country=="Russia",]$inc_group_16 <- "H"   #USSR
combined_country[combined_country$country=="Ethiopia",]$inc_group_16 <- "L"   #Ethiopia PDR
combined_country[combined_country$country=="Belgium-Luxembourg",]$inc_group_16 <- "H"   
combined_country[combined_country$country=="Serbia and Montenegro",]$inc_group_16 <- "UM"
combined_country[combined_country$country=="Yugoslav SFR",]$inc_group_16 <- "UM"

# change South Sudan income category since it is different from Sudan (former), making it seem as if there is a large
# change in variables by income group when the country splits
combined_country[combined_country$country=="South Sudan",]$inc_group_16 <- "LM"

#add income categorizations to countries with substantial pasture area that dont have World Bank categorizations
combined_country[combined_country$country=="Western Sahara",]$inc_group_16 <- "LM" #had 2007 GDP per capita of $2500 accordinv to the CIA

###Create Alternative 3-level income classification
combined_country$inc_group_16_alt <- recode(combined_country$inc_group_16, "UM"="M", "LM" ="M")

### Reorder/code income classifications
combined_country$inc_group_16 <- as_factor(combined_country$inc_group_16)
combined_country$inc_group_16 <- fct_relevel(combined_country$inc_group_16, "H", "UM", "LM", "L", "..")

combined_country$inc_group_16_alt <- as_factor(combined_country$inc_group_16_alt)
combined_country$inc_group_16_alt <- fct_relevel(combined_country$inc_group_16_alt, "H", "M", "L", "..")


#add income groups and major and minor region to fao_forecast
fao_forecast <- left_join(fao_forecast, 
                          select(filter(combined_country, is.na(iso_code)==F, year==2016), iso_code, inc_group_16_alt, inc_group_16, minor_group, major_group, income4classes, income3classes),
                          by=c("iso_code"))


############## Create filtered datasets for ease of use --------

#global totals for animal production, based on adjusted country data from above. this is for all herding animals included
meat_prod_global_adjusted <- summarize(group_by(meat_prod_country, year, item), m_head=sum(m_head, na.rm=T), m_prod=sum(m_prod, na.rm=T) , m_yld=m_prod/m_head)
milk_prod_global_adjusted <- summarize(group_by(milk_prod_country, year, item), d_head=sum(d_head, na.rm=T), d_prod=sum(d_prod, na.rm=T) , d_yld=d_prod/d_head)

#Global consumption of bovine, mutton and goat, and pig meat, and of dairy
meat_cons_global <- filter(cons_global, item %in% c("Bovine Meat", "Mutton & Goat Meat", "Pigmeat"))
milk_cons_global <- filter(cons_global, item %in% c("Butter, Ghee", "Cheese", "Cream","Milk - Excluding Butter"))


############## Save tidy and cleaned data --------
write_csv(x = combined_country, path = "data/clean_data/combined_tidy.csv")
write_csv(x = meat_cons_global, path = "data/clean_data/meat_cons_global.csv")
write_csv(x = milk_cons_global, path = "data/clean_data/milk_cons_global.csv")
write_csv(x = meat_prod_global, path = "data/clean_data/meat_prod_global.csv")
write_csv(x = milk_prod_global, path = "data/clean_data/milk_prod_global.csv")
write_csv(x = meat_prod_global_adjusted, path = "data/clean_data/meat_prod_global_adjusted.csv")
write_csv(x = milk_prod_global_adjusted, path = "data/clean_data/milk_prod_global_adjusted.csv")
write_csv(x = cattle_stocks_global, path = "data/clean_data/cattle_stocks_global.csv")
write_csv(x = major_crosswalk, path = "data/clean_data/major_crosswalk.csv")
write_csv(x = minor_crosswalk, path = "data/clean_data/minor_crosswalk.csv")
write_csv(x = hyde, path = "data/clean_data/hyde.csv")
write_csv(x = land_area_country, path = "data/clean_data/land_area.csv")
write_csv(x = fao_forecast, path = "data/clean_data/fao_beef_milk_consumption_forecast.csv")