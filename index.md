# Metro Analysis: Kansas City



```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

# Load Packages
library(tidycensus)
library(tidyverse)
library("stringr")
library(readxl)
library(kableExtra)
library(blsAPI)
library(rjson)
library(blscrapeR)
library(bea.R)
library(leaflet)
library(sf)
library(mapview)
library(tmap)
library(RColorBrewer)
library(cartogram)

# Set Census API Key 
census_key = "1ec66b188f803d840cbac94da58f536c2d1eb7f3"
census_api_key(census_key)

```

```{r, echo=F, message=F, warning=F, include = F}

###########################################################################
### Metro Analysis by Zip Code - Kansas City Metro Area
###########################################################################

# Counties Needed
# Wyandotte, KS
# Johnson, KS
# Platte, MO
# Jackson, MO
# Clay, MO


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
########################################
### Set Years and Location Variables ###
current = 2019
lag = 2014

cur_var = "value_cur"
lag_var = "value_lag"

state = c("KS", "MO")

# Import Zip Codes for Kansas City Metro
zips = read_excel(path = "~/Desktop/JOHN/!REAL_ESTATE/Research/R Projects/Metro_Zip_Codes.xlsx",
                  sheet = "Kansas_City_Metro")
colnames(zips) = c("GEOID", "County", "State")
zips$GEOID = as.character(zips$GEOID)

########################################
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

```





```{r, echo=F, message=F, warning=F}

########################################
########################################
### Median Household Income ###
########################################
########################################

# SET VARIABLE
var = "B19013_001"

# Pull Current Year Data - Zip Code Level
data_cur = get_acs(survey = "acs5",
                    year = current,
                    variable = var,
                    geography = "zcta",
                    state = state,
                   geometry = TRUE)
data_cur = data_cur %>% select(GEOID, estimate, geometry)
colnames(data_cur) = c("GEOID", cur_var, "geometry")
data_cur = inner_join(data_cur, zips, byu = c("GEOID"))

# Pull 5 year ago data - Zip Code Level 
data_lag = get_acs(survey = "acs5",
                    year = lag,
                    variable = var,
                    geography = "zcta")
data_lag = data_lag %>% select(GEOID, estimate)
colnames(data_lag) = c("GEOID", lag_var)
data_lag = inner_join(data_lag, zips, byu = c("GEOID"))

# Join Current and 5 year ago data - Zip Code Level 
data = inner_join(data_cur, data_lag, by = c("GEOID", "County", "State"))
data = data %>% mutate(Growth = 100*(value_cur/value_lag-1))
data = data %>% mutate(Change = value_cur-value_lag)
col_order = c("State", "County", "GEOID", "value_cur", "value_lag", "Growth", "Change", "geometry")
data = data[, col_order]

# Change Name Below
Metro_HHINC = data
colnames(Metro_HHINC) = c("State", 
                          "County",
                          "GEOID",
                          "HHINC_cur",
                          "HHINC_lag",
                          "HHINC_Growth", 
                          "HHINC_Change",
                          "geometry")

```


### Median Household Income 

```{r, echo=F, message=F, warning=F}

# Median Household Income Levels

mypal = colorBin(palette = "YlGnBu", domain = Metro_HHINC$HHINC_cur,
                 bins = 10, pretty = TRUE)

bbox = st_bbox(Metro_HHINC) %>% as.vector()

popup = paste0("Zip Code: ", Metro_HHINC$GEOID, "<br>", 
               "Level: ", Metro_HHINC$HHINC_cur, "<br>",
               "Growth: ", round(Metro_HHINC$HHINC_Growth),0)

mymap_1 = leaflet(options = leafletOptions(zoomSnap = 0)) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = Metro_HHINC, 
              fillColor = ~mypal(Metro_HHINC$HHINC_cur),
              color = "#b2aeae",
              fillOpacity = 0.7,
              weight = 1,
              smoothFactor = 0.2,
              popup = popup) %>% 
  addLegend(pal = mypal, 
            values = Metro_HHINC$HHINC_cur, 
            position = "bottomright", 
            title = "Median Household Income")

mymap_1

```


