# Pittard, Steve wsp@emory.edu, November 2020

library(rvest)
library(leaflet)
library(janitor)

# Random web page with Zip Code / Income for Atlanta
url <- "http://localistica.com/usa/ga/atlanta/zipcodes/most-populated-zipcodes/"

# Get a specific table from the above URL
atl_info <- web %>% html_node("#dgZipCodes") %>% 
  html_table(header=TRUE) %>% clean_names() 

# Clean up the table 
atl_info <- atl_info %>% mutate(zip_code=as.numeric(zip_code)) %>% 
  mutate(income_per_household=as.numeric(gsub("[\\$,]", "", income_per_household))) %>% 
  filter(income_per_household != 0.00) %>%
  mutate(zip_code=as.character(zip_code))
  
# Get some Atlanta area Geogrpahic information at the county level 
# note - this code is a small alteration of Rich Majerus code found at
# https://rstudio-pubs-static.s3.amazonaws.com/508643_d6cb2a0b10484f40b1bcba052dda28e1.html

# Atlanta zips begin with 303
char_zips <- zctas(cb = TRUE, starts_with = "303")

# Do a left join 
char_zips <- geo_join(char_zips, 
                      atl_info, 
                      by_sp = "GEOID10", 
                      by_df = "zip_code",
                      how = "left")

# Setup some colors
pal <- colorNumeric(
  palette = "Reds",
  domain = char_zips$income_per_household)

# Need labels for when you mouse over the map
# create labels for zipcodes
labels <- 
  paste0(
    "Zip Code: ",
    char_zips$GEOID10, "<br/>",
    "Income Per Household: ",
    scales::dollar(char_zips$income_per_household)) %>%
  lapply(htmltools::HTML)

# Now use the mighty leaflet to help make the map

char_zips %>% 
  leaflet %>% 
  # add base map
  addProviderTiles("CartoDB") %>% 
  # add zip codes
  addPolygons(fillColor = ~pal(income_per_household),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
              highlight = highlightOptions(weight = 2,
                                           color = "#666",
                                           dashArray = "",
                                           fillOpacity = 0.7,
                                           bringToFront = TRUE),
              label = labels) %>% 

addLegend(pal = pal, 
          values = ~income_per_household, 
          opacity = 0.7, 
          title = htmltools::HTML("Mean House Hold Income <br> 
                                    by Atlanta Zip Code <br>
                                    2016"),
          position = "bottomright")
