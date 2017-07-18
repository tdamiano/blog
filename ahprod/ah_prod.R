# Housing Production Analysis
library(pacman)
p_load(tidyverse, janitor, stringr)

#All years all cities, remove townships/unicorporated (Using constant 2010 Met Council Def of Affordability @60% AMI)

ahprod_raw <- read_csv("./data/housing_prod_mc.csv") 

ahprod <- ahprod_raw %>% 
  clean_names() %>%
  select(-ctu_code, -update_date) %>% 
  rename(
    cntyfips  = co_code,
    ctu_fips  = coctu_id,
    type      = housing_type_desc,
    tenure    = housing_tenure,
    cost      = housing_cost,
    nunits    = quantity,
    comtype   = community_designation
  ) %>% 
  mutate(
    affyn  = ifelse(cost %in% c("AFF30", "AFF50", "AFF60"),"Affordable", "Market Rate")
  ) %>%
  dplyr::filter(!str_detect(ctu_name, 'Township')) %>%
  dplyr::filter(!str_detect(ctu_name, 'unorganized'))

##### Data cleaning for DataWrapper #######

# Fig 1 - Affordable Housing Production by Year 85-15
prodyr_dw<- ahprod %>% 
  group_by(affyn, year) %>% 
  summarise(
    prod = sum(nunits)
  ) %>% spread(affyn,prod) #To wide for dw

write_csv(prodyr_dw, "./data/prodyr_dw.csv")


# Fig 2 - Total Prod by Aff Level 95-15
afflev_dw <- ahprod %>%
  mutate(cost = ifelse(cost == "AFF115", "MKT", cost),
         city_type = ifelse(ctu_name %in% c("Minneapolis","St. Paul"),"Central City", "Suburb")) %>% 
  group_by(cost, city_type) %>%
  summarise(
    prod = sum(nunits)
  ) %>%
  spread(city_type, prod) %>% 
  ungroup()

write_csv(afflev_dw, "./data/afflev_dw.csv")





