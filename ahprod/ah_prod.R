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


### Cost burden Analysis ###
cbrent <- readxl::read_excel("./data/region_cb.xlsx", sheet = 1) %>%
cbown <- readxl::read_excel("./data/region_cb.xlsx", sheet = 2)


#Renters 
# by income level
cbbyinc <- cbrent %>% 
  filter(lowincome == "Y") %>%
  mutate(
    incomeband = factor(incomeband,  levels = c("Less than $10,000", "$10,000 to $19,999", "$20,000 to $34,999", "$35,000 to $49,999"))
  ) %>% 
  group_by(incomeband) %>%
  summarise(
    tothh  = sum(nhh),
    ncb    = sum(nhh[costburden == "Y"]),
    pct_cb  = (ncb/tothh)*100,
    nxcb   = sum(nhh[excostburden == "Y"]),
    pctxcb = (nxcb/tothh)*100
  )
  
# total
cbtotal <- cbrent %>% 
  filter(lowincome == "Y") %>%
  mutate(
    incomeband = factor(incomeband,  levels = c("Less than $10,000", "$10,000 to $19,999", "$20,000 to $34,999", "$35,000 to $49,999"))
  ) %>% 
  summarise(
    tothh  = sum(nhh),
    ncb    = sum(nhh[costburden == "Y"]),
    pct_cb  = (ncb/tothh)*100,
    nxcb   = sum(nhh[excostburden == "Y"]),
    pctxcb = (nxcb/tothh)*100
  ) %>% mutate(incomeband = "All")

x <- cbrent %>% 
  filter(lowincome == "Y") %>%
  mutate(
    incomeband = factor(incomeband,  levels = c("Less than $10,000", "$10,000 to $19,999", "$20,000 to $34,999", "$35,000 to $49,999"))
  ) %>% group_by(afflevel, incomeband) %>% 
  summarise(
    totunits = sum(nhh)
  )

# by income level crosstab
xtab_dw <- cbrent %>% 
  mutate(
    afflevel = ifelse(afflevel %in% c("Less than 20.0 Percent","20.0 to 24.9 Percent","25.0 to 29.9 Percent"), "nocb",
                      ifelse(afflevel %in% c("30.0 to 34.9 Percent", "35.0 to 39.9 Percent", "40.0 to 49.9 Percent"), "cb",
                             ifelse(afflevel == "50.0 Percent or More", "xcb", NA)))
  ) %>% 
  group_by(incomeband, afflevel) %>% 
  summarise(
    nhh = sum(nhh, na.rm = TRUE)
  ) %>% spread(afflevel, nhh) %>% 
  select(-`<NA>`) %>% drop_na(incomeband)

write_csv(xtab_dw, "./data/xtab_dw.csv")
