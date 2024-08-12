# JOINING ALL THE EXTERNAL DATA FRAMES TOGETHER 
library(readr)
load("/Users/sophiemccauley/Library/CloudStorage/OneDrive-WashingtonandLeeUniversity/WVS_Time_Series_1981-2022_Rdata_v5_0 2.rdata")
load("~/OneDrive - Washington and Lee University/SRS 2024/migration_urban_FINAL.rdata")


migration_urban <- `migration_urban`
HouseholdSize <- read_csv("~/Downloads/HouseholdSize.csv", show_col_types = FALSE)
Homicide <- read_csv("~/Downloads/Homicide.csv", show_col_types = FALSE)
dens_pov_hdi_gdp <- read_csv("~/Downloads/all_dens_pov_hdi_gdp_clean (1).csv", show_col_types = FALSE)
All_Gini <- read_csv("~/Downloads/All_Gini_EE_cleaned.csv",  show_col_types = FALSE)
HIEF_all <- read_csv("~/Downloads/HIEF_all.csv",   show_col_types = FALSE)
RDI <- read_csv("~/Downloads/RDI_cleaned.csv", show_col_types = FALSE)

migration_urban$survey_year <- as.numeric(migration_urban$survey_year)
#test of left joining the external data frames from others 
mig_urb_hom<-  Homicide %>%
left_join(migration_urban, by = c("Country Name" ="Country Name", "Year" = "survey_year"))


mig_urban_hom_house <- left_join(mig_urb_hom, HouseholdSize, by = c("Country Name" = "Country", "Year" = "Year"))

outside_pst <- left_join(mig_urban_hom_house, dens_pov_hdi_gdp, by = c("Country Name" = "Country", "Year" = "Year"), relationship = "many-to-many")

outside_pstz <- left_join(outside_pst, All_Gini, by = c("Country Name" = "Country Name", "Year" = "Year"), relationship = "many-to-many")

outside_pstzt <- left_join(outside_pstz, HIEF_all, by = c("Country Name" = "Country", "Year" = "Year"), relationship = "many-to-many" )

outside_pstzt_final <-left_join(outside_pstzt, RDI, by = c("Country Name" = "country_name", "Year" = "year"), relationship = "many-to-many" ) 
outside_pstzt_final <- outside_pstzt_final %>% select(-"country_code") %>% select(- "Country Code.x")



save(outside_pstzt_final, file = "outside_sources_final.rdata")  
  
  
  