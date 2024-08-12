library(tidyverse)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)


#world bank data poverty rates
wb_pop_dens <- read_excel("~/Desktop/JE SRS 24/global project/world_bank_pop_dens.xls", sheet = 1)

#data cleaning
#dropping first 2 rows that are just blank '
wb_pop_dens <- wb_pop_dens[-c(1, 2), ]

#renaming column names
colnames(wb_pop_dens) <- as.character(wb_pop_dens[1, ])
wb_pop_dens <- wb_pop_dens[-1, ]
wb_pop_dens <- wb_pop_dens %>% rename(Country = "Country Name")

wb_pop_long <- wb_pop_dens %>% 
  pivot_longer(cols = "1960":"2023", 
               names_to = "Year", 
               values_to = "Pop_dens")

wb_pop_long <- wb_pop_long %>% 
  mutate(Country = case_when(
    Country == "Egypt, Arab Rep" ~ "Egypt",
    Country == "Hong Kong SAR, China" ~ "Hong Kong SAR",
    Country == "Iran, Islamic Rep." ~ "Iran",
    Country == "Kyrgyz Republic" ~ "Kyrgyzstan",
    Country == "Korea, Rep." ~ "South Korea",
    Country == "Macao SAR, China" ~ "Macao SAR",
    Country == "Myanmar(Burma)" ~ "Myanmar",
    Country == "West Bank and Gaza" ~ "Palestine",
    Country == "Slovak Republic" ~ "Slovakia",
    Country == "Russian Federation" ~ "Russia",
    Country == "Turkiye" ~ "Turkey",
    Country == "Venezuela, RB" ~ "Venezuela",
    Country == "Viet Nam" ~ "Vietnam",
    Country == "Yemen, Rep." ~ "Yemen",
    Country == "Trinidad and Tobago" ~ "Trinidad & Tobago",
    Country == "Bosnia and Herzegovina" ~ "Bosnia & Herzegovina",
    TRUE ~ Country))

wb_pop_long <- wb_pop_long %>% select(-"Indicator Name", -"Indicator Code")

glimpse(wb_pop_long)
#creating csv
write.csv(wb_pop_long, "wb_pop_dens.csv", row.names = FALSE)

#world bank data
wb_pov_rates <- read_excel("~/Desktop/JE SRS 24/global project/world_bank_poverty_rates.xls", sheet = 1)
View(wb_pov_rates)

#data cleaning
#dropping first 2 rows that are just blank
wb_pov_rates <- wb_pov_rates[-c(1, 2), ]
#View(wb_pov_rates)

#renaming column names
#Income share held by lowest 10%
#Income share held by lowest 20%
#Poverty headcount ratio at $2.15 a day (2017 PPP) (% of population)
colnames(wb_pov_rates) <- as.character(wb_pov_rates[1, ])
wb_pov_rates <- wb_pov_rates[-1, ]
View(wb_pov_rates)

#wb_pov_rates <- wb_pov_rates %>% rename(Country = "Country Name")
#wb_pov_rates <- wb_pov_rates %>% as.numeric(Country = "Country Name")

#3 dataframes
pov_2D <- wb_pov_rates %>% filter(`Indicator Name` == "Poverty headcount ratio at $2.15 a day (2017 PPP) (% of population)")
pov_10 <- wb_pov_rates %>% filter(`Indicator Name` == "Income share held by lowest 10%")
pov_20 <- wb_pov_rates %>% filter(`Indicator Name` == "Income share held by lowest 20%")

pov_2D_long <- pov_2D %>% 
  pivot_longer(cols = "1960":"2023", 
               names_to = "Year", 
               values_to = "Poverty Rate")
pov_10_long <- pov_10 %>% 
  pivot_longer(cols = "1960":"2023", 
               names_to = "Year", 
               values_to = "Income of lowest 10%")
pov_20_long <- pov_20 %>% 
  pivot_longer(cols = "1960":"2023", 
               names_to = "Year", 
               values_to = "Income of lowest 20%")

pov_2D_long <- pov_2D_long %>% select(-"Indicator Name", -"Indicator Code")
pov_10_long <- pov_10_long %>% select(-"Indicator Name", -"Indicator Code")
pov_20_long <- pov_20_long %>% select(-"Indicator Name", -"Indicator Code")

#manipulating of data
#wb_pov_rates <- wb_pov_rates %>% select(-"Indicator Name", -"Indicator Code")
#wb_pov_rates <- wb_pov_rates %>% 
 # pivot_longer(cols = "1960":"2023", 
               #names_to = "Year")
#wb_pov_rates <- wb_pov_rates %>% select(-"value")
#wb_pov_rates <- wb_pov_rates %>% group_by(`Country Name`, `Country Code`, `Year`) 

#joining data
new_wb_pov_rates <- left_join(pov_10_long, pov_2D_long, by = c("Country Name", "Year", "Country Code"))
new_wb_pov_rates <- left_join(new_wb_pov_rates, pov_20_long, by = c("Country Name", "Year", "Country Code"))
new_wb_pov_rates <- new_wb_pov_rates %>% rename(Country = "Country Name")

#wb_pov_rates <- wb_pov_rates %>% group_by(`Country Name`, `Year`) 

View(new_wb_pov_rates)

new_wb_pov_rates <- new_wb_pov_rates %>% 
  mutate(Country = case_when(
    Country == "Egypt, Arab Rep" ~ "Egypt",
    Country == "Hong Kong SAR, China" ~ "Hong Kong SAR",
    Country == "Iran, Islamic Rep." ~ "Iran",
    Country == "Kyrgyz Republic" ~ "Kyrgyzstan",
    Country == "Korea, Rep." ~ "South Korea",
    Country == "Macao SAR, China" ~ "Macao SAR",
    Country == "Myanmar(Burma)" ~ "Myanmar",
    Country == "West Bank and Gaza" ~ "Palestine",
    Country == "Slovak Republic" ~ "Slovakia",
    Country == "Russian Federation" ~ "Russia",
    Country == "Turkiye" ~ "Turkey",
    Country == "Venezuela, RB" ~ "Venezuela",
    Country == "Viet Nam" ~ "Vietnam",
    Country == "Yemen, Rep." ~ "Yemen",
    Country == "Trinidad and Tobago" ~ "Trinidad & Tobago",
    Country == "Bosnia and Herzegovina" ~ "Bosnia & Herzegovina",
    TRUE ~ Country))

#creating csv
write.csv(new_wb_pov_rates, "wb_pov_rates.csv", row.names = FALSE)

#undp hdi values
undp_hdi <- read_csv("~/Desktop/JE SRS 24/global project/undp_hdi_data.csv")
View(undp_hdi)

#data cleaning
#renaming column names
undp_hdi <- undp_hdi %>% rename("Country Code" = "iso3")
undp_hdi <- undp_hdi %>% rename("Country" = "country")
undp_hdi <- undp_hdi %>% select(-"hdi_rank_2022")
#View(undp_hdi)

undp_hdi <- undp_hdi %>% 
  mutate(Country = case_when(
    Country == "Eswatini (Kingdom of)" ~ "Eswatini",
    Country == "Hong Kong SAR, China" ~ "Hong Kong SAR",
    Country == "Iran (Islamic Republic of)" ~ "Iran",
    Country == "Kyrgyz Republic" ~ "Kyrgyzstan",
    Country == "Korea, Rep." ~ "South Korea",
    Country == "Korea (Democratic People's Rep. of)" ~ "North Korea",
    Country == "Lao People's Democratic Republic" ~ "Laos",
    Country == "Micronesia (Federated States of)" ~ "Micronesia",
    Country == "Moldova (Republic of)" ~ "Moldova",
    Country == "Palestine, State of" ~ "Palestine",
    Country == "Syrian Arab Republic" ~ "Syria",
    Country == "Tanzania (United Republic of)" ~ "Tanzania",
    Country == "Viet Nam" ~ "Vietnam",
    Country == "T\xfcrkiye" ~ "Turkey",
    Country == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
    Country == "Bolivia (Plurinational State of)" ~ "Bolivia",
    Country == "C\xf4te d'Ivoire" ~ "Ivory Coast",
    Country == "Korea (Republic of)" ~ "South Korea",
    Country == "Trinidad and Tobago" ~ "Trinidad & Tobago",
    Country == "Bosnia and Herzegovina" ~ "Bosnia & Herzegovina",
    TRUE ~ Country))

hdi_long <- undp_hdi %>%
  select(Country, hdi_1990: hdi_2022) %>%
  pivot_longer(
    cols = -c(Country),
    names_to = "hdi_year",
    values_to = "hdi_value"
  ) %>%
  mutate(Year = as.integer(str_extract(hdi_year, "\\d{4}"))) %>%
  select(-hdi_year)
View(hdi_long)

write.csv(hdi_long, "undp_hdi.csv", row.names = FALSE)

#gdp per capita
wb_gdp <- read_excel("~/Desktop/JE SRS 24/global project/world_bank_gdp_per_capita.xls", sheet = 1)

#data cleaning
#dropping first 2 rows that are just blank
wb_gdp <- wb_gdp[-c(1, 2), ]

#renaming column names
colnames(wb_gdp) <- as.character(wb_gdp[1, ])
wb_gdp <- wb_gdp[-1, ]
wb_gdp <- wb_gdp %>% rename(Country = "Country Name")

#putting df in long form
wb_gdp <- wb_gdp %>% 
  pivot_longer(cols = "1960":"2023", 
               names_to = "Year", 
               values_to = "GDP per Capita")

wb_gdp <- wb_gdp %>% 
  mutate(Country = case_when(
    Country == "Egypt, Arab Rep" ~ "Egypt",
    Country == "Hong Kong SAR, China" ~ "Hong Kong SAR",
    Country == "Iran, Islamic Rep." ~ "Iran",
    Country == "Kyrgyz Republic" ~ "Kyrgyzstan",
    Country == "Korea, Rep." ~ "South Korea",
    Country == "Macao SAR, China" ~ "Macao SAR",
    Country == "Myanmar(Burma)" ~ "Myanmar",
    Country == "West Bank and Gaza" ~ "Palestine",
    Country == "Slovak Republic" ~ "Slovakia",
    Country == "Russian Federation" ~ "Russia",
    Country == "Turkiye" ~ "Turkey",
    Country == "Venezuela, RB" ~ "Venezuela",
    Country == "Viet Nam" ~ "Vietnam",
    Country == "Yemen, Rep." ~ "Yemen",
    Country == "Trinidad and Tobago" ~ "Trinidad & Tobago",
    Country == "Bosnia and Herzegovina" ~ "Bosnia & Herzegovina",
    TRUE ~ Country))

wb_gdp <- wb_gdp %>% select(-"Indicator Name",-"Indicator Code")

#creating csv
write.csv(wb_gdp, "wb_gdp.csv", row.names = FALSE)

#creating merged dataset
pop_dens <- read_csv("wb_pop_dens.csv")
pov_rates <- read_csv("wb_pov_rates.csv")
gdp <- read_csv("wb_gdp.csv")
hdi <- read_csv("undp_hdi.csv")

df_final <- pov_rates %>%
  left_join(pop_dens, by = c("Country", "Year"), relationship = "many-to-many") %>%
  left_join(gdp, by = c("Country", "Year"), relationship = "many-to-many") %>%
  left_join(hdi, by = c("Country", "Year"), relationship = "many-to-many")

df_final <- df_final %>% select(-"Country Code.x",-"Country Code.y")
df_final <- df_final %>% select(-"Country Code.x.x",-"Country Code.y.y")
#View(df_final)
write.csv(df_final, "dens_pov_hdi_gdp.csv", row.names = FALSE)

