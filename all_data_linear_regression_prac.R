library(tidyverse)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(car)
library(jtools)
library(modelsummary)
options(scipen = 999)

#loading in data
load("~/Desktop/JE SRS 24/global project/wvs_external.rdata")

write.csv(wvs_external, "wvs_external.csv", row.names = FALSE)

wvs_external <- read.csv("~/SRS_2024/wvs_external.csv")
wvs_external <- wvs_external %>% rename(country_code = COUNTRY_ALPHA.x)

#releveling to make USA the standard
wvs_external$country_name <- forcats::fct_relevel(wvs_external$country_name, "United States")

#getting proportion of trust and making a new column
wvs_external <- wvs_external %>%
  group_by(country_name, survey_year) %>%
  mutate(prop_trust = mean(trust == "Most people can be trusted", na.rm = TRUE)) %>%
  ungroup()

#creating trust dummies          
wvs_external$trust_dummy <- ifelse(wvs_external$trust == "Most people can be trusted", 1, 0)

#trust for all data
m1 <- lm(prop_trust ~ survey_year, data = wvs_external)
ggplot(wvs_external, aes(survey_year, prop_trust)) + geom_point(aes(color = country_name)) +
  geom_smooth(method = "lm", se = T) + 
  labs(title = "Trust by Country Over Time") +
  theme_minimal() + theme(legend.position = "upper right",
                          legend.key.size = unit(0.2, 'cm'),
                          legend.key.height = unit(0.1, 'cm'),
                          legend.key.width = unit(0.1, 'cm'),
                     legend.title = element_text(size = 6),
                     legend.text = element_text(size = 4))

#linear regression of just trust by country and year
modelsummary::modelsummary(m1, stars = T) #higher survery year -> less trust

#individual class status and country levels of trust
glm1 <- glm(prop_trust ~ income, data = wvs_external)
modelsummary(glm1, stars = T, exponentiate = T)

#individual class status and trust
glm2 <- glm(trust_dummy ~ income, data = wvs_external)
modelsummary(glm2, stars = T, exponentiate = T)

#taking other individual level factors into account
glm3 <- glm(trust_dummy ~  income + education + age + sex + city_size + survey_year, data = wvs_external, family = "binomial")
modelsummary(glm3, stars = T, exponentiate = T, fmt = 3)
effect_plot(glm3, pred = "income", interval = TRUE, rug = TRUE) +
  labs(title = "Trust and Economic Status Around the World", y = "Trust Probability", x = "Income Level")

#crPlot(glm3, "survey_year")

#taking country level factors into account
glm4 <- glm(trust_dummy ~  income + country_name + education + age + sex + city_size + survey_year + Pop_dens + Income.of.lowest.10. + Income.of.lowest.20. + Poverty.Rate + GDP.per.Capita + hdi_value, data = wvs_external, family = "binomial")
modelsummary(glm4, stars = T, exponentiate = T, fmt = 3)
effect_plot(glm4, pred = "income", interval = TRUE, rug = TRUE, int.type = "confidence") +
  labs(title = "Trust and Economic Status Around the World", y = "Trust Probability", x = "Income Level")

#poverty rate and trust
glm5 <- glm(trust_dummy ~  Poverty.Rate + country_name, data = wvs_external, family = "binomial")
modelsummary(glm5, stars = T, exponentiate = T, fmt = 3)
effect_plot(glm5, pred = "country_name", interval = TRUE, rug = TRUE) +
  labs(title = "Trust and Poverty Around the World", y = "Trust Probability", x = "Country") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

glm6 <- glm(trust_dummy ~  Poverty.Rate + country_name + education + age + sex + city_size + survey_year + Pop_dens + Income.of.lowest.10. + Income.of.lowest.20. + Poverty.Rate + GDP.per.Capita + hdi_value, data = wvs_external, family = "binomial")
modelsummary(glm6, stars = T, exponentiate = T, fmt = 3)
effect_plot(glm6, pred = "country_name", interval = TRUE, rug = TRUE) +
  labs(title = "Trust and Poverty Around the World", y = "Trust Probability", x = "Country")

mods <- list(glm5, glm6)
modelsummary(mods, stars = T, exponentiate = T, fmt = 3)
#correlation of proportion of low income individuals with trust
country_trust_inc <- wvs_external %>% select(country_name, trust, income)

class_regress <- country_trust_inc %>% group_by(country_name) %>%
  summarise(prop_li = mean(income == "Low", na.rm = T), .groups = "keep")
View(class_regress)

trust_regress <- country_trust_inc %>% group_by(country_name) %>%
  summarise(prop_trust = mean(trust == "Most people can be trusted", na.rm = T), .groups = "keep")

class_trust_regress <- class_regress %>% left_join(trust_regress, by = "country_name") 

m3 <- lm(prop_trust ~ prop_li, data = class_trust_regress)
ggplot(class_trust_regress, aes(x = prop_li, y = prop_trust)) + 
  geom_point(aes(color = country_name)) +
  geom_smooth(method = "lm", se = TRUE) + 
  labs(title = "Proportion Trust by Proportion of Low Income Respondants",
       x = "Proportion of Low Income Respondants",
       y = "Proportion of Trust") +
  theme_minimal() + theme(legend.position = "right",
                          legend.title = element_text(size = 6),
                          legend.text = element_text(size = 4),
                          legend.key.size = unit(0.3, "cm"))

modelsummary(m3, stars = T)


#colonization/colonial history 
unique(wvs_external$country_name)
#need to divide into catergories of 
#1) colonizer 
#2) never colonized
#3) colonized by


