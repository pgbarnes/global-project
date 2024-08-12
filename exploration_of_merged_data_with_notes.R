library(tidyverse)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

#loading in data
outside_data <- read_csv("~/Desktop/JE SRS 24/global project/FinalizedDataOfficial.csv")
View(outside_data)

outside_data <- distinct(outside_data)

colnames(outside_data)

#pop dens
ggplot(outside_data, aes(x = Pop_dens)) +
  geom_histogram(bins = 100) +
  ggtitle("Histogram of Population Density") +
  theme_minimal()
df1 <- arrange(outside_data, Pop_dens) %>% select(`Country Name`, `survey_year`, Pop_dens)
df <- arrange(outside_data, desc(Pop_dens)) %>% select(`Country Name`, `survey_year`, `Pop_dens`)
View(df)
View(df1)
#log??

#gdp 
ggplot(outside_data, aes(x = `GDP per Capita`)) +
  geom_histogram() +
  ggtitle("Histogram of GDP per Capita") +
  theme_minimal()
df2 <- arrange(outside_data, `GDP per Capita`) %>% select(`Country Name`, `survey_year`, `GDP per Capita`)
df3 <- arrange(outside_data, desc(`GDP per Capita`)) %>% select(`Country Name`, `survey_year`, `GDP per Capita`)
View(df2)
View(df3)
#find Georgia GDP per capita values 1987-1989
#log??

#poverty rate
ggplot(outside_data, aes(x = `Poverty Rate`)) +
  geom_histogram() +
  ggtitle("Histogram of Poverty Rates ($2.15 a Day)") +
  theme_minimal()
df4 <- arrange(outside_data, `Poverty Rate`) %>% select(`Country Name`, `survey_year`, `Poverty Rate`)
df5 <- arrange(outside_data, desc(`Poverty Rate`)) %>% select(`Country Name`, `survey_year`, `Poverty Rate`)
View(df4)
View(df5)

#hdi 
ggplot(outside_data, aes(x = `hdi_value`)) +
  geom_histogram() +
  ggtitle("Histogram of HDI") +
  theme_minimal()
df6 <- arrange(outside_data, `hdi_value`) %>% select(`Country Name`, `survey_year`, `hdi_value`)
df7 <- arrange(outside_data, desc(`hdi_value`)) %>% select(`Country Name`, `survey_year`, `hdi_value`)
View(df6)
View(df7)


#low income 10 
ggplot(outside_data, aes(x = `Income of lowest 10%`)) +
  geom_histogram() +
  ggtitle("Histogram of the Income of lowest 10%") +
  theme_minimal()
df8 <- arrange(outside_data, `Income of lowest 10%`) %>% select(`Country Name`, `survey_year`, `Income of lowest 10%`)
df9 <- arrange(outside_data, desc(`Income of lowest 10%`)) %>% select(`Country Name`, `survey_year`, `Income of lowest 10%`)
View(df8)
View(df9)

#low income 20 
ggplot(outside_data, aes(x = `Income of lowest 20%`)) +
  geom_histogram() +
  ggtitle("Histogram of the Income of lowest 20%") +
  theme_minimal()
arrange(outside_data, `Income of lowest 20%`) %>% select(`Country Name`, `survey_year`)
df10 <- arrange(outside_data, `Income of lowest 20%`) %>% select(`Country Name`, `survey_year`, `Income of lowest 20%`)
df11 <- arrange(outside_data, desc(`Income of lowest 20%`)) %>% select(`Country Name`, `survey_year`, `Income of lowest 20%`)
View(df10)
View(df11)
