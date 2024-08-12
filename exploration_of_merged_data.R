library(tidyverse)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

#loading in data
all_WVS <- read_csv("~/Desktop/JE SRS 24/global project/FinalizedDataOfficial.csv")
View(all_WVS)

all_WVS <- distinct(all_WVS)

colnames(all_WVS)

#pop dens
ggplot(all_WVS, aes(x = Pop_dens)) +
  geom_histogram(bins = 100) +
  ggtitle("Histogram of Population Density") +
  xlab("Value") +
  ylab("Frequency") +
  theme_minimal()

#gdp 
ggplot(all_WVS, aes(x = `GDP per Capita`)) +
  geom_histogram() +
  ggtitle("Histogram of GDP per Capita") +
  xlab("Value") +
  ylab("Frequency") +
  theme_minimal()

#poverty rate
ggplot(all_WVS, aes(x = `Poverty Rate`)) +
  geom_histogram() +
  ggtitle("Histogram of Poverty Rates ($2.15 a Day)") +
  xlab("Value") +
  ylab("Frequency") +
  theme_minimal()

#hdi 
ggplot(all_WVS, aes(x = `hdi_value`)) +
  geom_histogram() +
  ggtitle("Histogram of HDI") +
  xlab("Value") +
  ylab("Frequency") +
  theme_minimal()

#low income 10 
ggplot(all_WVS, aes(x = `Income of lowest 10%`)) +
  geom_histogram() +
  ggtitle("Histogram of the Income of lowest 10%") +
  xlab("Value") +
  ylab("Frequency") +
  theme_minimal()

#low income 20 
ggplot(all_WVS, aes(x = `Income of lowest 20%`)) +
  geom_histogram() +
  ggtitle("Histogram of the Income of lowest 20%") +
  xlab("Value") +
  ylab("Frequency") +
  theme_minimal()

