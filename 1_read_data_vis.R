rm(list=ls())
library(tidyverse)
library(rgdal)
library(maptools)
library(dplyr)

#read data
setwd("C:/Users/emanu/Dropbox (Personal)/Doutorado - Emanuelle/Cap 2 - Inclusion criteria/data")

#setwd("taxonomic-bias")

webs <- read.csv("Scientiometric_Data_March_2023.csv",  sep=";")
summary(webs)

## ***********************************************
## Useful data to this chapter
## ***********************************************
## webs without countries, nothing we can do here
sum(webs$Country == "")
webs$Country[webs$Country == ""] <- NA
webs$iso3[webs$ISO3 == ""] <- NA
webs <- webs[!is.na(webs$Country),]

## webs without latitude, nothing we can do here
sum(webs$LAT == "")
webs$LAT[webs$LAT == ""] <- NA
webs <- webs[!is.na(webs$LAT),]

#data by realms
realm.data <- webs %>%
  group_by(Realm_WWF)%>%
  summarise(webs = n())

#data by country
country.data <- webs %>%
  group_by(ISO3, Country)%>%
  summarise(webs = n())


##########################
# GRAPHS FOR DATA VISUALISATION
##########################
### networks count x year
attach(webs)
webs %>% group_by(Publi_Year) %>% 
  summarize(count = length(Publi_Year)) %>%
  ggplot(aes(x = Publi_Year, y = count)) +
  geom_bar(stat = "identity") 

#organizing sequences
years_groups <- webs %>%
  mutate(year_group = cut(Publi_Year, 
                          breaks = c(seq(1920, 1969, by = 10), seq(1970, 2021, by = 5), Inf))) %>%
  group_by(year_group) %>%
  summarize(count = n())

#improving plot
ggplot(years_groups, aes(x = year_group, y = count)) +
  geom_bar(stat = "identity") +
  labs(x = "Years group",
       y = "Published networks number") +
  geom_col(fill = "#0099f9") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.7)) +
  scale_x_discrete(labels=c("1920:1930", "1960:1970", "1970:1975", "1975:1980", "1980:1985",
                            "1985:1990", "1990:1995", "1995:2000", "2000:2005", "2005:2010", 
                            "2010:2015", "2015:2020", "2021")) +
  geom_hline(yintercept = mean(years_groups$count), linetype = "dashed", size = 1)

### networks number x taxonomic restriction (ANIMALS)
#counting taxonomic level
webs %>% 
  group_by(Animal_Taxonomic_level) %>% 
  summarize(count = length(Animal_Taxonomic_level)) %>%
  ggplot(aes(x = Animal_Taxonomic_level, y = count)) +
  geom_bar(stat = "identity") 

taxa_groups_A <- webs %>%
  drop_na(Animal_Taxonomic_level) %>% 
  group_by(Animal_Taxonomic_level) %>% 
  summarize(count = length(Animal_Taxonomic_level))
 
taxa_groups_A %>%
  arrange(count) %>%
  mutate(Animal_Taxonomic_level = factor(Animal_Taxonomic_level, 
                                         levels=c("phylum", "class", "order", "subfamily", 
                                                  "family", "superfamily", "tribe", "genus", 
                                                  "NER"))) %>% 
  ggplot(aes(x = Animal_Taxonomic_level, y = count)) +
  geom_bar(stat = "identity") +
  labs(x = "Animal Taxon",
       y = "Published networks number") +
  geom_col(fill = "#0099f9") +
  theme_minimal()


### networks number x taxonomic restriction (PLANTS)
#counting taxonomic level
webs %>% 
  group_by(Plant_Taxonomic_level) %>% 
  summarize(count = length(Plant_Taxonomic_level)) %>%
  ggplot(aes(x = Plant_Taxonomic_level, y = count)) +
  geom_bar(stat = "identity") 

taxa_groups_P <- webs %>%
  drop_na(Plant_Taxonomic_level) %>% 
  group_by(Plant_Taxonomic_level) %>% 
  summarize(count = length(Plant_Taxonomic_level))

taxa_groups_P %>%
  filter(Plant_Taxonomic_level != "species") %>%
  arrange(count) %>%
  mutate(Plant_Taxonomic_level = factor(Plant_Taxonomic_level, 
                                         levels=c("family", "genus", "NER"))) %>% 
  ggplot(aes(x = Plant_Taxonomic_level, y = count)) +
  geom_bar(stat = "identity") +
  labs(x = "Plant Taxon",
       y = "Published networks number") +
  geom_col(fill = "#0099f9") +
  theme_minimal()



