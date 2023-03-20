rm(list=ls())
library(tidyverse)
library(rgdal)
library(maptools)
library(dplyr)

#read data
setwd("C:/Users/emanu/Dropbox (Personal)/Doutorado - Emanuelle/Cap 2 - Inclusion criteria/data")

#setwd("taxonomic-bias")

webs <- read.csv("Scientiometric_Data_March_2023.csv",  sep=";")

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



