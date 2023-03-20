rm(list=ls())
library(tidyverse)
library(rgdal)
library(maptools)
library(dplyr)

#read data
setwd("C:/Users/emanu/Dropbox (Personal)/Doutorado - Emanuelle/Cap 2 - Inclusion criteria/data")

#setwd("taxonomic-bias")

webs <- read.csv("Scientiometric_Data_March_2023.csv",  sep=";")
