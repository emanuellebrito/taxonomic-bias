# Load packages
library(ggplot2)
library(performance)

#Read data
setwd("C:/Users/emanu/Dropbox (Personal)/Doutorado - Emanuelle/Cap 2 - Taxonomic bias/data")
webs <- read.csv("webs_resolution.csv",  sep=";", dec = ",")

# Data inspection
mean(webs$TotalSolved)
sd(webs$TotalSolved)

no_zero <- webs %>%
  filter(TotalUnsolved > 0)

geo_taxon <- webs %>%
  select(Web_Code, Size, Connectance, NODF, M_Beckett, Animals, Region, Hemisphere, Continent, 
         Realm_WWF, Biome_WWF, TotalUnsolved, TotalSolved)

ggplot(webs, aes(x="", y=TotalSolved)) + 
  geom_boxplot()

ggplot(webs, aes(x=TotalSolved, y=Size)) +
  geom_point()

pairs(~ Size + Connectance + NODF + M_Beckett + Animals + TotalUnsolved, data = no_zero)

# Testing the Unsolved level by Size
summary(no_zero)
var(no_zero$TotalUnsolved)
hist(no_zero$TotalUnsolved)
hist(no_zero$NODF)
mod1 <- glm(TotalUnsolved ~ Size, data = no_zero, family = quasipoisson(link = "log"))

plot(mod1)
check_model(mod1)
summary(mod1)

ggplot(no_zero, aes(x="", y=TotalUnsolved)) + 
  geom_boxplot()

ggplot(no_zero, aes(x=Realm_WWF, y=TotalUnsolved, color=Realm_WWF)) +
  #geom_point()
  geom_boxplot()






