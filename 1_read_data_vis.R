rm(list=ls())
library(tidyverse)
library(rgdal)
library(maptools)
library(dplyr)
library(ggeffects)
library(nlme)

#read data
setwd("C:/Users/emanu/Dropbox (Personal)/Doutorado - Emanuelle/Cap 2 - Taxonomic bias/data")

#setwd("taxonomic-bias")

webs <- read.csv("Scientiometric_Data_May_2023.csv",  sep=";", dec = ",")
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
       y = "Published networks") +
  geom_col(fill = "#0099f9") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.7)) +
  scale_x_discrete(labels=c("1920-1930", "1960-1969", "1970-1975", "1976-1980", "1981-1985",
                            "1986-1990", "1991-1995", "1996-2000", "2001-2005", "2006-2010", 
                            "2011-2015", "2016-2020", "2021")) +
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

#improving plot
taxa_groups_A %>%
  arrange(count) %>%
  mutate(Animal_Taxonomic_level = factor(Animal_Taxonomic_level, 
                                         levels=c("phylum", "class", "order", "superfamily", 
                                                  "family", "subfamily", "tribe", "genus", 
                                                  "NER"))) %>% 
  ggplot(aes(x = Animal_Taxonomic_level, y = count)) +
  geom_bar(stat = "identity") +
  labs(x = "Animal Taxon",
       y = "Published networks") +
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


### networks number x animal taxa groups (ANIMALS)
webs %>% 
  drop_na(Animal_taxon) %>%
  group_by(Animal_taxon) %>% 
  summarize(count = length(Animal_taxon)) %>%
  ggplot(aes(x = Animal_taxon, y = count)) +
  geom_bar(stat = "identity") 

animal_groups <- webs %>%
  drop_na(Animal_taxon) %>% 
  group_by(Animal_Taxonomic_level, Animal_taxon) %>% 
  summarize(count = length(Animal_taxon))

filtered_data <- animal_groups %>% filter(count > 3)

# stacked barplot with multiple groups
filtered_data %>%
  arrange(count) %>%
  mutate(Animal_Taxonomic_level = factor(Animal_Taxonomic_level, 
                                         levels=c("NER", "phylum", "class", "order", "subfamily", 
                                                  "superfamily", "family", "tribe", "genus"))) %>%
  ggplot(aes(x = count, y = Animal_Taxonomic_level, fill = Animal_taxon)) +
  geom_bar(stat = "identity") +
  labs(x = "Published networks",
       y = "Animal Taxonomic Level") +
  theme_minimal()

#simple barplot only with animal taxa
ggplot(data = filtered_data, aes(x = count, y = reorder(Animal_taxon, -count))) +
  geom_bar(stat = "identity") +
  xlab("Published networks") +
  ylab("Animal Taxonomic Group") +
  theme_minimal()


### networks number x plant taxa groups (PLANTS)
webs %>% 
  drop_na(Plant_taxon) %>%
  group_by(Plant_taxon) %>% 
  summarize(count = length(Plant_taxon)) %>%
  ggplot(aes(x = Plant_taxon, y = count)) +
  geom_bar(stat = "identity") 

plant_groups <- webs %>%
  drop_na(Plant_taxon) %>% 
  group_by(Plant_Taxonomic_level, Plant_taxon) %>% 
  summarize(count = length(Plant_taxon))

filtered_data2 <- plant_groups %>% filter(count > 2)

###plants has a very reduced number of taxonomic groups###

####### anual study count fo the most published animal taxa
#grouping the 3 most published animal taxa
animal_year <- webs %>%
  drop_na(Animal_taxon) %>% 
  mutate(Animal_taxon_group = case_when(
    Animal_taxon %in% c("Insecta", "Apoidea", "Trochilidae") ~ Animal_taxon,
    TRUE ~ "Other")) %>%
  group_by(Publi_Year, Animal_taxon_group) %>% 
  summarize(count = n())

#### test 
#testing the assumptions
ay_glm <- glm(count ~ Animal_taxon_group * Publi_Year, data = animal_year, family = poisson)
plot(ay_glm)
summary(ay_glm)

library(car)
vif(ay_glm)

library(broom)
my_data_augmented <- augment(ay_glm)
ggplot(my_data_augmented, aes(x = .fitted, y = .resid)) + 
  geom_point() +
  xlab("Predicted Values") +
  ylab("Residuals") +
  ggtitle("GLM Residuals vs. Predicted Values")

#plotting
ggplot(animal_year, aes(x = Publi_Year, y = count, col = Animal_taxon_group)) +
  geom_point() +
  stat_smooth(method = "glm", formula = count ~ Animal_taxon_group * Publi_Year, se = FALSE) +
  facet_wrap(~ Animal_taxon_group)

ggplot(animal_year, aes(x = Publi_Year, y = count, col = Animal_taxon_group)) +
  geom_point() +
  geom_smooth(method = "glm", formula = count ~ Publi_Year + Animal_taxon_group, se = F)


# Predict marginal effects for plotting
pred <- ggpredict(ay_glm, terms = c("Publi_Year", "Animal_taxon_group"), type = "fe")
ggplot(pred, aes(x = x, y = predicted, color = factor(group))) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ group)

predlm <- predict(ay_glm, newdata = animal_year)
animal_year$predlm <- predlm

ggplot(animal_year, aes(x = Publi_Year, y = count, col = Animal_taxon_group)) +
  geom_point() +
  geom_line(aes(y = predlm), size = 1)

# Plotting the real graph for each group
# Create a new column in animal_year to match the animal taxon group with the predicted data
animal_year$group <- factor(animal_year$Animal_taxon_group, levels = levels(pred$group))

# Original plot with points and facets
plot1 <- ggplot(animal_year, aes(x = Publi_Year, y = count, col = Animal_taxon_group)) +
  geom_point() +
  facet_wrap(~ Animal_taxon_group)

# Second plot with predicted values and facets
plot2 <- ggplot(pred, aes(x = x, y = predicted, color = factor(group))) +
  geom_line() +
  facet_wrap(~ group)

# Combine the two plots using the + operator
combined_plot <- plot1 +
  geom_line(aes(x = x, y = predicted, color = factor(group)), data = pred, size = 1) +
  theme_minimal()+
  theme(legend.position = "none") +  # Remove legend from the first plot
  labs(x = "Year",
       y = "Published networks") +
  facet_wrap(~ group, scales = "free_y")  # Create a separate facet for each group

# Display the combined plot
combined_plot















