library(tidyverse)
library(rgdal)
library(maptools)
library(dplyr)
library(ggeffects)
library(nlme)
library(car)
library(broom)

# Read data
setwd("C:/Users/emanu/Dropbox (Personal)/Doutorado - Emanuelle/Cap 2 - Taxonomic bias/data")

# Setwd("taxonomic-bias")

webs <- read.csv("Scientiometric_Data_May_2023.csv",  sep=";", dec = ",")
reuse <- read.csv("ReusedData_May_2023.csv",  sep=";", dec = ",")
all_data <- read.csv("All_Data_May_2023.csv",  sep=";", dec = ",")
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
### networks count x year ORIGINAL
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
tiff('figure2a.tif', w=1600, h=900, units="px", res=300, compression = "lzw")
ggplot(years_groups, aes(x = year_group, y = count)) +
  geom_bar(stat = "identity") +
  labs(x = "Years group",
       y = "Published networks") +
  geom_col(fill = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.7)) +
  scale_x_discrete(labels=c("1920-1930", "1960-1969", "1970-1975", "1976-1980", "1981-1985",
                            "1986-1990", "1991-1995", "1996-2000", "2001-2005", "2006-2010", 
                            "2011-2015", "2016-2020", "2021")) +
  geom_hline(yintercept = mean(years_groups$count), linetype = "dashed", size = 1)
dev.off()
### networks count x year REUSED
reuse_summary <- reuse %>% 
  group_by(Reference_Year) %>% 
  summarize(count = length(Reference_Year))

reuse_summary %>%
  ggplot(aes(x = Reference_Year, y = count)) +
  geom_bar(stat = "identity", fill = "black") +
  labs(x = "Year", y = "Reused networks")+
  theme_minimal() +
  geom_hline(yintercept = mean(reuse_summary$count), linetype = "dashed", size = 1)

#organizing sequences by years group
years_groups_r <- reuse %>%
  mutate(year_group = cut(Reference_Year, 
                          breaks = c(seq(2000, 2021, by = 5), Inf))) %>%
  group_by(year_group) %>%
  summarize(count = n())

tiff('figure2b.tif', w=1600, h=900, units="px", res=300, compression = "lzw")
ggplot(years_groups_r, aes(x = year_group, y = count)) +
  geom_bar(stat = "identity") +
  labs(x = "Years group",
       y = "Reused networks") +
  geom_col(fill = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.7)) +
  scale_x_discrete(labels=c("2000-2005", "2006-2010", "2011-2015", "2016-2020")) +
  geom_hline(yintercept = mean(years_groups_r$count), linetype = "dashed", size = 1)
dev.off()


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
  geom_col(fill = "black") +
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
  geom_col(fill = "black") +
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

filtered_data <- animal_groups %>% filter(count > 2)

# stacked barplot with multiple groups ####ORIGINAL
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

#simple barplot only with animal taxa #### ORIGINAL
tiff('figure3a.tif', w=1100, h=1300, units="px", res=300, compression = "lzw")
ggplot(data = filtered_data, aes(x = count, y = reorder(Animal_taxon, -count))) +
  geom_bar(stat = "identity") +
  xlab("Original plant-pollinator networks") +
  ylab("Animal Taxonomic Group") +
  theme_minimal()
dev.off()
#############################################################################
### REUSED
### networks number x animal taxa groups (ANIMALS) #REUSED
reuse %>% 
  drop_na(Animal_taxon) %>%
  group_by(Animal_taxon) %>% 
  summarize(count = length(Animal_taxon)) %>%
  ggplot(aes(x = Animal_taxon, y = count)) +
  geom_bar(stat = "identity") 

animal_groups_reuse <- reuse %>%
  drop_na(Animal_taxon) %>% 
  group_by(Animal_Taxonomic_level, Animal_taxon) %>% 
  summarize(count = length(Animal_taxon))

animal_groups_reuse <- animal_groups_reuse[animal_groups_reuse$Animal_Taxonomic_level != "N/A", ]

filtered_data_reuse <- animal_groups_reuse %>% filter(count > 2)

# stacked barplot with multiple groups ####REUSED
filtered_data_reuse %>%
  arrange(count) %>%
  mutate(Animal_Taxonomic_level = factor(Animal_Taxonomic_level, 
                                         levels=c("NER", "phylum", "class", "order", "subfamily", 
                                                  "superfamily", "family", "tribe", "genus"))) %>%
  ggplot(aes(x = count, y = Animal_Taxonomic_level, fill = Animal_taxon)) +
  geom_bar(stat = "identity") +
  labs(x = "Reused networks",
       y = "Animal Taxonomic Level") +
  theme_minimal()

#simple barplot only with animal taxa #### REUSED
tiff('figure3b.tif', w=1100, h=1300, units="px", res=300, compression = "lzw")
ggplot(data = filtered_data_reuse, aes(x = count, y = reorder(Animal_taxon, -count))) +
  geom_bar(stat = "identity") +
  xlab("Reused plant-pollinator networks") +
  ylab("Animal Taxonomic Group") +
  theme_minimal()
dev.off()


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
  mutate(Animal_taxon = case_when(
    Animal_taxon %in% c("Insecta", "Apoidea", "Trochilidae", "Animalia") ~ Animal_taxon,
    TRUE ~ "Other")) %>%
  group_by(Publi_Year, Animal_taxon) %>% 
  summarize(count = n())

#### test ORIGINAL NETWORKS ####
#testing the assumptions
ay_glm <- glm(count ~ Animal_taxon * Publi_Year, data = animal_year, family = poisson)
plot(ay_glm)
summary(ay_glm)

vif(ay_glm)

my_data_augmented <- augment(ay_glm)
ggplot(my_data_augmented, aes(x = .fitted, y = .resid)) + 
  geom_point() +
  xlab("Predicted Values") +
  ylab("Residuals") +
  ggtitle("GLM Residuals vs. Predicted Values")

###ggeffects
mydf <- ggpredict(ay_glm, terms = "Publi_Year")

ggplot(mydf, aes(x, predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1)

mydf <- ggpredict(ay_glm, terms = c("Publi_Year", "Animal_taxon"))

ggplot(mydf, aes(x = x, y = predicted, colour = group)) +
  geom_line() +
  facet_wrap(~group)


#plotting
ggplot(animal_year, aes(x = Publi_Year, y = count, col = Animal_taxon)) +
  geom_point() +
  stat_smooth(method = "glm", formula = count ~ Animal_taxon * Publi_Year, se = FALSE) +
  facet_wrap(~ Animal_taxon)

ggplot(animal_year, aes(x = Publi_Year, y = count, col = Animal_taxon)) +
  geom_point() +
  geom_smooth(method = "glm", formula = count ~ Publi_Year + Animal_taxon, se = F)


# Predict marginal effects for plotting
pred <- ggpredict(ay_glm, terms = c("Publi_Year", "Animal_taxon"), type = "fe")
ggplot(pred, aes(x = x, y = predicted, color = factor(group))) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ group)

predlm <- predict(ay_glm, newdata = animal_year)
animal_year$predlm <- predlm

ggplot(animal_year, aes(x = Publi_Year, y = count, col = Animal_taxon)) +
  geom_point() +
  geom_line(aes(y = predlm), size = 1)

# Plotting the real graph for each group
# Create a new column in animal_year to match the animal taxon group with the predicted data
animal_year$group <- factor(animal_year$Animal_taxon, levels = levels(pred$group))

# Original plot with points and facets
plot1 <- ggplot(animal_year, aes(x = Publi_Year, y = count, col = Animal_taxon)) +
  geom_point() +
  facet_wrap(~ Animal_taxon)

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
       y = "Predict - Published networks") +
  facet_wrap(~ group, scales = "free_y")  # Create a separate facet for each group

# Display the combined plot
tiff('figure7.tif', w=1400, h=900, units="px", res=300, compression = "lzw")
combined_plot
dev.off()


#plotting with ggeffects
#marginals <- tibble(elements = c("Publi_Year", "Animal_taxon"), fit = list(ay_glm)) %>%
   #mutate(marginal = purrr::map2(fit, elements, ggpredict)) %>%
   #select(-fit) %>%
   #unnest()

pr <- ggpredict(ay_glm, c("Publi_Year", "Animal_taxon"))
tiff('figure7.tif', w=1400, h=1100, units="px", res=300, compression = "lzw")
ggplot(pr, aes(x=x, y=predicted, fill=group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .25) +
  theme_bw() +
  facet_wrap(~group, scales = "free") +
  theme(legend.position = "none")+
  labs(x = "Year",
       y = "Predict (Published networks)") 
dev.off()


####### anual study count fo the most published animal taxa in reused networks
#grouping the 3 most published animal taxa
animal_year_reused <- reuse %>%
  drop_na(Animal_taxon) %>% 
  mutate(Animal_taxon = case_when(
    Animal_taxon %in% c("Insecta", "Animalia", "Trochilidae", "Apoidea") ~ Animal_taxon,
    TRUE ~ "Other")) %>%
  group_by(Reference_Year, Animal_taxon) %>% 
  summarize(count = n())

#### test REUSED NETWORKS ####
#testing the assumptions
ay_glm_reuse <- glm(count ~ Animal_taxon * Reference_Year, data = animal_year_reused, family = poisson)
plot(ay_glm_reuse)
summary(ay_glm_reuse)

vif(ay_glm_reuse)

#plotting with ggeffects
p_reused <- ggpredict(ay_glm_reuse, c("Reference_Year", "Animal_taxon"))
tiff('figure8.tif', w=1400, h=1100, units="px", res=300, compression = "lzw")
ggplot(p_reused, aes(x=x, y=predicted, fill=group)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .25) +
  theme_bw() +
  facet_wrap(~group, scales = "free") +
  theme(legend.position = "none")+
  labs(x = "Year",
       y = "Predict (Published networks)") 
dev.off()



#############################
#Sampling focus

#data by sf
focus.data <- webs %>%
  group_by(Sampling_Focus)%>%
  summarise(webs = n()) %>%
  na.omit() %>%
  mutate(pct = round(webs / sum(webs)*100, 3))

#############################
#Pollinators x Visitors

#data by pollinate detection
pollen.data <- webs %>%
  group_by(Pollinator)%>%
  summarise(webs = n()) %>%
  na.omit() %>%
  mutate(pct = round(webs / sum(webs)*100, 3)) %>%
  arrange(desc(pct))

pollen.yes <- webs %>%
  group_by(Pollinator, Detect.pollinator)%>%
  summarise(webs = n()) %>%
  filter(Pollinator == "Yes") %>%
  na.omit() %>%
  mutate(pct = round(webs / sum(webs)*100, 2)) %>%
  arrange(desc(pct))

# Barplot
ggplot(pollen.data, aes(x=Pollinator, y=webs)) + 
  geom_bar(stat = "identity")

# Barplot
ggplot(pollen.yes, aes(x=Detect.pollinator, y=webs)) + 
  geom_bar(stat = "identity")


ggplot(pollen.yes, aes(fill=Detect.pollinator, x = "", y=webs)) + 
  geom_col() +
  geom_text(aes(label = paste0(pct, "%")), 
            position = position_stack(vjust = 0.5))


#############################
#Taxonomic criteria

#data by taxonomic criteria for animal and plants
taxon.data <- webs %>%
  select(Web_Code, Publi_Year, Publi_Decade, Plant_taxon_criterion., Plant_Taxonomic_level, 
         Animal_taxon_criterion., Animal_Taxonomic_level)%>%
  mutate(Both_taxon_criteria = 
           case_when(Plant_taxon_criterion. == 'yes' & Animal_taxon_criterion. == 'yes' ~ 'Both',
                    Plant_taxon_criterion. == 'no' & Animal_taxon_criterion. == 'no' ~ 'None',
                    Plant_taxon_criterion. == 'yes' & Animal_taxon_criterion. == 'no' ~ 'Plant',
                    Plant_taxon_criterion. == 'no' & Animal_taxon_criterion. == 'yes' ~ 'Animal'))  %>%
  na.omit()


# Plot the chart
ggplot(taxon.data, aes(x=Both_taxon_criteria, y=Publi_Decade)) + 
  geom_boxplot()

#sum data
taxon.data.sum <- taxon.data %>%
  group_by(Both_taxon_criteria)%>%
  summarise(taxon.data = n()) %>%
  na.omit() %>%
  mutate(pct = round(taxon.data / sum(taxon.data)*100, 3)) %>%
  arrange(desc(pct))

ggplot(taxon.data.sum, aes(x=Both_taxon_criteria, y=taxon.data)) + 
  geom_bar(stat = "identity")

