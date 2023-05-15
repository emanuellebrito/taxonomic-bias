library(tidyverse)

#read data
setwd("C:/Users/emanu/Dropbox (Personal)/Doutorado - Emanuelle/Cap 2 - Taxonomic bias/data")

all <- read.csv("All_Data_May_2023.csv",  sep=";", dec = ",")

all.data_a <- all %>%
  group_by(Animal_Taxonomic_level, Data_Status)%>%
  summarise(all = n())

all.data_b <- all.data_a %>%
  arrange(Animal_Taxonomic_level) %>%
  group_by(Data_Status) %>%
  mutate(pct_change = (all - lag(all))/lag(all) * 100)

#Plot created to show % change
ggplot(all.data_b, mapping=aes(x=Animal_Taxonomic_level, 
                           y=pct_change)) +
  geom_col()

#number format changed to show Y axis as %
ggplot(all.data_b, mapping=aes(x=Animal_Taxonomic_level, 
                           y=pct_change)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent)


###########################
library(ggalluvial)

all.data_a <- all.data_a[all.data_a$Animal_Taxonomic_level != "0", ]
all.data_a <- all.data_a[all.data_a$Animal_Taxonomic_level != "#N/A", ]
all.data_a <- all.data_a[all.data_a$Animal_Taxonomic_level != "phylum", ]

is_lodes_form(alluvial_data, key = "Animal_Taxonomic_level", value = "all", id = "Data_Status")

# Restructure the data into the lodes format
alluvial_data <- all.data_a %>%
  group_by(Animal_Taxonomic_level, Data_Status) %>%
  summarise(all = sum(all)) %>%
  ungroup()

dat_ggalluvial <- df_result

df_pct <- df %>%
  mutate(Original_Percent = Original / sum(Original) * 100,
         Reused_Percent = Reused / sum(Reused) * 100)

df_pct_ori <- df_pct %>%
 select(Animal_Taxonomic_level, Original, Original_Percent) %>%
  mutate(Subject = 1:9) %>%
  mutate(Bar = "Original") %>%
  rename(Percent = Original_Percent)

df_pct_reu <- df_pct %>%
  select(Animal_Taxonomic_level, Reused, Reused_Percent) %>%
  mutate(Subject = 1:9) %>%
  mutate(Bar = "Reused")%>%
  rename(Percent = Reused_Percent)

################################

animal_taxonomic_level <- c('class', 'superfamily', 'family', 'NER', 'order', 'tribe', 'genus', 'phylum', 'subfamily')
original_percent <- c(63.354037, 13.664596, 8.074534, 5.590062, 3.726708, 2.484472, 1.863354, 0.621118, 0.621118)
reused_percent <- c(61.0662359, 3.3925687, 9.9623048, 22.4555735, 0.4846527, 0.1615509, 0.1615509, 1.7770598, 0.538503)

# Create data frame
data <- data.frame(
  Animal_Taxonomic_level = animal_taxonomic_level,
  Difference = original_percent - reused_percent
)

# Plot
ggplot(data, aes(x = Animal_Taxonomic_level, y = Difference, fill = Difference > 0)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = c("#FF0000", "#00FF00")) +
  coord_flip() +
  labs(x = "Animal_Taxonomic_level", y = "Difference", title = "Difference between Original_Percent and Reused_Percent") +
  theme_minimal()

##################################################
colnames(df_pct_ori) <- colnames(df_pct_reu)
sankey_data_net <- rbind(df_pct_ori, df_pct_reu)

sankey_data_net = rbind(df_pct_ori, df_pct_reu) %>%
  mutate(Bar = factor(Bar, levels = c("Original", "Reused"))) %>%
  mutate(label_manual= paste0(round(Percent),"%"))


ggplot(sankey_data_net,
       aes(x = Bar, stratum = Animal_Taxonomic_level, alluvium = Subject,
           y = Percent,
           fill = Animal_Taxonomic_level, label = Animal_Taxonomic_level)) +
  scale_x_discrete(expand = c(.1, .1)) +
  ggalluvial::geom_flow() +
  ggalluvial::geom_stratum(alpha = .7) +
  geom_text(stat = "stratum", size = 4,
            aes(label = scales::percent(after_stat(prop), accuracy = 1))) +
  #ggtitle("Delinquency: Title holders vs. amount unpaid", subtitle = "Paraná State") +
  xlab("") +
  ylab("Percent (%)") +
  theme_minimal() +
  #theme(legend.position = "none") +
  guides(fill=guide_legend(title="Taxonomic group")) +
  scale_fill_brewer(type = "qual", palette = "RdBu", direction = -1) 
  
  
  