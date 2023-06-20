# Load packages
library(rgbif)
library(vegan)
library(dplyr)

setwd("D:/Dropbox (Personal)/Doutorado - Emanuelle/Cap 2 - Taxonomic bias/data/pollination_webs")

# Reading dataset
web <- read.csv("kato_et_al_1993.csv", header = TRUE, sep = ";", dec = ",")
filtered_matrix <- web[!grepl("Unidentified", web[, 1]), ]
web <- filtered_matrix
row_names <- web[, 1]
#row_names <- row_names[!grepl("Unidentified", row_names)]
#names <- gsub("\\s+sp(\\d+)?\\b", "", row_names)


# Function to retrieve taxonomic information
get_taxonomic_info <- function(species_name) {
  # Search for the species
  species_result <- name_backbone(name = species_name, limit = 1, kingdom = "Animalia")
  
  # Extract the taxonKey
  if (length(species_result$usageKey) > 0) {
    taxon_key <- species_result$usageKey
    
    # Retrieve taxonomic information for the taxonKey
    taxon_info <- name_usage(key = taxon_key)
    
    # Extract relevant taxonomic ranks
    ID <- species_name
    species <- ifelse(is.null(taxon_info$data$species), NA, taxon_info$data$species)
    genus <- ifelse(is.null(taxon_info$data$genus), NA, taxon_info$data$genus)
    family <- ifelse(is.null(taxon_info$data$family), NA, taxon_info$data$family)
    order <- ifelse(is.null(taxon_info$data$order), NA, taxon_info$data$order)
    class <- ifelse(is.null(taxon_info$data$class), NA, taxon_info$data$class)
    phylum <- ifelse(is.null(taxon_info$data$phylum), NA, taxon_info$data$phylum)
    
    # Create a new dataframe with the taxonomic information
    taxonomic_df <- data.frame(ID = ID, Species = species, Genus = genus, Family = family, Order = order, Class = class, Phylum = phylum)
    
    return(taxonomic_df)
  } else {
    cat("Species not found.\n")
    return(NULL)
  }
}

# Create an empty dataframe to store the taxonomic information
taxonomic_df <- data.frame(ID = character(),
                           Species = character(),
                           Genus = character(),
                           Family = character(),
                           Order = character(),
                           Class = character(),
                           Phylum = character(),
                           stringsAsFactors = FALSE)

# Iterate over each species name
for (i in 1:length(row_names)) {
  species_name <- row_names[i]
  
  # Get taxonomic information for the species
  taxonomic_info <- get_taxonomic_info(species_name)
  
  if (!is.null(taxonomic_info)) {
    # Create a new row with species name and taxonomic information
    new_row <- data.frame(ID = taxonomic_info$ID,
                          Species = taxonomic_info$Species,
                          Genus = taxonomic_info$Genus,
                          Family = taxonomic_info$Family,
                          Order = taxonomic_info$Order,
                          Class = taxonomic_info$Class,
                          Phylum = taxonomic_info$Phylum,
                          stringsAsFactors = FALSE)
    
    # Append the row to the taxonomic dataframe
    taxonomic_df <- rbind(taxonomic_df, new_row)
  }
}

# Print the resulting taxonomic dataframe
print(taxonomic_df)


# Calculate Taxonomic Diversity and Distinctness
# Transpose the original matrix
tmat <- t(web)
# Set the first row as column names
colnames(tmat) <- tmat[1, ]
# Remove the first row
tmat <- tmat[-1, ]

# Convert tmat to numeric matrix
tmat <- as.matrix(tmat)
tmat <- apply(tmat, 2, as.numeric)


# Set steps for taxonomic weights
taxonomy.weights_p <- c(Base = 10, Species = 15, Genus = 15, Family = 15, Order = 15,
                        Class = 15, Phylum = 15)

sum(taxonomy.weights_p)

# Using Gower distance
taxdist.FD <- FD::gowdis(taxonomic_df, w=taxonomy.weights_p)
dis_WG <- as.matrix(100*taxdist.FD)

# Taxonomic diversity and distinctness indices 
mod <- taxondive(tmat, dis_WG, match.force = F)
mod
summary(mod)
plot(mod)


