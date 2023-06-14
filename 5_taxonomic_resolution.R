######################################################################
# TAXONOMIC RESOLUTION OF NETWORKS DATA
#---------------------------------------------------------------------
# This script computes the taxonomic resolution of web data
# by classifying row names according to taxonomic levels.
# Creator: Emanuelle Brito
######################################################################

# Load necessary libraries
library(stringr)
library(dplyr)

TAXON.RESOLUTION <- function(diretorio, ext) {
  setwd(diretorio)
  dados <- list.files(diretorio, pattern = ext, full.names = TRUE)
  n <- length(dados)
  matriz <- vector("list", n)
  row_names <- vector("list", n)
  names <- vector("list", n)
  count_levels <- vector("list", n)
  
  for (i in 1:n) {
    matriz[[i]] <- read.csv(dados[i], header = TRUE, sep = ";", dec = ",")
    row_names[[i]] <- matriz[[i]][, 1]
    names[[i]] <- gsub("\\s+sp(\\d+)?\\b", "", row_names[[i]])
    
    # Classify row names according to taxonomic levels
    taxonomic_levels <- case_when(
      str_detect(names[[i]], "^\\S+$") & str_detect(names[[i]], "Insecta$") ~ "Order",
      str_detect(names[[i]], "^\\S+$") & str_detect(names[[i]], "Araneida$") ~ "Order",
      str_detect(names[[i]], "^\\S+$") & str_detect(names[[i]], "Unidentified$") ~ "Unidentified",
      str_detect(names[[i]], "^\\S+$") & str_detect(names[[i]], "ptera$") ~ "Order",
      str_detect(names[[i]], "^\\S+$") & str_detect(names[[i]], "oidea$") ~ "Superfamily",
      str_detect(names[[i]], "^\\S+$") & str_detect(names[[i]], "dae$") ~ "Family",
      str_detect(names[[i]], "^\\S+$") & str_detect(names[[i]], "inae$") ~ "Subfamily",
      str_detect(names[[i]], "^\\S+$") & str_detect(names[[i]], "ini$") ~ "Tribe",
      str_detect(names[[i]], "\\b\\S+\\s\\S+\\b") ~ "Species",
      TRUE ~ "Genus"
    )
    
    # Create a data frame with the count of each taxonomic level
    count_levels[[i]] <- list(
      Order = sum(taxonomic_levels == "Order"),
      Superfamily = sum(taxonomic_levels == "Superfamily"),
      Family = sum(taxonomic_levels == "Family"),
      Subfamily = sum(taxonomic_levels == "Subfamily"),
      Tribe = sum(taxonomic_levels == "Tribe"),
      Genus = sum(taxonomic_levels == "Genus"),
      Species = sum(taxonomic_levels == "Species"),
      Unidentified = sum(taxonomic_levels == "Unidentified"),
      Total = sum(!is.na(taxonomic_levels))
    )
  }
  
  # Combine the count matrices into a single matrix
  result_resolution <- do.call(rbind, count_levels)
  
  rownames(result_resolution) <- basename(dados)
  return(result_resolution)
}

# Usage example:
resolution <- TAXON.RESOLUTION(diretorio = "C:/Users/emanu/Dropbox (Personal)/Doutorado - Emanuelle/Cap 2 - Taxonomic bias/data/pollination_webs" , ext = ".csv")  
write.csv(resolution, "webs_resolution.csv")

# Proceed to check/validation
new_dataframe <- data.frame(names = names, taxonomic_levels = taxonomic_levels)

######################################################################
# TAXONOMIC REPRESENTATION BY GROUPS IN NETWORKS
#---------------------------------------------------------------------

TAXON.GROUPS <- function(diretorio, ext) {
  setwd(diretorio)
  dados <- list.files(diretorio, pattern = ext, full.names = TRUE)
  n <- length(dados)
  matriz <- vector("list", n)
  row_names <- vector("list", n)
  all_taxa <- c()
  origin <- c()
  
  for (i in 1:n) {
    matriz[[i]] <- read.csv(dados[i], header = TRUE, sep = ";", dec = ",")
    row_names[[i]] <- matriz[[i]][, 1]
    names <- gsub("\\s+sp(\\d+)?\\b", "", row_names[[i]])
    
    all_taxa <- c(all_taxa, sapply(strsplit(names, " "), "[", 1))
    origin <- c(origin, rep(basename(dados[i]), length(names)))
  }
  
  # Create a data frame with unique values and their frequencies
  taxa <- data.frame(names = unique(all_taxa), stringsAsFactors = FALSE)
  taxa$frequency <- table(all_taxa)[taxa$names]
  
  # Populate the origin column
  origin_vector <- rep(NA, nrow(taxa))
  for (i in 1:nrow(taxa)) {
    origin_names <- origin[all_taxa %in% taxa$names[i]]
    origin_vector[i] <- ifelse(length(origin_names) > 0, paste(unique(origin_names), collapse = ", "), NA)
  }
  taxa$origin <- origin_vector
  
  return(taxa)
}

webs_taxa <- TAXON.GROUPS(diretorio = "C:/Users/emanu/Dropbox (Personal)/Doutorado - Emanuelle/Cap 2 - Taxonomic bias/data/pollination_webs", ext = ".csv")

write.csv(webs_taxa, "webs_taxa.csv")

save(webs_taxa, file = "webs_taxa.RData")