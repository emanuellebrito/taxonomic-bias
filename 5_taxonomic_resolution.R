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
    names[[i]] <- str_remove(row_names[[i]], "\\s(sp\\S+)$")
    
    # Classify row names according to taxonomic levels
    taxonomic_levels <- case_when(
      str_detect(names[[i]], "Unidentified") ~ "Unidentified",
      str_detect(names[[i]], "^era$") ~ "Order",
      str_detect(names[[i]], "oidea$") ~ "Superfamily",
      str_detect(names[[i]], "dae$") ~ "Family",
      str_detect(names[[i]], "inae$") ~ "Subfamily",
      str_detect(names[[i]], "ini$") ~ "Tribe",
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
subset <- TAXON.RESOLUTION(diretorio = "C:/Users/emanu/Dropbox (Personal)/Doutorado - Emanuelle/Cap 1 - Scientometric/data/subset" , ext = ".csv")  
