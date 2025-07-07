priors_df <- read.csv("data_processed/kozich_2013/prior_weights/kozich_prior_weights_1.0.csv", 
                      head = TRUE)
species_ground_truth <- priors_df$species

tax_list <- taxize::classification(species_ground_truth, db = "ncbi")

library(tidyverse)
# Step 2: Convert list to tidy data frame
taxonomy_df <- map_dfr(names(tax_list), function(sp) {
  tax <- tax_list[[sp]]
  if (is.null(tax)) return(NULL)
  
  # Convert from long (rank + name) to wide (rank → column)
  tax_wide <- tax %>%
    select(rank, name) %>%
    filter(!duplicated(rank)) %>%  # in case there are repeated ranks
    pivot_wider(names_from = rank, values_from = name)
  
  tax_wide$input_name <- sp  # Add the species name as a column
  return(tax_wide)
})

# Step 3: Reorder columns if desired
taxonomy_df <- taxonomy_df %>%
  select(domain, kingdom, phylum, class, order, family, genus, species)

write.csv(taxonomy_df, file = "data_processed/kozich_2013/ground_truth_taxonomy.csv",
          row.names = FALSE)
