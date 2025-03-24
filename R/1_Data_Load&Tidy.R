# Magellan Strait food web: Load & tidy data
# Date: September 2023 - March 2025
# Author: Tomas I. Marina


# Load packages -----------------------------------------------------------
require(tidyverse)
require(janitor)


# Load data ---------------------------------------------------------------
magellan_raw <- read.csv(file = "Data/MagellanStrait_links_22mar25.csv", header = TRUE)

## Check duplicated interactions
magellan_dup <- magellan_raw %>% 
  janitor::get_dupes(Prey, Predator)
magellan_df <- magellan_raw %>% 
  dplyr::select(Prey, Taxonomic_group_prey, Predator, Taxonomic_group_pred, Reference, Reference_link) %>% 
  distinct()


# Species list ------------------------------------------------------------
# Transforming the data
unique_sp_prey <- magellan_df %>% 
  dplyr::select(Prey, Taxonomic_group_prey) %>% 
  rename(Species = Prey, Group = Taxonomic_group_prey) %>% 
  distinct()
unique_sp_pred <- magellan_df %>% 
  dplyr::select(Predator, Taxonomic_group_pred) %>% 
  rename(Species = Predator, Group = Taxonomic_group_pred) %>% 
  distinct()
sp_list <- full_join(unique_sp_prey, unique_sp_pred) %>% 
  mutate(across(where(is.character), str_trim)) %>% 
  distinct()


# Save data ---------------------------------------------------------------
save(magellan_df, sp_list,
     file = "Results/Data_tidy_22mar25.rda")
