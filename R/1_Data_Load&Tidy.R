# Magellan Strait food web: Load & tidy data
# Data: September 2023 - January 2025
# Author: Tomas I. Marina


# Load packages -----------------------------------------------------------
require(tidyverse)
require(janitor)


# Load data ---------------------------------------------------------------
beagle_df <- read.csv(file = "Data/BeagleChannel_links.csv", header = TRUE)
magellan_raw <- read.csv(file = "Data/MagellanStrait_links_13feb24.csv", header = TRUE) %>% 
  dplyr::select(resource_original, consumer, resource_collapsed) %>% 
  mutate(across(where(is.character), str_trim)) %>% 
  dplyr::select(resource_collapsed, consumer) %>% 
  rename(resource = resource_collapsed)

## Check duplicated interactions
beagle_dup <- beagle_df %>% 
  janitor::get_dupes(resource, consumer)
magellan_dup <- magellan_raw %>% 
  janitor::get_dupes(resource, consumer)
magellan_df <- magellan_raw %>%
  distinct()


# Sim & Diff --------------------------------------------------------------
## Interactions
shared_links <- dplyr::inner_join(magellan_df, beagle_df)
beagle_ulinks <- dplyr::anti_join(beagle_df, magellan_df)
magellan_ulinks <- dplyr::anti_join(magellan_df, beagle_df)

## Species
### Beagle
b_res <- unique(beagle_df$resource)
b_con <- unique(beagle_df$consumer)
b_sp <- as.data.frame(c(b_res, b_con)) %>% 
  distinct() %>% 
  rename(., Species = "c(b_res, b_con)")
### Magellan
m_res <- unique(magellan_df$resource)
m_con <- unique(magellan_df$consumer)
m_sp <- as.data.frame(c(m_res, m_con)) %>% 
  distinct() %>% 
  rename(., Species = "c(m_res, m_con)")

shared_sp <- dplyr::inner_join(m_sp, b_sp)
beagle_usp <- dplyr::anti_join(b_sp, m_sp)
magellan_usp <- dplyr::anti_join(m_sp, b_sp)


# Save data ---------------------------------------------------------------
save(beagle_df, magellan_df, b_sp, m_sp,
     file = "Results/Data_tidy_13feb24.rda")
