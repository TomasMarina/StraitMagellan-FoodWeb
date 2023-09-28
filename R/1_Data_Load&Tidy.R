# Load & tidy data
# Data: September 2023
# Author: Tomas I. Marina


# Load packages -----------------------------------------------------------
require(tidyverse)
require(janitor)


# Load data ---------------------------------------------------------------

beagle_df <- read.csv(file = "Data/BeagleChannel_links.csv", header = TRUE)
magellan_df <- read.csv(file = "Data/MagellanStrait_links.csv", header = TRUE)
## Check duplicated interactions
beagle_dup <- read.csv(file = "Data/BeagleChannel_links.csv", header = TRUE) %>% 
  janitor::get_dupes(resource, consumer)
magellan_dup <- read.csv(file = "Data/MagellanStrait_links.csv", header = TRUE) %>% 
  janitor::get_dupes(resource, consumer)
magellan_ok <- magellan_df %>%
  distinct()


# Sim & Diff --------------------------------------------------------------

## Interactions
shared_links <- dplyr::inner_join(magellan_ok, beagle_df)
beagle_ulinks <- dplyr::anti_join(beagle_df, magellan_ok)
magellan_ulinks <- dplyr::anti_join(magellan_ok, beagle_df)

## Species
### Beagle
b_res <- unique(beagle_df$resource)
b_con <- unique(beagle_df$consumer)
b_sp <- as.data.frame(c(b_res, b_con)) %>% 
  distinct() %>% 
  rename(., Species = "c(b_res, b_con)")
### Magellan
m_res <- unique(magellan_ok$resource)
m_con <- unique(magellan_ok$consumer)
m_sp <- as.data.frame(c(m_res, m_con)) %>% 
  distinct() %>% 
  rename(., Species = "c(m_res, m_con)")

shared_sp <- dplyr::inner_join(m_sp, b_sp)
beagle_usp <- dplyr::anti_join(b_sp, m_sp)
magellan_usp <- dplyr::anti_join(m_sp, b_sp)


# Save data ---------------------------------------------------------------
save(beagle_df, magellan_ok, b_sp, m_sp,
     file = "Results/Data_tidy.rda")

