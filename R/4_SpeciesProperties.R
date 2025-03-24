# Magellan Strait food web: all species-level properties
# Data: March 2025
# Author: Tomas I. Marina

# Packages ----------------------------------------------------------------
require(tidyverse)


# Load data ---------------------------------------------------------------
load("Results/Network&SpProps_22mar25.rda")
load("Results/Modularity_22mar25.rda")


# Merge species properties ------------------------------------------------
clas.role.ok <- clas.role %>% 
  rename(TrophicSpecies = name) %>% 
  dplyr::select(-node)
all_sp_prop <- key_ind %>% 
  dplyr::select(-ID) %>% 
  left_join(clas.role.ok) %>% 
  rename(TopRole = type)
#write.csv(all_sp_prop, file = "Results/All_sp_prop_22mar25.csv")


# Save results ------------------------------------------------------------
save(all_sp_prop, 
     file = "Results/AllSpProps_24mar25.rda")
