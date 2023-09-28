# Food web analyses
# Data: September 2023
# Author: Tomas I. Marina


# Load packages -----------------------------------------------------------
require(igraph)
require(multiweb)
require(tidyverse)

# Load data ---------------------------------------------------------------
load("Results/Data_tidy.rda")

# Network properties ------------------------------------------------------
## igraph objects
b_ig <- graph_from_data_frame(beagle_df, directed = TRUE)
m_ig <- graph_from_data_frame(magellan_ok, directed = TRUE)

## Complexity & structure (topology)
b_prop <- bind_cols(calc_topological_indices(b_ig), calc_modularity(b_ig)) %>% mutate(Name = "Beagle Channel")
m_prop <- bind_cols(calc_topological_indices(m_ig)) %>% mutate(Name = "Magellan Strait")
all_prop <- bind_rows(b_prop, m_prop) %>% rename(Network = Name)

m_comp <- decompose(m_ig, mode = "weak")  # components
m_comp
