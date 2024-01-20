# Food web analyses
# Data: September 2023 - January 2024
# Author: Tomas I. Marina


# Load packages -----------------------------------------------------------
require(igraph)
require(multiweb)
require(tidyverse)
require(NetIndices)
require(network)
require(NetworkExtinction)


# Load data ---------------------------------------------------------------
load("Results/Data_tidy_20jan24.rda")


# Network analyses ------------------------------------------------------
## igraph objects
b_ig <- graph_from_data_frame(beagle_df, directed = TRUE)
m_ig <- graph_from_data_frame(magellan_df, directed = TRUE)

### Connected & disconnected spp: membership
m_comp <- decompose(m_ig, mode = "weak")  # components
comp <- as.data.frame(components(m_ig)["membership"])
#write.csv(comp, file = "Data/Magellan_sp_nov23.csv")

## Network-level ----
b_prop <- bind_cols(calc_topological_indices(b_ig), calc_modularity(b_ig)) %>% mutate(Name = "Beagle Channel")
m_prop <- bind_cols(calc_topological_indices(m_ig), calc_modularity(m_ig)) %>% mutate(Name = "Magellan Strait")
all_prop <- bind_rows(b_prop, m_prop) %>% rename(Network = Name)

### Degree distribution ----
m_net <- network::as.network(as.matrix(m_ig))
m_dd <- NetworkExtinction::DegreeDistribution(m_net)
m_dd  # plot cumulative degree distribution, best model: PowerLaw

### Trophic coherence

### QSS


## Node-level ----
### Trophic level & omnivory
adj_mat <- as_adjacency_matrix(m_ig, sparse = TRUE)
tl <- round(TrophInd(as.matrix(adj_mat)), digits = 3)
#write.csv(tl, file = "Results/Magellan_tl_nov23.csv")
V(m_ig)$TL <- tl$TL
V(m_ig)$Omn <- tl$OI

### Degree
V(m_ig)$TotalDegree <- degree(m_ig, mode = "total")
V(m_ig)$InDegree <- degree(m_ig, mode = "in")
V(m_ig)$OutDegree <- degree(m_ig, mode = "out")

### Betweenness
V(m_ig)$Btw <- betweenness(m_ig, directed = TRUE, cutoff = -1)

## Node properties
spp_id <- as.data.frame(1:m_prop$Size)
spp_name <- as.data.frame(V(m_ig)$name)
spp_totdegree <- as.data.frame(V(m_ig)$TotalDegree)
spp_indegree <- as.data.frame(V(m_ig)$InDegree)
spp_outdegree <- as.data.frame(V(m_ig)$OutDegree)
spp_btw <- as.data.frame(V(m_ig)$Btw)
#spp_cls <- as.data.frame(V(m_ig)$Close)
spp_tl <- as.data.frame(V(m_ig)$TL)
spp_omn <- as.data.frame(V(m_ig)$Omn)
spp_total <- bind_cols(spp_id, spp_name, spp_totdegree, spp_indegree, 
                       spp_outdegree, spp_btw, spp_tl, spp_omn)
colnames(spp_total) <- c("ID", "TrophicSpecies", "TotalDegree", 
                         "NumPrey", "NumPred", "Between", "TL", "Omn")
# write.csv(spp_total, file = "Results/Magellan_sp_prop_20jan24.csv")
