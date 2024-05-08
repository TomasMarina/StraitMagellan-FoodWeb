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
load("Results/Data_tidy_13feb24.rda")


# Network analyses ------------------------------------------------------
## igraph objects
b_ig <- graph_from_data_frame(beagle_df, directed = TRUE)
m_ig <- graph_from_data_frame(magellan_df, directed = TRUE)

### Connected & disconnected spp: membership
m_comp <- decompose(m_ig, mode = "weak")  # components
comp <- as.data.frame(components(m_ig)["membership"])
#write.csv(comp, file = "Data/Magellan_sp_nov23.csv")


## Network-level ----
### Complexity & Structure ----
b_prop <- bind_cols(calc_topological_indices(b_ig), calc_modularity(b_ig)) %>% mutate(Name = "Beagle Channel")
m_prop <- bind_cols(calc_topological_indices(m_ig), calc_modularity(m_ig)) %>% mutate(Name = "Magellan Strait")
all_prop <- bind_rows(b_prop, m_prop) %>% rename(Network = Name)

### Degree distribution ----
b_net <- network::as.network(as.matrix(b_ig))
b_dd <- NetworkExtinction::DegreeDistribution(b_net)
b_dd  # plot cumulative degree distribution, best model: Exponential

m_net <- network::as.network(as.matrix(m_ig))
m_dd <- NetworkExtinction::DegreeDistribution(m_net)
m_dd  # plot cumulative degree distribution, best model: Exponential

### Small-world pattern ----
rnd_g <- lapply(1:100, function (x) {
  e <- sample_gnm(m_prop$Size, m_prop$Links, directed = TRUE) # create Erdos-Renyi (ER) networks
  # Check that the ER networks has only one connected component
  while(components(e)$no > 1)
    e <- erdos.renyi.game(m_prop$Size, m_prop$Links, type = "gnm", directed = TRUE)
  return(e) 
})

sw <- multiweb::calc_swness_zscore(m_ig, nullDist = rnd_g, weights = NA, ncores = 4)
datos_sw <- as.data.frame(sw["da"])


## Node-level ----
### TL & Omn ----
adj_mat <- as_adjacency_matrix(m_ig, sparse = TRUE)
tl <- round(TrophInd(as.matrix(adj_mat)), digits = 3)
#write.csv(tl, file = "Results/Magellan_tl_nov23.csv")
V(m_ig)$TL <- tl$TL
V(m_ig)$Omn <- tl$OI

### Degree ----
V(m_ig)$TotalDegree <- degree(m_ig, mode = "total")
V(m_ig)$InDegree <- degree(m_ig, mode = "in")
V(m_ig)$OutDegree <- degree(m_ig, mode = "out")

### Betweenness ----
V(m_ig)$Btw <- igraph::betweenness(m_ig, directed = TRUE, cutoff = -1)

### Closeness ----
V(m_ig)$Clo <- igraph::closeness(m_ig, mode = "all")

## Node properties
spp_id <- as.data.frame(1:m_prop$Size)
spp_name <- as.data.frame(V(m_ig)$name)
spp_totdegree <- as.data.frame(V(m_ig)$TotalDegree)
spp_indegree <- as.data.frame(V(m_ig)$InDegree)
spp_outdegree <- as.data.frame(V(m_ig)$OutDegree)
spp_btw <- as.data.frame(V(m_ig)$Btw)
spp_cls <- as.data.frame(V(m_ig)$Clo)
spp_tl <- as.data.frame(V(m_ig)$TL)
spp_omn <- as.data.frame(V(m_ig)$Omn)
m_spp_total <- bind_cols(spp_id, spp_name, spp_totdegree, spp_indegree, 
                       spp_outdegree, spp_btw, spp_cls, spp_tl, spp_omn)
colnames(m_spp_total) <- c("ID", "TrophicSpecies", "TotalDegree", 
                         "NumPrey", "NumPred", "Between", "Closeness", "TL", "Omn")
#write.csv(m_spp_total, file = "Results/Magellan_sp_prop_13feb24.csv")


### Keystone index ----
# Mean of degree, betweenness & closeness
key_ind <- m_spp_total %>% 
  mutate(deg_rank = dense_rank(desc(TotalDegree)),
         btw_rank = dense_rank(desc(Between)),
         clo_rank = dense_rank(desc(Closeness))) %>% 
  mutate(Keystone_ind = (deg_rank + btw_rank + clo_rank)/3,
         Keystone_rank = dense_rank(Keystone_ind))
#write.csv(key_ind, file = "Results/Magellan_sp_prop_19feb24.csv")


# Plot food web -----------------------------------------------------------
plt_fw <- plot_troph_level(m_ig, vertexLabel = F, modules = F, ylab = "Trophic level")


# Save data ---------------------------------------------------------------
save(m_ig, m_dd, m_prop, m_spp_total, key_ind,
     file = "Results/Network&SpProps_19feb24.rda")
