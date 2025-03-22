# Magellan Strait food web: network & node-level analysis
# Data: September 2023 - January 2025
# Author: Tomas I. Marina


# Load packages -----------------------------------------------------------
require(igraph)
require(multiweb)
require(tidyverse)
require(NetIndices)
require(network)
require(NetworkExtinction)


# Load data ---------------------------------------------------------------
load("Results/Data_tidy_22mar25.rda")


# Network analyses --------------------------------------------------------
## igraph object
magellan_fw <- magellan_df %>% 
  dplyr::select(Prey, Predator)
m_ig <- graph_from_data_frame(magellan_fw, directed = TRUE)

### Connected & disconnected spp: membership
m_comp <- decompose(m_ig, mode = "weak")  # components
comp <- as.data.frame(components(m_ig)["membership"])
#write.csv(comp, file = "Data/Magellan_sp_feb25.csv")


## Network-level ----
### Complexity & Structure ----
m_prop <- bind_cols(calc_topological_indices(m_ig), calc_modularity(m_ig)) %>% mutate(Name = "Magellan Strait")

top_per <- round(m_prop$Top/m_prop$Size*100, 2)  # Percentage of Top Species
bas_per <- round(m_prop$Basal/m_prop$Size*100, 2)  # Percentage of Basal Species
int_per <- 100 - (top_per+bas_per)  # Percentage of Intermediate Species
omn_per <- round(m_prop$Omnivory*100, 2)  # Percentage of Omnivores
mean_tl <- round(m_prop$TLmean, 2)  # Food web mean trophic level

### Degree distribution ----
m_net <- network::as.network(as.matrix(m_ig))
m_dd <- NetworkExtinction::DegreeDistribution(m_net)
m_dd  # plot cumulative degree distribution
dd_best_fit <- m_dd$models[1,]  # best fit: Exponential

### Small-world pattern ----
rnd_g <- lapply(1:1000, function (x) {
  e <- sample_gnm(m_prop$Size, m_prop$Links, directed = TRUE) # create Erdos-Renyi (ER) networks
  # Check that the ER networks has only one connected component
  while(components(e)$no > 1)
    e <- sample_gnm(m_prop$Size, m_prop$Links, directed = TRUE)
  return(e) 
})

sw <- multiweb::calc_swness_zscore(m_ig, nullDist = rnd_g, weights = NA, ncores = 4)
datos_sw <- as.data.frame(sw["da"])

path_length <- round(datos_sw$da.PathLength, 2)  # Food web path length
clus_coef <- round(datos_sw$da.Clustering, 2)  # Food web clustering coefficient


## Node-level ----
### TL & Omn ----
adj_mat <- as_adjacency_matrix(m_ig, sparse = TRUE)
tl <- round(TrophInd(as.matrix(adj_mat)), digits = 3)
#write.csv(tl, file = "Results/Magellan_tl_feb25.csv")
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

## Species properties
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
#write.csv(m_spp_total, file = "Results/Magellan_sp_prop_12feb25.csv")


### Keystone index ----
# Mean of degree, betweenness & closeness
key_ind <- m_spp_total %>% 
  mutate(deg_rank = dense_rank(desc(TotalDegree)),
         btw_rank = dense_rank(desc(Between)),
         clo_rank = dense_rank(desc(Closeness))) %>% 
  mutate(Keystone_ind = (deg_rank + btw_rank + clo_rank)/3,
         Keystone_rank = dense_rank(Keystone_ind))
#write.csv(key_ind, file = "Results/Magellan_sp_prop_22mar25.csv")


# Plot food web -------------------------------------------------------------
plt_fw <- plot_troph_level(m_ig, vertexLabel = F, modules = F, ylab = "Trophic level")


# Save data -----------------------------------------------------------------
save(m_ig, m_dd, m_prop, m_spp_total, key_ind,
     file = "Results/Network&SpProps_22mar25.rda")
