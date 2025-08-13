# Magellan Strait food web: network & node-level analysis
# Date: September 2023 - March 2025
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
## Taxonomic resolution (Genus + Species)
resolution_summary <- sp_list %>%
  filter(Species != "" & !is.na(Species)) %>%
  mutate(Resolution = case_when(
    str_detect(Species, " sp\\.| spp\\.") ~ "Genus",
    str_detect(Species, " ") ~ "Species",
    TRUE ~ "Other"
  )) %>%
  count(Resolution, name = "Count") %>%
  mutate(Percentage = round((Count / sum(Count)) * 100, 2))

combined_percentage <- resolution_summary %>%
  filter(Resolution %in% c("Species", "Genus")) %>%
  summarise(TotalPercentage = sum(Percentage)) %>%
  pull(TotalPercentage)

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
### Taxonomic group ----
sp_fg <- sp_list %>% rename(id = Species)
df_ig <- igraph::as_data_frame(m_ig, 'both')
df_ig$vertices <- df_ig$vertices %>% 
  left_join(sp_fg, c('name' = 'id'))
m_ig_fg <- graph_from_data_frame(df_ig$edges, directed = TRUE, vertices = df_ig$vertices)
m_ig <- m_ig_fg
vertex.attributes(m_ig)

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

### Generality ----
gen.fun <- function(m_ig){
  pred <- degree(m_ig, mode = "in") > 0
  G <- sum(degree(m_ig, mode = "in")[pred] / sum(pred))
  return(G)
}
round(gen.fun(m_ig), 2) # average number of preys per predator

### Vulnerability ----
vul.fun <- function(m_ig){
  prey <- degree(m_ig, mode = "out") > 0
  V <- sum(degree(m_ig, mode = "out")[prey]) / sum(prey)
  return(V)
}
round(vul.fun(m_ig), 2) # average number of predators per prey

### Betweenness ----
V(m_ig)$Btw <- igraph::betweenness(m_ig, directed = TRUE, cutoff = -1)

### Closeness ----
V(m_ig)$Clo <- igraph::closeness(m_ig, mode = "all")

## Species properties
spp_id <- as.data.frame(1:m_prop$Size)
spp_name <- as.data.frame(V(m_ig)$name)
spp_fg <- as.data.frame(V(m_ig)$Group)
spp_totdegree <- as.data.frame(V(m_ig)$TotalDegree)
spp_indegree <- as.data.frame(V(m_ig)$InDegree)
spp_outdegree <- as.data.frame(V(m_ig)$OutDegree)
spp_btw <- as.data.frame(V(m_ig)$Btw)
spp_cls <- as.data.frame(V(m_ig)$Clo)
spp_tl <- as.data.frame(V(m_ig)$TL)
spp_omn <- as.data.frame(V(m_ig)$Omn)
m_spp_total <- bind_cols(spp_id, spp_name, spp_fg, spp_totdegree, spp_indegree, 
                       spp_outdegree, spp_btw, spp_cls, spp_tl, spp_omn)
colnames(m_spp_total) <- c("ID", "TrophicSpecies", "Group", "TotalDegree", 
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
save(m_ig, m_prop, m_dd, datos_sw, m_spp_total, key_ind,
     file = "Results/Network&SpProps_22mar25.rda")
