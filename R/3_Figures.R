# Figures
# Data: May 2024
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
load("Results/Network&SpProps_19feb24.rda")



# Network-level analysis --------------------------------------------------

## Plot food web -----------------------------------------------------------
plt_fw <- plot_troph_level(m_ig, vertexLabel = F, modules = F, ylab = "Trophic level")

plot_fw <- plot.igraph(m_ig,
                       vertex.size = degree*0.5,
                       vertex.label = NA,
                       layout = layout_trophic,
                       edge.width = .75,
                       edge.arrow.size = 0.15, edge.curved = 0.3)

### by degree ----
## Calculate trophic level & omnivory
adj_mat <- get.adjacency(m_ig, sparse = FALSE)
tl <- round(TrophInd(adj_mat), digits = 3)
degree <- degree(m_ig, mode = "total")
V(m_ig)$TL <- tl$TL
V(m_ig)$Omn <- tl$OI
V(m_ig)$totdegree <- degree
vertex.attributes(m_ig)

layout_trophic <- matrix(nrow = length(V(m_ig)), ncol = 2)
layout_trophic[, 1] <- runif(length(V(m_ig)))
layout_trophic[, 2] <- tl$TL

### Food web plot by degree
plot_fw <- plot.igraph(m_ig,
                       vertex.size = degree*0.5,
                       vertex.label = NA,
                       layout = layout_trophic,
                       edge.width = .75,
                       edge.arrow.size = 0.15, edge.curved = 0.3)


## Degree distribution -----------------------------------------------------
degree <- as.data.frame(degree(m_ig, mode = "total"))
plot_distdegree <- ggplot(degree, aes(degree[,1])) +
    geom_histogram(stat = "count") +
    labs(x = "Cantidad de interacciones", y = "Frecuencia") +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(face = "bold", size = 16),
          axis.title.y = element_text(face = "bold", size = 16))

### Model fitting ----
m_net <- network::as.network(as.matrix(m_ig))
m_dd <- NetworkExtinction::DegreeDistribution(m_net)


## Generality & Vulnerability ----------------------------------------------

tot.deg <- degree(m_ig, mode = "all")
V(m_ig)$totdegree <- degree(m_ig, mode="all")
out.deg <- degree(m_ig, mode = "out")
V(m_ig)$outdegree <-  out.deg
in.deg <- degree(m_ig, mode = "in")
V(m_ig)$indegree <-  in.deg

# Generality
gen.fun <- function(m_ig){
  pred <- degree(m_ig, mode = "in") > 0
  G <- sum(degree(m_ig, mode = "in")[pred] / sum(pred))
  
  return(G)
}

data_indeg <- as.data.frame(V(m_ig)$indegree) 
data_indeg <- data_indeg %>% 
  filter(V(m_ig)$indegree != 0) %>% 
  rename(n = "V(m_ig)$indegree")
(plot_indeg <- ggplot(data_indeg, aes(x = n)) +
    geom_histogram(stat = "count") +
    labs(x = "Depredadores", y = "Cantidad de presas") +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(face = "bold", size = 16),
          axis.title.y = element_text(face = "bold", size = 16)) +
    annotate("text", x = Inf, y = Inf, label = paste("Presas por depredador = ", round(gen.fun(m_ig),2), sep = ""), 
             size = 6, vjust=1, hjust=1))

# Vulnerability
vul.fun <- function(m_ig){
  prey <- degree(m_ig, mode = "out") > 0
  V <- sum(degree(m_ig, mode = "out")[prey]) / sum(prey)
  
  return(V)
}

data_outdeg <- as.data.frame(V(m_ig)$outdegree) 
data_outdeg <- data_outdeg %>% 
  filter(V(m_ig)$outdegree != 0) %>% 
  rename(n = "V(m_ig)$outdegree")
(plot_outdeg <- ggplot(data_outdeg, aes(x = n)) +
    geom_histogram(stat = "count") +
    labs(x = "Presas", y = "Cantidad de depredadores") +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(face = "bold", size = 16),
          axis.title.y = element_text(face = "bold", size = 16)) +
    annotate("text", x = Inf, y = Inf, label = paste("Depredadores por presa = ", round(vul.fun(m_ig),2), sep = ""), 
             size = 6, vjust=1, hjust=1))



# Species-level analysis --------------------------------------------------

## TL vs Degree ----
plot_sp_tl <- ggplot(m_spp_total, aes(x = reorder(TrophicSpecies, TL), y = TotalDegree)) +
  geom_point() +
  geom_smooth(aes(as.numeric(reorder(TrophicSpecies, TL)), degree), method = "loess") +
  # geom_vline(xintercept = c(spp_nt_12+1, spp_nt_12+spp_nt_13+1, spp_nt_14+1), linetype = "longdash", colour = "red") +
  labs(x = "Species (increasing TL)", y = "Number of interactions") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 18, face = "bold"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 15))

## Prey & Predator int ----
data_plot_sp_tl <- m_spp_total %>% 
  gather(type, count, NumPrey:NumPred) 
#hue_pal()(2)  # get color code
plot_sp_tl_bar <- ggplot(data_plot_sp_tl, aes(x = reorder(TrophicSpecies, TL), y = count, 
                                              fill = forcats::fct_rev(type))) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("#F8766D","#00BFC4"), labels = c("Prey", "Predator")) +
  labs(x = "Species (increasing TL)", y = "Number of interactions") +
  guides(fill = guide_legend(title = "Number of:")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_blank())

# Interactive plot
library(plotly)
pr_pred_int <- m_spp_total %>% 
  filter(TotalDegree > 5) %>%
  plot_ly(., x = ~reorder(TrophicSpecies, -TotalDegree), y = ~NumPrey, type = 'bar', name = 'NumPrey') %>%
  add_trace(y = ~NumPred, name = 'NumPred') %>%
  layout(yaxis = list(title = 'Number of interactions'), 
         xaxis = list(title = 'Species'), barmode = 'stack')

## Centrality indices ----
### Closeness ----
V(m_ig)$closeness <- igraph::closeness(m_ig,mode="all")

### Betweenness ----
V(m_ig)$betweeness <- igraph::betweenness(m_ig)

# Gráficos comparativos índices de centralidad
par(mfrow = c(1,1))
set.seed(1)
deg_plot <- multiweb::plot_troph_level(m_ig, vertex.size=0.5*(V(m_ig)$totdegree), ylab = "Trophic level", main = "Degree")
set.seed(1)
btw_plot <- multiweb::plot_troph_level(m_ig, vertex.size=sqrt(V(m_ig)$betweeness), ylab = "Trophic level", main = "Betweenness")
set.seed(1)
clo_plot <- multiweb::plot_troph_level(m_ig, vertex.size=2000*(V(m_ig)$closeness), ylab = "Trophic level", main = "Closeness")

## TL vs Keystone ind ----
ind_tl <- key_ind %>% 
   #dplyr::filter(IEC <= 10) %>% 
   ggplot(aes(x=TL, y=Keystone_ind)) + 
   geom_point() +
   geom_smooth(method = "loess") +
   scale_x_continuous(breaks=seq(1,6,1)) +
   scale_y_reverse(breaks=c(1,10,20,30,40,50,60)) +
   labs(x = "Trophic level", y = "Keystone index") +
   theme_classic()
