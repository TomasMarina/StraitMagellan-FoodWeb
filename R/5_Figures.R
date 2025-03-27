# Figures
# Date: March 2025
# Author: Tomas I. Marina


# Load packages -----------------------------------------------------------
require(igraph)
require(multiweb)
require(tidyverse)
require(ggjoy)
require(ggpubr)
require(NetIndices)
require(network)
require(NetworkExtinction)


# Load data ---------------------------------------------------------------
load("Results/Data_tidy_22mar25.rda")
load("Results/Network&SpProps_22mar25.rda")
load("Results/Modularity_22mar25.rda")


# Network-level analysis --------------------------------------------------
## Figure 2 ---------------------------------------------------------------
set.seed(1)
plt_fw <- plot_troph_level(m_ig, vertexLabel = F, vertex.size = 8,
                           ylab = "Trophic level")

### by degree
## Calculate trophic level & omnivory
adj_mat <- as_adjacency_matrix(m_ig, sparse = FALSE)
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


## Figure 3 ---------------------------------------------------------------
### Degree distribution ----
tot.deg <- degree(m_ig, mode = "all")
V(m_ig)$totdegree <- degree(m_ig, mode="all")
out.deg <- degree(m_ig, mode = "out")
V(m_ig)$outdegree <-  out.deg
in.deg <- degree(m_ig, mode = "in")
V(m_ig)$indegree <-  in.deg

### Cumulative
upgrade_graph(m_ig)
g_net <- as.network(as.matrix(m_ig))
dist_fit <- NetworkExtinction::DegreeDistribution(g_net)

(plot_cumdeg <- dist_fit[["graph"]] +
  labs(y = "Cumulative degree distribution", x = "Degree (k)") +
  #ggtitle("A)") +
  theme(axis.title.x = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 7),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        panel.background = element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line = element_line(colour = "black")))

### Generality ----
gen.fun <- function(m_ig){
  pred <- degree(m_ig, mode = "in") > 0
  G <- sum(degree(m_ig, mode = "in")[pred] / sum(pred))
  return(G)
}
gen.fun(m_ig) # average number of preys per predator

data_indeg <- as.data.frame(V(m_ig)$indegree) 
data_indeg <- data_indeg %>% 
  filter(V(m_ig)$indegree != 0) %>% 
  rename(n = "V(m_ig)$indegree")
(plot_indeg <- ggplot(data_indeg, aes(x = n)) +
  geom_histogram(stat = "count") +
  labs(x = "Predators", y = "Number of prey") +
  theme_bw() +
  theme(legend.position = "none", 
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10)))

### Vulnerability ----
vul.fun <- function(m_ig){
  prey <- degree(m_ig, mode = "out") > 0
  V <- sum(degree(m_ig, mode = "out")[prey]) / sum(prey)
  return(V)
}
vul.fun(m_ig) # average number of predators per prey

data_outdeg <- as.data.frame(V(m_ig)$outdegree) 
data_outdeg <- data_outdeg %>% 
  filter(V(m_ig)$outdegree != 0) %>% 
  rename(n = "V(m_ig)$outdegree")
(plot_outdeg <- ggplot(data_outdeg, aes(x = n)) +
  geom_histogram(stat = "count") +
  labs(x = "Prey", y = "Number of predators") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10)))

### Distribution by Group ----
plot_deg_g <- m_spp_total %>% 
  add_count(Group) %>% 
  filter(n > 2) %>% 
  group_by(Group) %>% 
  mutate(TL_g = mean(TL)) %>% 
  mutate(color = case_when(TL_g <= 1 ~ "#006837",
                           between(TL_g, 1.1, 1.95) ~ "#1A9850",
                           between(TL_g, 1.98, 2.05) ~ "#66BD63",
                           between(TL_g, 2.06, 2.25) ~ "#A6D96A",
                           between(TL_g, 2.26, 2.5) ~ "#D9EF8B",
                           between(TL_g, 2.51, 3.0) ~ "#FFFFBF",
                           between(TL_g, 3.001, 3.25) ~ "#FEE08B",
                           between(TL_g, 3.26, 3.5) ~ "#FDAE61",
                           between(TL_g, 3.51, 4.0) ~ "#F46D43",
                           between(TL_g, 4.01, 4.49) ~ "#D73027",
                           TL_g >= 4.5 ~ "#A50026")) %>% 
  ggplot(., aes(x = TotalDegree, y = reorder(Group, TL_g))) + 
  ggjoy::geom_joy(aes(scale = 3.75, fill = color)) +
  scale_fill_identity() +
  labs(x = "Number of interactions", y = "Group", tag="B") +
  theme_joy(grid = FALSE) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 6),
        axis.title.x = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10),
        plot.tag = element_text(face = "bold", size = 10),
        plot.tag.position = c(0.05, 1))


### Figure 3
fig3 <- ggarrange(plot_cumdeg,   # First row
                      ggarrange(plot_deg_g, ncol = 1),  # Second row
                      ggarrange(plot_indeg, plot_outdeg, ncol = 2, labels = c("C", "D"), font.label=list(color="black", size=10)),   # Third row
                      nrow = 3, labels = "A",
                      font.label=list(color="black", size=10))

# ggsave(filename = "Figures/Fig3_24mar25.png", plot = fig3,
#        width = 10, units = "in", dpi = 600, bg = "white")


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


## Figure 4 ---------------------------------------------------------------
### Closeness ----
V(m_ig)$closeness <- igraph::closeness(m_ig, mode="all")

### Betweenness ----
V(m_ig)$betweeness <- igraph::betweenness(m_ig)

### Figure 4
par(mfrow = c(1,1))
set.seed(1)
deg_plot <- multiweb::plot_troph_level(m_ig, vertex.size=0.5*(V(m_ig)$TotalDegree), 
                                       edge.arrow.width=0.5, 
                                       ylab = "Trophic level", main = "A)")
set.seed(1)
btw_plot <- multiweb::plot_troph_level(m_ig, vertex.size=sqrt(V(m_ig)$betweeness), 
                                       edge.arrow.width=0.5,
                                       main = "B)")
set.seed(1)
clo_plot <- multiweb::plot_troph_level(m_ig, vertex.size=2000*(V(m_ig)$closeness),
                                       edge.arrow.width=0.5,
                                       main = "C)")


## Figure 5 ---------------------------------------------------------------
(top_role_plot <- ggplot(clas.role, aes(among_module_conn, within_module_degree, color=type)) + 
  geom_point() + 
  geom_vline(xintercept=0.625, linetype="dashed", color = "red", linewidth=0.25) +
  geom_hline(yintercept=2.5, linetype="dashed", color = "red", linewidth=0.25) +
  xlab("Among module connectance") + ylab("Within module degree") +
  labs(color="Role") +
  geom_text(label = ifelse(clas.role$type == "hubcon", clas.role$name, NA), 
            size=3, nudge_y = 0.07) + 
  geom_text(label = ifelse(clas.role$type == "modcon", clas.role$name, NA), 
            size=3, nudge_y = 0.07) +
  ylim(-1.5,4) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(face = "bold", size = 10),
        axis.title.y = element_text(face = "bold", size = 10)))

# ggsave(filename = "Figures/Fig5_27mar25.png", plot = top_role_plot,
#       width = 10, units = "in", dpi = 600, bg = "white")


## TL vs Keystone ind -----------------------------------------------------
ind_tl <- key_ind %>% 
  #dplyr::filter(IEC <= 10) %>% 
  ggplot(aes(x=TL, y=Keystone_ind)) + 
  geom_point() +
  geom_smooth(method = "loess") +
  scale_x_continuous(breaks=seq(1,6,1)) +
  scale_y_reverse(breaks=c(1,10,20,30,40,50,60)) +
  labs(x = "Trophic level", y = "Keystone index") +
  theme_classic()
