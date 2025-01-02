## FOOD WEB ANALYSIS: MAGELLAN STRAIT
# Fecha: 20/11/2024

# Packages ----------------------------------------------------------------
require(igraph)
require(multiweb)
library(tidyverse)
library(ggplot2)

# Load data ---------------------------------------------------------------
int_list <- read.csv("MagellanStrait_links_13feb24.csv")
int_list <- int_list %>% 
  dplyr::select(resource_original, consumer)

# Create g object ---------------------------------------------------------
g <- igraph::graph_from_edgelist(as.matrix(int_list), directed = T)

# Simulation FW same S and L
#gsim<- curve_ball(g)

# Modularity analysis -----------------------------------------------------
mod <- multiweb::calc_modularity(g, weights = NULL)
modules <- igraph::cluster_spinglass(g)  # modules
#cbind(V(g)$name, modules$membership)

grupos<-cbind(modules$names, modules$membership)
colnames(grupos)<-c("Node", "Group")
grupos

write.csv(grupos, "grupos.csv")

modsim<-calc_modularity(gsim)

#Modularity plot

dev.new()
svg(filename="Modularity.svg", 
    width=6, 
    height=4, 
    pointsize=12)
H3<-ggplot(modsim, aes(x=Modularity)) +
  geom_histogram(alpha=0.5, position ="identity",bins=17, color="darkorchid4", fill="darkviolet") + theme(legend.title =element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + geom_vline(xintercept=(as.numeric(mod)), linetype="dashed", color = "deeppink", linewidth=0.25)
H3
dev.off()

# Topological roles
top.role <- multiweb::calc_topological_roles(g, nsim = 1000)
clas.role <- multiweb::classify_topological_roles(top.role, g, plt = TRUE)
top.role.df <- clas.role %>% 
  mutate(Module = modules$membership[node]) %>% 
  rename(TrophicSpecies = name, TopRole = type) %>% 
  dplyr::select(TrophicSpecies, TopRole)
top.role.count <- top.role.df %>% 
  count(TopRole)

write.csv(clas.role, "TopRoles_magellan.csv")
write.csv(top.role.df, "ClasRoles_magellan.csv")

dev.new()
svg(filename="TopologicalRoles_labels.svg", 
    width=6, 
    height=4, 
    pointsize=12)
roles<- ggplot(clas.role, aes(among_module_conn, within_module_degree, color=type)) + 
geom_point() + theme_bw() + geom_vline(xintercept=0.625, linetype="dashed", color = "red", linewidth=0.25) +
geom_hline(yintercept=2.5, linetype="dashed", color = "red", linewidth=0.25) +
xlab("Among module connectance") + ylab("Within module degree") +  
geom_text(label=clas.role$name, size=3, nudge_y = 0.07) + ylim(-1.5,3)
roles
dev.off()

# Include role as node attribute in g object
df_g <- igraph::as_data_frame(g, 'both')
df_g$vertices <- df_g$vertices %>% 
  left_join(top.role.df, c('name' = 'TrophicSpecies'))
g <- graph_from_data_frame(df_g$edges, directed = TRUE, vertices = df_g$vertices)


# Food web graph ----------------------------------------------------------
g_graph <- multiweb::plot_troph_level(g, modules = T)
