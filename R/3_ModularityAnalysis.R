# Magellan Strait food web: modularity analysis
# Data: December 2024 - January 2025
# Author: Tomas I. Marina

# Packages ----------------------------------------------------------------
require(igraph)
require(multiweb)
require(tidyverse)
require(ggplot2)

# Load data ---------------------------------------------------------------
load("Results/Network&SpProps_19feb24.rda")
m_ig <- igraph::upgrade_graph(m_ig)

# Modularity analysis -----------------------------------------------------
# Simulation FW same S and L
gsim <- multiweb::curve_ball(m_ig, nsim = 1000, istrength = FALSE)

mod <- multiweb::calc_modularity(m_ig, weights = NULL)
modules <- igraph::cluster_spinglass(m_ig)  # modules
# cbind(V(g)$name, modules$membership)

grupos <- cbind(modules$names, modules$membership)
colnames(grupos) <- c("Node", "Group")

# write.csv(grupos, "grupos.csv")
modsim <- calc_modularity(gsim, ncores = 4, weights = NULL)

# Modularity plot
# dev.new()
# svg(filename="Modularity.svg",
#     width=6,
#     height=4,
#     pointsize=12)
modsim_plot <- ggplot(modsim, aes(x=Modularity)) +
  geom_histogram(alpha=0.5, position ="identity", bins=17, color="darkorchid4", fill="darkviolet") + 
  theme(legend.title =element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  geom_vline(xintercept=(as.numeric(mod)), linetype="dashed", color = "deeppink", linewidth=0.25)
modsim_plot
# dev.off()
# ggsave(filename = "Figures/FigS1_060125.png", plot = modsim_plot,
#        width = 10, units = "in", dpi = 600, bg = "white")

# Topological roles
top.role <- multiweb::calc_topological_roles(m_ig, nsim = 1000, ncores = 4)
clas.role <- multiweb::classify_topological_roles(top.role, m_ig, plt = TRUE)
top.role.df <- clas.role %>% 
  mutate(Module = modules$membership[node]) %>% 
  rename(TrophicSpecies = name, TopRole = type) %>% 
  dplyr::select(TrophicSpecies, TopRole)
top.role.count <- top.role.df %>% 
  count(TopRole)

#write.csv(clas.role, "TopRoles_magellan.csv")
#write.csv(top.role.df, "ClasRoles_magellan.csv")

# dev.new()
# svg(filename="TopologicalRoles_labels.svg", 
#     width=6, 
#     height=4, 
#     pointsize=12)
roles <- ggplot(clas.role, aes(among_module_conn, within_module_degree, color=type)) + 
  geom_point() + 
  geom_vline(xintercept=0.625, linetype="dashed", color = "red", linewidth=0.25) +
  geom_hline(yintercept=2.5, linetype="dashed", color = "red", linewidth=0.25) +
  xlab("Among module connectance") + ylab("Within module degree") +
  labs(color="Role") +
  geom_text(label = ifelse(clas.role$type == "hubcon", clas.role$name, NA), 
            size=3, nudge_y = 0.07) + 
  geom_text(label = ifelse(clas.role$type == "modcon", clas.role$name, NA), 
            size=3, nudge_y = 0.07) +
  ylim(-1.5,3.5) +
  theme_bw()
roles
# dev.off()
# ggsave(filename = "Figures/Fig5_060125.png", plot = roles,
#        width = 10, units = "in", dpi = 600, bg = "white")

# Include role as node attribute in g object
df_g <- igraph::as_data_frame(m_ig, 'both')
df_g$vertices <- df_g$vertices %>% 
  left_join(top.role.df, c('name' = 'TrophicSpecies'))
g_mod <- graph_from_data_frame(df_g$edges, directed = TRUE, vertices = df_g$vertices)


# Food web graph ----------------------------------------------------------
g_graph <- multiweb::plot_troph_level(g_mod, modules = T)


# Save results ------------------------------------------------------------
save(m_ig, modsim, modules, clas.role, roles, g_mod,
     file = "Results/Modularity_06jan25.rda")
