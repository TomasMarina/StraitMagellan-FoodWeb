# Magellan Strait food web: modularity analysis
# Date: December 2024 - March 2025
# Author: Tomas I. Marina

# Packages ----------------------------------------------------------------
require(igraph)
require(multiweb)
require(tidyverse)
require(ggplot2)

# Load data ---------------------------------------------------------------
load("Results/Network&SpProps_22mar25.rda")
m_ig <- igraph::upgrade_graph(m_ig)

# Modularity analyses -----------------------------------------------------
## Modularity
mod <- multiweb::calc_modularity(m_ig, cluster_function = cluster_spinglass)

## Topological roles
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
save(m_ig, modsim, modules, clas.role, top.role.count, roles, g_mod,
     file = "Results/Modularity_22mar25.rda")
