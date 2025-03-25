# Magellan Strait food web: all species-level properties
# Date: March 2025
# Author: Tomas I. Marina

# Packages ----------------------------------------------------------------
require(tidyverse)


# Load data ---------------------------------------------------------------
load("Results/Network&SpProps_22mar25.rda")
load("Results/Modularity_22mar25.rda")


# Merge species properties ------------------------------------------------
clas.role.ok <- clas.role %>% 
  rename(TrophicSpecies = name) %>% 
  dplyr::select(-node)
all_sp_prop <- key_ind %>% 
  dplyr::select(-ID) %>% 
  left_join(clas.role.ok) %>% 
  rename(TopRole = type)
#write.csv(all_sp_prop, file = "Results/All_sp_prop_22mar25.csv")


# Retrieve taxonomy -------------------------------------------------------
data <- Lista_especies_para_Parques_Nacionales_Hoja_1
require(taxize)
sp_list <- unique(data$`Especie/Taxón`)
sp_names <- taxize::gna_verifier(data$`Especie/Taxón`, best_match_only = TRUE, canonical=TRUE)
clas_sp <- taxize::tax_name(sci = sp_names$matchedCanonicalSimple, 
                            get = c("genus","family","order","class"), db = "ncbi")


# Save results ------------------------------------------------------------
save(all_sp_prop, 
     file = "Results/AllSpProps_24mar25.rda")
