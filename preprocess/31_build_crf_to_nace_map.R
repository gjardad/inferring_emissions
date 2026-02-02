###############################################################################
# 01_preprocess/31_build_crf_to_nace_map.R
#
# PURPOSE
#   Build mapping from CRF (national inventory) categories to NACE sectors.
#
# WHERE THIS FILE BELONGS
#   loocv_pipeline/01_preprocess/31_build_crf_to_nace_map.R
###############################################################################

#### HEADER -------

## This code creates a mapping between IPCC sectors and NACE 2-digit sectors

# The IPCC categories are the most granular made available by the national inventories
# The corresponding 2-digit NACE codes were taken from Annex I Correspondence between CRF-NFR-NACE-Rev.2 to the Manual of Air Emissions Accounts
# The corresponding 4-digit NACE codes are my own

#####################

# Setup ------
rm(list = ls())

if(tolower(Sys.info()[["user"]]) == "jardang"){
  folder <- "X:/Documents/JARDANG" 
}

raw_data <- paste0(folder, "/carbon_policy_networks/data/raw")

int_data <- paste0(folder, "/carbon_policy_networks/data/intermediate")

proc_data <- paste0(folder, "/carbon_policy_networks/data/processed")

output <- paste0(folder, "/carbon_policy_networks/output")

code <- paste0(folder, "/carbon_policy_networks/code")

# Libraries ----

library(tibble)
library(dplyr)
library(tidyr)
library(purrr)

# Create mapping -----------
crf_nace_map <- tribble(
  ~crf, ~crf_label, ~nace,
  
  # -------- Fuel combustion ---------
  
  "1.A.1.a.", "Public electricity and heat production",
  list(c("D35")),
  
  "1.A.1.b.",     "Petroleum refining",
  list(c("C19")),
  
  "1.A.1.c.i.",   "Manufacture of solid fuels",
  # the official mapping also includes C24 but in Belgium 1.A.1.c is
  # exclusively coke ovens (see section 3.2.6.1 in Belgium's NIR)
  # which corresponds to NACE C19
  list(c("C19")),
  
  "1.A.1.c.ii.",  "Oil and gas extraction",
  list(c("B05", "B06", "B07", "B08", "B09")),
  
  "1.A.1.c.iii.", "Other energy industries",
  list(c("D35")),
  
  "1.A.2.a.",     "Iron and steel",
  # the official mapping also includes C25, but C25 is very small compared to C24
  # in Belgium and including C25 would force me to create a nace_group that includes
  # C24, C25, and many others (the "others" category)
  list(c("C24")),
  
  "1.A.2.b.",     "Non-ferrous metals",
  # the official mapping also includes C25, but C25 is very small compared to C24
  # in Belgium and including C25 would force me to create a nace_group that includes
  # C24, C25, and many others (the "others" category)
  list(c("C24")),
  
  "1.A.2.c.",     "Chemicals",
  list(c("C20", "C21")),
  
  "1.A.2.d.",     "Pulp, paper and print",
  list(c("C17", "C18")),
  
  "1.A.2.e.",     "Food processing, beverages and tobacco",
  list(c("C10", "C11", "C12")),
  
  "1.A.2.f.",     "Non-metallic minerals",
  list(c("C23")),
  
  "1.A.2.g.vi.",  "Textile and leather",
  list(c("C13", "C14", "C15")),
  
  "1.A.2.g.vii.", "Off-road vehicles and other machinery",
  list(c("C16", "C22", "C25", "C26", "C27", "C28", "C29", "C30", 
         "C31_32", "C33", "F")),
  
  "1.A.2.g.viii.", "Other manufacturing and construction",
  list(c("C16", "C22", "C25", "C26", "C27", "C28", "C29", "C30", 
         "C31_32", "C33", "F")),
  
  "1.A.4.a.",  "Commercial/institutional ",
  list(c("E36", "G45", "G46", "G47", "H52", "H53", "I", "J", "K", "L", "M", "N",
          "P", "Q", "R", "S", "T")),
  
  "1.A.4.c.i.", "Stationary",
  list(c("A01", "A02")),
  
  "1.A.4.c.ii.", "Off-road vehicles and other machinery",
  list(c("A01", "A02")),
  
  "1.A.4.c.iii.", "Fishing",
  list(c("A03")),
  
  # ------- Fugitive emissions ---------
  
  # Natural gas distribution
  "1.B.2.b.v.", "Distribution",
  list(c("D35")),
  
  "1.B.2.c.i.", "Venting",
  list(c("D35")),
  
  "1.B.2.c.ii.", "Flaring",
  list(c("C19")),
  
  # Obs: the official mapping for 1.B.2.c also includes B05, B06, B07, B08, B09
  # but NIR reports that there are two only sources of emissions
  # for this category: 1.B.2.c.ii corresponds exclusively to
  # flaring of refinery gas (section 3.2.6.2.2 Petroleum refining),
  # which is NACE 19, and a small amount of CH4 from venting
  # at a gas receiving terminal (section 3.3.2.2.4 Venting and Flaring in NIR)
  # which is NACE 35 
  
  # ------- Industrial processes ---------
  
  "2.A.", "Mineral industry",
  list(c("C23")),
  
  "2.B.", "Chemical industry",
  list(c("C20")),
  
  "2.C.", "Metal industry",
  list(c("C24")),
  
  # unclear which NACE to assign 2.D. to;
  # leave it out for now since it's small
  
  # 2.E. and 2.G. are not relevant for Belgium
  
  "2.H.1.", "Pulp and paper",
  list(c("C17")),
  
)

# Expand so that each CRF x NACE is one row --------

crf_nace_long <- crf_nace_map %>%
  mutate(nace = map(nace, ~ .x[[1]])) %>%
  unnest_longer(nace) %>%
  rename(nace_code = nace)

# Create NACE groups ---------

  # obs: a group is a set of connected components in a graph of NACE codes

  # there's an edge between any two NACE codes if they ever appear together for the same crf
  # each connected componenent of the graph is a nace_group

  library(igraph)

  # 1. Clean unique CRF–NACE pairs
  df_clean <- crf_nace_long %>%
    distinct(crf, nace_code) %>%
    filter(!is.na(crf), !is.na(nace_code))

  # 2. For each CRF, make all unordered pairs of NACE codes that co-occur
  edges <- df_clean %>%
    group_by(crf) %>%
    summarise(
      nace_vec = list(sort(unique(nace_code))),
      .groups = "drop"
    ) %>%
    # keep only CRFs that map to at least 2 NACE codes
    filter(lengths(nace_vec) > 1) %>%
    mutate(
      pairs = map(nace_vec, ~ {
        # all unordered pairs of the vector .x
        m <- t(combn(.x, 2))
        # combn() returns a vector if there are only 2 elements, so ensure matrix
        if (is.null(dim(m))) {
          tibble(from = .x[1], to = .x[2])
        } else {
          tibble(from = m[, 1], to = m[, 2])
        }
      })
    ) %>%
    select(pairs) %>%
    unnest(pairs) %>%
    distinct()

  # 3. All NACE codes as vertices
  all_nace <- sort(unique(df_clean$nace_code))

  # 4. Build graph (handles case with zero edges)
  if (nrow(edges) > 0) {
    g <- graph_from_data_frame(
      d = edges,
      directed = FALSE,
      vertices = data.frame(name = all_nace, stringsAsFactors = FALSE)
    )
  } else {
    g <- make_empty_graph(n = length(all_nace), directed = FALSE)
    V(g)$name <- all_nace
  }
  
  # 5. Connected components = nace_group
  comp <- components(g)
  
  nace_groups <- tibble(
    nace_code  = V(g)$name,
    nace_group = comp$membership[match(V(g)$name, names(comp$membership))]
  )
  
  # 6. Join back to your original data and rename it
  crf_to_nace_map <- crf_nace_long %>%
    left_join(nace_groups, by = "nace_code")

# Save it ---------
save(crf_to_nace_map, file = paste0(proc_data, "crf_to_nace_map.RData"))
