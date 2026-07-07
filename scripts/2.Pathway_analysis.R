# ==============================================================================
# SCRIPT: SIMFS Mechanisms Network Analysis 
# PURPOSE: Analyze relationships between Mechanisms, Attributes, and Domains of 
#          Sustainable Intensification (SI) research 
#          
# AUTHOR: Chimonyo, V.G.P., Hougni, D.G.J.M.
# DATE: 2026-07-07
# ==============================================================================

# Clean global environment
rm(list = ls())

# Install required packages. Note that further packages will be installed by "webshot".
packages <- c("tidyverse", "igraph", "ggraph", "tidygraph", 
              "webshot", "htmlwidgets",
              "networkD3", "DiagrammeR", "deSolve",
              "FactoMineR", "factoextra", "reshape2")
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) install.packages(packages[!installed])
lapply(packages, library, character.only = TRUE)

# ==============================================================================
# Set working directory. Replace "getwd()" with the local path to "scripts" directory.
setwd(getwd())

# Define file paths (customize this section depending on local paths)
output_path3 <- '../data/processed/SIMFS_Mechanisms_v3.csv'
output_path4 <- '../data/processed/SIMFS_Mechanisms_v4.csv'
output_path5 <- '../output/SIMFS_Sankey_diagram.html'
output_path6 <- '../output/SIMFS_Sankey_diagram.png'
output_path7 <- '../data/processed/SIMFS_Sankey_diagram.csv'

# Load data
dat <- read.csv('../data/raw/SIMFS_mechanisms.csv')

# ==============================================================================
# data wrangling
# Standardize text for consistency
dat <- dat %>%
  mutate(
    Mechanism = tolower(Mechanism),
    Attribute = tolower(Attribute),
    SI_Domain = tolower(SI_Domain)
  )
# 
# Replace full terms with abbreviations for a broader range of attributes
dat$Attribute <- recode(dat$Attribute,
                         # Attributes given in your example
                         "inorganic fertilizer" = "fert. use",
                         "organic fertilizer" = "org. fert.",
                         "fertilizer use" = "fert. use",
                         "legume integration" = "leg. int.",
                         "livestock integration" = "liv. int.",
                         "farm diversification" = "far. div.",
                         "diverse farming strategies" = "far. div.",
                         "intercropping" = "inter.",
                         "agroforestry" = "agrof.",
                         "rotations" = "rot.",
                         "rotation" = "rot.",
                         "crop rotation"  = "rot.",

                         # Additional attributes
                         "land management" = "land mgmt.",
                         "soil fertility" = "soil fert.",
                         "capital investment" = "cap. inve.",
                         "farmer decision making" = "farm dec. mak.",
                         "irrigation management" = "irr. mgmt.",
                         "irrigation" = "irr. mgmt.",
                         "irrigation. mechanization" = "irr. mgmt.",
                         "knowledge dependent intervention" = "know. intv.",
                         "extension services" = "ext. serv.",
                         "farmer-to-farmer extension" = "ext. serv.",
                         "improved information management" = "imp. info. mgmt.",
                         "market access" = "mkt acc",
                         "market access and income generation" = "mkt acc",
                         "conservation agriculture" = "cons. agric.",
                         "improved varieties" = "imp. var.",
                         "infrastructure development" = "infra. dev.",
                         "land suitability" = "land suit.",
                         "input use efficiency" = "input use eff.",
                         "income diversification" = "inc. div.",
                         "financial support" = "fin. supp.",
                         "policy intervention" = "policy intv.",
                         "training support" = "train. supp.",
                         "credit access" = "cred. acc",
                         "asset ownership" = "asset own.",
                         "asset holdings" = "asset own.",
                         "crop water productivity" = "rue",
                         "residue management" = "resid. mgmt.",
                         "residues management" = "resid. mgmt.",
                         "residues incorparation" = "resid. mgmt.",
                         "residue incorporation" = "resid. mgmt.",
                         "mulching"= "mulch.",
                         "residue mulching" = "mulch.",
                         "weed management" = "weedmg mt.",
                         "rotation management" = "rot.",
                         "farm consolidation" = "farm cons.",
                         "farm size adjustment" = "farm cons.",
                         "capacity building" = "cap. bui.",
                         "lead-and-follow farmer training" = "cap. bui.",
                         "labor dynamics" = "lab. dyn.",
                         "labour productivity" = "lab. dyn.",
                         "livestock management" = "liv. man.",
                         "off-farm income" = "off. far. inc.",
                         "policies" = "policy"

)

dat <- dat %>%
  mutate(Attribute = str_replace_all(Attribute, c(
    "social network|social networks|social structures" = "soc. net.",
    "land holding|land tenure" = "land",
    "resource use efficiency|input use efficiency|wue|input use eff." = "rue",
    "road network|infrastructure|infrustructure development" = "infr",
    "change of crop types|crop change" = "cro. cha.",
    "fertiliser|fertiliser use" = "fert.",
    "crop diversification|crop diversity" = "cro. div.",
    "hybrid seed|improved seed" = "imp. var.",
    "seed availability|seed production and distribution|manual seeding systems|community seedbanks" = "seed"

  )))

unique(dat$Attribute)

# Replace full terms with abbreviations for a broader range of attributes
dat$Mechanism <- recode(dat$Mechanism,
                         # Mechanisms with long labels

                         "crop/breed change and product differentiation" = "crop/breed change",
                         "improved information management" =  "information management",
                         "regional specialisation and concentration" =  "regional specialisation",
                         "farm consolidation or fragmentation" =  "farm consolidation" ,
 )

# Remove duplicates
dat <- dat %>% distinct()

# Count occurrences of each mechanism-attribute pair
mechanism_attribute_counts <- dat %>%
  group_by(Mechanism, Attribute) %>%
  tally() 

# Save the updated dat as a CSV
write_csv(mechanism_attribute_counts, output_path3)


# Count occurrences of each mechanism-attribute pair
mechanism_attribute_counts <- dat %>%
  group_by(Mechanism, Attribute) %>%
  tally() %>%
  filter(n >= 15)  # Filter for 10 or more occurrences

# Merge back to keep only relevant data
filtered_dat <- dat %>%
  inner_join(mechanism_attribute_counts, by = c("Mechanism", "Attribute"))

dat <- filtered_dat

dat$Attribute <- recode(dat$Attribute,
                         "fert. use" = "fertilizer use",
                         "liv. int." = "livestock integration",
                         "imp. var." = "improved varieties",
                         "lab. dyn." = "labour dynamics",
                         "cro. div." = "crop diversification",
                         "leg. int." = "legume integration",
                         "ext. serv." = "extension service",
                         "rot." = "rotations",
                         "cons. agric." = "conservation agriculture",
                         "mkt acc" = "market access",
                         "cap. bui." = "capacity building")

# Save the updated dat as a CSV
write_csv(dat, output_path4)


# =============================
# SECTION 1: SANKEY DIAGRAM
# =============================

dat$Attribute <- recode(dat$Attribute,
                         "fert. use" = "fertilizer use",
                         "liv. int." = "livestock integration",
                         "imp. var." = "improved varieties",
                         "lab. dyn." = "labour dynamics",
                         "cro. div." = "crop diversification",
                         "leg. int." = "legume integration",
                         "ext. serv." = "extension service",
                         "rot." = "rotations",
                         "cons. agric." = "conservation agriculture",
                         "mkt acc" = "market access",
                         "cap. bui." = "capacity building")

# Create edge lists for SI_Domain → Mechanism and Mechanism → Attribute
edges <- dat %>%
  select(source = Attribute, middle = Mechanism, target = SI_Domain)

# Define nodes and map to indices
nodes <- data.frame(name = unique(c(edges$source, edges$middle, edges$target)))
get_index <- function(x) match(x, nodes$name) - 1

# Build links from SI_Domain → Mechanism
links1 <- edges %>%
  select(source, target = middle) %>%
  mutate(source = get_index(source), target = get_index(target))

# Build links from Mechanism → Attribute
links2 <- edges %>%
  select(source = middle, target) %>%
  mutate(source = get_index(source), target = get_index(target))

# Combine and summarise link weights
links <- bind_rows(links1, links2) %>%
  group_by(source, target) %>%
  summarise(value = n(), .groups = "drop")

# Plot Sankey
sankey <- networkD3::sankeyNetwork(Links = links, Nodes = nodes,
                                   Source = "source", Target = "target",
                                   Value = "value", NodeID = "name",
                                   fontSize = 20, nodeWidth = 40,
                                   sinksRight = FALSE)
sankey

# Save the Sankey diagram as an HTML file
networkD3::saveNetwork(sankey, output_path5)


# Convert the saved HTML to a PNG using webshot
# First, install PhantomJS if not done yet
webshot::install_phantomjs() # better not include it at the top of the script to avoid conflicts

webshot::is_phantomjs_installed()

# Now save the HTML file as a PNG
webshot::webshot(output_path5, output_path6,
                 vwidth = 1500, vheight = 750)

# =============================
# SECTION 2: NETWORK ANALYSIS
# =============================

# -------------------------------------------------
# STEP 1: Create network edges
# -------------------------------------------------

# Prepare edges from SI_Domain → Mechanism
edges1 <- dat %>%
  select(from = SI_Domain, to = Mechanism)

# Prepare edges from Mechanism → Attribute
edges2 <- dat %>%
  select(from = Mechanism, to = Attribute)

# Combine and count frequencies
edges_all <- bind_rows(edges1, edges2) %>%
  group_by(from, to) %>%
  summarise(weight = n(), .groups = "drop")

# -------------------------------------------------
# STEP 2: Create graph object
# -------------------------------------------------

graph <- tidygraph::tbl_graph(edges = edges_all, directed = TRUE)

# Assign node type based on inclusion in column
graph <- graph %>%
  mutate(type = case_when(
    name %in% dat$SI_Domain ~ "SI_Domain",
    name %in% dat$Mechanism ~ "Mechanism",
    name %in% dat$Attribute ~ "Attribute",
    TRUE ~ "Other"
  ))

# -------------------------------------------------
# STEP 3: Compute centrality metrics
# -------------------------------------------------

graph <- graph %>%
  mutate(
    degree = tidygraph::centrality_degree(mode = "all"),
    betweenness = tidygraph::centrality_betweenness(),
    closeness = tidygraph::centrality_closeness()
  )

# -------------------------------------------------
# STEP 4: Plot the network
# -------------------------------------------------

ggraph::ggraph(graph, layout = "fr") +
  ggraph::geom_edge_link(aes(width = weight), alpha = 0.4) +
  ggraph::geom_node_point(aes(color = type, size = degree)) +
  ggraph::geom_node_text(aes(label = name), repel = TRUE, size = 3.2) +
  ggraph::scale_edge_width(range = c(0.2, 2)) +
  theme_void() +
  ggtitle("Sustainable Intensification Network: SI_Domain → Mechanism → Attribute")

# -------------------------------------------------
# STEP 5: Statistical summaries
# -------------------------------------------------

# Summary by node type
summary_table <- graph %>%
  as_tibble() %>%
  group_by(type) %>%
  summarise(
    count = n(),
    mean_degree = round(mean(degree), 2),
    max_betweenness = round(max(betweenness), 2),
    top_node = name[which.max(betweenness)]
  )

print(summary_table)

# Top 10 influential mechanisms
top_mechanisms <- graph %>%
  as_tibble() %>%
  filter(type == "Mechanism") %>%
  arrange(desc(betweenness)) %>%
  slice(1:10)

print(top_mechanisms)

# Edge statistics
edge_stats <- edges_all %>%
  summarise(
    mean_weight = mean(weight),
    sd_weight = sd(weight),
    max_weight = max(weight),
    median_weight = median(weight)
  )

print(edge_stats)


# ---------------------------------------------------
# Construct Tripartite Matrix (long-form for network)
# ---------------------------------------------------

# Create two edge types
edges1 <- dat %>% select(from = SI_Domain, to = Mechanism) %>% mutate(type = "domain_mech")
edges2 <- dat %>% select(from = Mechanism, to = Attribute) %>% mutate(type = "mech_attr")

# Combine edges
edges_all <- bind_rows(edges1, edges2)

# Create node type
nodes <- unique(c(edges_all$from, edges_all$to))
node_type <- tibble(name = nodes) %>%
  mutate(type = case_when(
    name %in% dat$SI_Domain ~ "SI_Domain",
    name %in% dat$Mechanism ~ "Mechanism",
    name %in% dat$Attribute ~ "Attribute",
    TRUE ~ "Other"
  ))

# ---------------------------------------------------
# Create Graph Object
# ---------------------------------------------------
g <- igraph::graph_from_data_frame(d = edges_all, vertices = node_type, directed = TRUE)
g_tbl <- tidygraph::as_tbl_graph(g)

# ---------------------------------------------------
# Plot Network
# ---------------------------------------------------
ggraph::ggraph(g_tbl, layout = 'fr') +
  ggraph::geom_edge_link(aes(edge_colour = type), alpha = 0.5) +
  ggraph::geom_node_point(aes(color = type), size = 4) +
  ggraph::geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  ggraph::scale_edge_colour_manual(values = c("domain_mech" = "blue", "mech_attr" = "green")) +
  theme_void() +
  ggtitle("Tripartite Network: SI_Domain → Mechanism → Attribute")

# ---------------------------------------------------
# Optional: Co-occurrence Matrix (Mechanism × Attribute)
# ---------------------------------------------------
mech_attr_mat <- table(dat$Mechanism, dat$Attribute)

# Plot as heatmap
heatmap(mech_attr_mat, Rowv = NA, Colv = NA,
        col = colorRampPalette(c("white", "steelblue"))(100),
        main = "Mechanism–Attribute Co-Occurrence Heatmap",
        xlab = "Attribute", ylab = "Mechanism")

# =============================
# SECTION 3: MULTIVARIATE ANALYSIS
# =============================

# ---------------------------------------------------
# 1. Create Binary Incidence Matrix
# ---------------------------------------------------
inc_matrix <- table(dat$Mechanism, dat$Attribute)
inc_matrix[inc_matrix > 1] <- 1  # Convert to binary presence/absence

# Convert to data frame for MCA
inc_df <- as.data.frame.matrix(inc_matrix)

# ---------------------------------------------------
# 2. Run MCA
# ---------------------------------------------------
# Convert binary columns to factors as required by MCA
inc_df_factors <- inc_df %>%
  mutate(across(everything(), ~ as.factor(.)))

# Now run MCA
mca_result <- FactoMineR::MCA(inc_df_factors, graph = FALSE)

# MCA biplot
factoextra::fviz_mca_biplot(mca_result,
                            repel = TRUE,
                            ggtheme = theme_minimal(),
                            title = "MCA Biplot: Mechanisms and Attributes")

# END OF SCRIPT
# ==============================================================================
