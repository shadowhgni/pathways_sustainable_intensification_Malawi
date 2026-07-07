# ==============================================================================
# SCRIPT: SIMFS Mechanisms Network Analysis 
# PURPOSE: Analyze relationships between Mechanisms and Attributes in Sustainable
#          Intensification (SI) research 
#          
# AUTHORS: Chimonyo, V.G.P, Hougni, D.G.J.M.
# DATE: 2026-07-07
# ==============================================================================

# ==============================================================================
# SECTION 1: ENVIRONMENT SETUP AND LIBRARY LOADING
# ==============================================================================

# Clean global environment
rm(list = ls())

# Install required packages. 
packages <- c("tidyverse", "igraph", "threejs", "tidygraph", 
              "corrplot", "htmlwidgets")
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) install.packages(packages[!installed])
lapply(packages, library, character.only = TRUE)


# ==============================================================================
# SECTION 2: FILE PATH CONFIGURATION
# ==============================================================================

# Set working directory. Replace "getwd()" with the local path to "scripts" directory.
setwd(getwd()) 

# Define input file path, and directories for intermediate files and final outputs 
input_file <- '../data/raw/SIMFS_mechanisms.csv'
dir_output <- '../output/'           
dir_processed <- '../data/processed/' 

# Create directories if they don't exist
if(!dir.exists(dir_output)) {
  dir.create(dir_output, recursive = TRUE)
}
if(!dir.exists(dir_processed)) {
  dir.create(dir_processed, recursive = TRUE)
}

# ==============================================================================
# SECTION 3: DATA LOADING AND PREPROCESSING
# ==============================================================================

# Read the CSV file
dat <- read.csv(input_file)

# Convert all text to lowercase for consistency
dat$Mechanism <- tolower(dat$Mechanism)
dat$R_Attribute <- tolower(dat$R_Attribute)
dat$SI_Domain <- tolower(dat$SI_Domain)

# ==============================================================================
# SECTION 4: DATA FILTERING (minimum 10 occurrences of mechanism-attribute pairs)
# ==============================================================================

# Count occurrences of each mechanism-attribute pair
mechanism_attribute_counts <- dat |>
  group_by(Mechanism, R_Attribute) |>
  tally() |>
  filter(n >= 10)

# Filter the original data
filtered_dat <- dat |>
  inner_join(mechanism_attribute_counts, by = c("Mechanism", "R_Attribute")) |>
  distinct()

# ==============================================================================
# SECTION 5: TEXT ABBREVIATION FUNCTION
# ==============================================================================

abbreviate_attributes <- function(text) {
  text <- gsub('farmer|farmers', 'far.', text)
  text <- gsub('intercropping', 'int.', text)
  text <- gsub('improved', 'imp.', text)
  text <- gsub('rotations|crop rotations|crop rotation', 'rot.', text)
  text <- gsub('credit', 'cre.', text)
  text <- gsub('access', 'acc', text)
  text <- gsub('fertiliser', 'fert.', text)
  text <- gsub('inorganic fertilizer', 'ino. fert.', text)
  text <- gsub('organic fertilizer', 'org. fert.', text)
  text <- gsub('fertilizer use', 'fert. use', text)
  text <- gsub('legume integration', 'leg. int.', text)
  text <- gsub('livestock integration', 'liv. int.', text)
  text <- gsub('farm diversification', 'far. div.', text)
  text <- gsub('farm consolidation', 'far. con', text)
  text <- gsub('crop diversification', 'cro. div.', text)
  text <- gsub('capacity building|access to education|lead-and-follow far. training|far. training', 'cap. bui.', text)
  text <- gsub('extension services|far.-to-far. extension', 'ext. ser.', text)
  text <- gsub('agriculture', 'agr.', text)
  text <- gsub('knowledge', 'know.', text)
  text <- gsub('acquisition', 'acqu.', text)
  text <- gsub('capital', 'capi', text)
  text <- gsub('investment|investments', 'inve.', text)
  text <- gsub('climatic|climate', 'clim.', text)
  text <- gsub('preference|preferences', 'pref.', text)
  text <- gsub('information|information availability', 'info.', text)
  text <- gsub('communication', 'comm.', text)
  text <- gsub('technology', 'tech.', text)
  text <- gsub('management', 'mgt.', text)
  text <- gsub('market|mkt. acc and income generation', 'mkt.', text)
  text <- gsub('conservation', 'con.', text)
  text <- gsub('agroforestry', 'agro.', text)
  text <- gsub('labor|labour productivity', 'lab.', text)
  text <- gsub('agri-trade dynamics', 'tra', text)
  text <- gsub('irrigation|irrigation. mechanization', 'irr', text)
  text <- gsub('residue incorparation|residues incorparation', 'res', text)
  text <- gsub('crop change|change of crop types', 'cro. cha.', text)
  text <- gsub('asset ownership|asset holdings', 'ass.', text)
  text <- gsub('mulching|residue mulching', 'mul.', text)
  text <- gsub('seed|seed availability|community seedbanks|seed production and distribution', 'seed', text)
  text <- gsub('social network|social networks|social structures', 'soc. net.', text)
  text <- gsub('resource use efficiency|input use efficiency|crop water productivity|wue', 'rue', text)
  return(text)
}

# ==============================================================================
# SECTION 6: DATA PREPARATION FUNCTION
# ==============================================================================

prepare_domain_data <- function(data, domain) {
  domain_data <- data |> filter(SI_Domain == domain)
  
  pair_counts <- domain_data |>
    group_by(Mechanism, R_Attribute) |>
    tally()
  
  return(list(
    data = domain_data,
    pair_counts = pair_counts
  ))
}

# ==============================================================================
# SECTION 7: OPTIMAL PLACEMENT FUNCTION (MaxMin Algorithm for main text)
# ==============================================================================

find_optimal_placement <- function(candidate_positions, existing_positions, 
                                   min_distance_threshold = 0.8,
                                   n_candidates = 200) {
  
  if(nrow(existing_positions) == 0) {
    return(c(0, 0))
  }
  
  if(nrow(candidate_positions) < 10) {
    best_dist <- -Inf
    best_pos <- candidate_positions[1, ]
    
    for(i in 1:nrow(candidate_positions)) {
      pos <- candidate_positions[i, ]
      min_dist <- min(sqrt(rowSums((existing_positions - matrix(pos, nrow = nrow(existing_positions), ncol = 2, byrow = TRUE))^2)))
      
      if(min_dist > best_dist) {
        best_dist <- min_dist
        best_pos <- pos
      }
    }
    return(best_pos)
  }
  
  if(nrow(candidate_positions) > n_candidates) {
    sampled_indices <- sample(1:nrow(candidate_positions), n_candidates)
    candidates <- candidate_positions[sampled_indices, ]
  } else {
    candidates <- candidate_positions
  }
  
  best_dist <- -Inf
  best_pos <- candidates[1, ]
  
  for(i in 1:nrow(candidates)) {
    pos <- candidates[i, ]
    
    distances <- sqrt(rowSums((existing_positions - matrix(pos, nrow = nrow(existing_positions), ncol = 2, byrow = TRUE))^2))
    min_dist <- min(distances)
    
    boundary_penalty <- 0
    if(abs(pos[1]) > 5) boundary_penalty <- abs(pos[1]) - 5
    if(abs(pos[2]) > 5) boundary_penalty <- max(boundary_penalty, abs(pos[2]) - 5)
    min_dist <- min_dist - boundary_penalty * 0.1
    
    if(min_dist > best_dist) {
      best_dist <- min_dist
      best_pos <- pos
    }
  }
  
  if(best_dist < min_distance_threshold) {
    search_radius <- min_distance_threshold * 0.5
    n_local_candidates <- 50
    
    local_candidates <- matrix(NA, nrow = n_local_candidates, ncol = 2)
    for(i in 1:n_local_candidates) {
      angle <- runif(1, 0, 2*pi)
      radius <- runif(1, 0, search_radius)
      local_candidates[i, ] <- best_pos + c(radius * cos(angle), radius * sin(angle))
    }
    
    for(i in 1:nrow(local_candidates)) {
      pos <- local_candidates[i, ]
      distances <- sqrt(rowSums((existing_positions - matrix(pos, nrow = nrow(existing_positions), ncol = 2, byrow = TRUE))^2))
      min_dist <- min(distances)
      
      if(min_dist > best_dist) {
        best_dist <- min_dist
        best_pos <- pos
      }
    }
  }
  
  return(best_pos)
}

# ==============================================================================
# SECTION 8: PLACE ATTRIBUTES OPTIMALLY (MaxMin placement) - FOR PNG
# ==============================================================================

place_attributes_optimally <- function(attr_to_place, mech_layout, mech_indices,
                                       graph, plot_width = 14, plot_height = 11,
                                       min_distance_threshold = 0.8) {
  
  if(length(attr_to_place) == 0) {
    return(matrix(numeric(0), ncol = 2))
  }
  
  attr_layout <- matrix(NA, nrow = length(attr_to_place), ncol = 2)
  existing_positions <- mech_layout
  
  for(i in 1:length(attr_to_place)) {
    attr_idx <- attr_to_place[i]
    
    connected_mechanisms <- neighbors(graph, attr_idx)
    mech_connected <- connected_mechanisms[which(V(graph)$is_mechanism[connected_mechanisms])]
    
    if(length(mech_connected) > 0) {
      mech_positions <- matrix(NA, nrow = length(mech_connected), ncol = 2)
      for(j in 1:length(mech_connected)) {
        mech_name <- V(graph)$name[mech_connected[j]]
        mech_idx <- which(V(graph)$name[mech_indices] == mech_name)
        if(length(mech_idx) > 0) {
          mech_positions[j, ] <- mech_layout[mech_idx[1], ]
        }
      }
      
      mech_positions <- mech_positions[!is.na(mech_positions[,1]), , drop = FALSE]
      
      if(nrow(mech_positions) > 0) {
        n_candidates <- 300
        candidates <- matrix(NA, nrow = n_candidates, ncol = 2)
        
        base_distance <- 0.3 * min(plot_width, plot_height)
        max_distance <- 0.5 * min(plot_width, plot_height)
        
        for(k in 1:nrow(mech_positions)) {
          mech_pos <- mech_positions[k, ]
          n_per_mech <- n_candidates %/% nrow(mech_positions)
          
          for(j in 1:n_per_mech) {
            idx <- (k-1) * n_per_mech + j
            if(idx > n_candidates) break
            
            angle <- runif(1, 0, 2*pi)
            distance <- base_distance + runif(1, 0, max_distance - base_distance)
            
            candidates[idx, ] <- mech_pos + c(distance * cos(angle), distance * sin(angle))
          }
        }
        
        candidates <- candidates[!is.na(candidates[,1]), , drop = FALSE]
        
        if(nrow(candidates) > 0) {
          best_pos <- find_optimal_placement(
            candidate_positions = candidates,
            existing_positions = existing_positions,
            min_distance_threshold = min_distance_threshold,
            n_candidates = min(200, nrow(candidates))
          )
          
          attr_layout[i, ] <- best_pos
          existing_positions <- rbind(existing_positions, best_pos)
          next
        }
      }
    }
    
    n_candidates <- 500
    candidates <- matrix(NA, nrow = n_candidates, ncol = 2)
    
    for(j in 1:n_candidates) {
      angle <- runif(1, 0, 2*pi)
      radius <- runif(1, 0, 0.7 * min(plot_width, plot_height))
      candidates[j, ] <- c(radius * cos(angle), radius * sin(angle))
    }
    
    best_pos <- find_optimal_placement(
      candidate_positions = candidates,
      existing_positions = existing_positions,
      min_distance_threshold = min_distance_threshold,
      n_candidates = 300
    )
    
    attr_layout[i, ] <- best_pos
    existing_positions <- rbind(existing_positions, best_pos)
  }
  
  return(attr_layout)
}

# ==============================================================================
# SECTION 9: TWO-STAGE LAYOUT (Mechanisms first, then Attributes) - FOR PNG
# ==============================================================================

get_two_stage_layout <- function(graph, plot_width = 14, plot_height = 11) {
  
  if(vcount(graph) == 0) {
    return(matrix(c(0, 0), ncol = 2))
  }
  
  if(vcount(graph) == 1) {
    return(matrix(c(0, 0), ncol = 2))
  }
  
  is_mechanism <- V(graph)$is_mechanism
  is_attribute <- V(graph)$is_attribute
  
  n_mechanisms <- sum(is_mechanism)
  n_attributes <- sum(is_attribute)
  n_nodes <- vcount(graph)
  
  cat(paste0("    Mechanisms: ", n_mechanisms, ", Attributes: ", n_attributes, "\n"))
  
  # Stage 1: Layout Mechanisms in a Circle
  if(n_mechanisms > 0) {
    mech_indices <- which(is_mechanism)
    
    if(length(mech_indices) > 1) {
      mech_subgraph <- induced_subgraph(graph, mech_indices)
      mech_layout <- layout_in_circle(mech_subgraph)
      mech_radius <- min(plot_width, plot_height) * 0.2
      mech_layout <- mech_layout * mech_radius
      cat(paste0("    Stage 1: Spread ", n_mechanisms, " mechanisms in a circle\n"))
    } else {
      mech_layout <- matrix(c(0, 0), ncol = 2)
      cat(paste0("    Stage 1: Single mechanism at center\n"))
    }
  } else {
    mech_indices <- integer(0)
    mech_layout <- matrix(numeric(0), ncol = 2)
  }
  
  # Stage 2: Place Attributes using Optimal Placement (MaxMin)
  if(n_attributes > 0) {
    attr_indices <- which(is_attribute)
    
    cat(paste0("    Stage 2: Placing ", n_attributes, " attributes using MaxMin algorithm\n"))
    
    attr_layout <- place_attributes_optimally(
      attr_to_place = attr_indices,
      mech_layout = mech_layout,
      mech_indices = mech_indices,
      graph = graph,
      plot_width = plot_width,
      plot_height = plot_height,
      min_distance_threshold = 0.8
    )
    
    cat(paste0("    Successfully placed ", nrow(attr_layout), " attributes\n"))
    
  } else {
    attr_indices <- integer(0)
    attr_layout <- matrix(numeric(0), ncol = 2)
  }
  
  # Combine layouts
  full_layout <- matrix(NA, nrow = n_nodes, ncol = 2)
  
  if(n_mechanisms > 0) {
    full_layout[mech_indices, ] <- mech_layout
  }
  
  if(n_attributes > 0) {
    full_layout[attr_indices, ] <- attr_layout
  }
  
  if(any(is.na(full_layout))) {
    full_layout[is.na(full_layout)] <- 0
  }
  
  # Scale to fill plot
  current_spread <- max(dist(full_layout))
  if(current_spread > 0) {
    target_spread <- sqrt(plot_width^2 + plot_height^2) * 0.8
    scale_factor <- target_spread / current_spread
    full_layout <- full_layout * scale_factor
  }
  
  full_layout <- scale(full_layout, scale = FALSE)
  
  return(full_layout)
}

# ==============================================================================
# SECTION 10: CREATE IGRAPH GRAPH WITH WEIGHTS
# ==============================================================================

create_weighted_graph <- function(domain_data, pair_counts) {
  # Extract unique edges
  edges_data <- domain_data |>
    select(Mechanism, R_Attribute) |>
    filter(Mechanism != R_Attribute) |>
    distinct(Mechanism, R_Attribute)
  
  if(nrow(edges_data) == 0) {
    return(NULL)
  }
  
  # Create graph with weights
  edges_with_weights <- edges_data |>
    left_join(pair_counts, by = c("Mechanism", "R_Attribute")) |>
    mutate(weight = ifelse(is.na(n), 1, n))
  
  g <- graph_from_data_frame(edges_with_weights, directed = FALSE)
  E(g)$weight <- edges_with_weights$weight
  
  # Identify node types
  all_mechanisms <- unique(edges_data$Mechanism)
  all_attributes <- unique(edges_data$R_Attribute)
  
  V(g)$is_mechanism <- V(g)$name %in% all_mechanisms
  V(g)$is_attribute <- V(g)$name %in% all_attributes
  V(g)$is_attribute <- V(g)$is_attribute & !V(g)$is_mechanism
  
  return(g)
}

# ==============================================================================
# SECTION 11: CREATE LABELS FOR NODES
# ==============================================================================

create_node_labels <- function(g) {
  node_labels <- V(g)$name
  
  # For mechanisms: assign sequential numbers (1, 2, 3, ...)
  if(any(V(g)$is_mechanism)) {
    mech_names <- V(g)$name[V(g)$is_mechanism]
    mech_numbers <- seq_along(mech_names)
    names(mech_numbers) <- mech_names
    node_labels[V(g)$is_mechanism] <- as.character(mech_numbers[V(g)$name[V(g)$is_mechanism]])
  }
  
  # For attributes: create unique 3-character codes with NUMBER suffixes
  if(any(V(g)$is_attribute)) {
    attr_names <- V(g)$name[V(g)$is_attribute]
    attr_codes <- toupper(substr(attr_names, 1, 3))
    
    code_counts <- table(attr_codes)
    counter <- list()
    
    for(i in seq_along(attr_names)) {
      base_code <- attr_codes[i]
      
      if(is.null(counter[[base_code]])) {
        counter[[base_code]] <- 0
      }
      
      counter[[base_code]] <- counter[[base_code]] + 1
      
      if(counter[[base_code]] == 1) {
        attr_codes[i] <- base_code
      } else {
        attr_codes[i] <- paste0(base_code, counter[[base_code]])
      }
    }
    
    names(attr_codes) <- attr_names
    node_labels[V(g)$is_attribute] <- attr_codes[V(g)$name[V(g)$is_attribute]]
  }
  
  V(g)$label <- node_labels
  V(g)$full_name <- V(g)$name
  
  return(g)
}

# ==============================================================================
# SECTION 12: CREATE STATIC PNG PLOT (Using MaxMin Layout)
# ==============================================================================

create_static_plot <- function(g, domain_name, output_dir) {
  # Set node properties
  V(g)$size <- ifelse(V(g)$is_mechanism, 24, 14)
  V(g)$shape <- ifelse(V(g)$is_mechanism, "circle", "square")
  V(g)$color <- ifelse(V(g)$is_mechanism, "lightblue", "lightgreen")
  
  V(g)$label <- V(g)$label
  V(g)$label.cex <- ifelse(V(g)$is_mechanism, 1.0, 0.7)
  V(g)$label.color <- "black"
  V(g)$label.font <- 2
  V(g)$label.dist <- 0
  
  V(g)$frame.color <- ifelse(V(g)$is_attribute, 'grey80', "black")
  V(g)$frame.width <- ifelse(V(g)$is_attribute, 1.5, 1.5)
  
  # Edge widths
  if(ecount(g) > 0) {
    if("weight" %in% edge_attr_names(g)) {
      edge_weights <- E(g)$weight
      if(length(unique(edge_weights)) == 1) {
        E(g)$width <- 2
      } else {
        min_w <- min(edge_weights)
        max_w <- max(edge_weights)
        if(max_w > min_w) {
          E(g)$width <- 1 + 4 * (edge_weights - min_w) / (max_w - min_w)
        } else {
          E(g)$width <- 2
        }
      }
    }
  }
  
  # Ensure circles on top
  node_order <- order(V(g)$is_mechanism, decreasing = TRUE)
  g <- permute(g, node_order)
  
  # Use the two-stage layout with MaxMin algorithm
  layout <- get_two_stage_layout(g)
  layout <- layout[node_order, ]
  
  # Domain title (only domain name)
  domain_title <- paste0(toupper(substr(domain_name, 1, 1)), 
                         substr(domain_name, 2, nchar(domain_name)), 
                         " Domain")
  
  # Save plot to output directory (PNG only)
  filename <- file.path(output_dir, paste0("network_", gsub(" ", "_", domain_name), ".png"))
  
  png(filename = filename, width = 14, height = 11, units = "in", res = 300)
  par(mar = c(2, 2, 4, 2))
  
  plot(g, layout = layout,
       vertex.label = V(g)$label,
       vertex.label.cex = V(g)$label.cex,
       vertex.label.dist = V(g)$label.dist,
       vertex.label.color = V(g)$label.color,
       vertex.label.font = V(g)$label.font,
       edge.color = "gray30",
       edge.width = E(g)$width,
       vertex.frame.color = V(g)$frame.color,
       vertex.frame.width = V(g)$frame.width,
       main = domain_title,
       cex.main = 1.4)
  
  dev.off()
  
  # Return graph and layout (will be saved to processed directory later)
  return(list(
    graph = g,
    layout = layout,
    n_nodes = vcount(g),
    n_edges = ecount(g),
    n_mechanisms = sum(V(g)$is_mechanism),
    n_attributes = sum(V(g)$is_attribute)
  ))
}

# ==============================================================================
# SECTION 13: CREATE INTERACTIVE 3D NETWORK (all nodes for Suppl materials) 
# ==============================================================================

create_interactive_3d <- function(g, domain_name, output_dir) {
  
  # Create 3D layout using force-directed algorithm (good 3D shape)
  if(vcount(g) > 1) {
    layout_3d <- layout_with_fr(g, dim = 3, niter = 3000)
    # Scale the layout
    max_range <- max(abs(layout_3d))
    if(max_range > 0) {
      layout_3d <- layout_3d / max_range * 10
    }
  } else {
    layout_3d <- matrix(c(0, 0, 0), ncol = 3)
  }
  
  # Prepare node properties
  node_colors <- ifelse(V(g)$is_mechanism, "#4A90D9", "#50C878")
  node_sizes <- ifelse(V(g)$is_mechanism, 1.5, 1.2)
  node_labels <- V(g)$label  # This contains the codes/numbers
  
  # Full names for tooltips
  tooltip_text <- paste0(V(g)$full_name, " (", 
                         ifelse(V(g)$is_mechanism, "Mechanism", "Attribute"), 
                         ") | Label: ", V(g)$label)
  
  # Edge properties
  if(ecount(g) > 0) {
    edge_weights <- E(g)$weight
    if(length(unique(edge_weights)) > 1) {
      edge_width <- 1 + 3 * (edge_weights - min(edge_weights)) / 
        (max(edge_weights) - min(edge_weights))
      edge_alpha <- 0.4 + 0.4 * (edge_weights - min(edge_weights)) / 
        (max(edge_weights) - min(edge_weights))
    } else {
      edge_width <- rep(2, ecount(g))
      edge_alpha <- rep(0.7, ecount(g))
    }
    edge_color <- rgb(0.3, 0.3, 0.8, alpha = edge_alpha)
  } else {
    edge_width <- NULL
    edge_color <- NULL
  }
  
  # Domain title (only domain name)
  domain_title <- paste0(toupper(substr(domain_name, 1, 1)), 
                         substr(domain_name, 2, nchar(domain_name)), 
                         " Domain")
  
  # Create the graphjs widget with CLEAR LABELS
  widget <- graphjs(
    g,
    layout = layout_3d,
    vertex.shape = "sphere",
    vertex.color = node_colors,
    vertex.size = node_sizes,
    vertex.label = node_labels,
    vertex.label.cex = 1.5,      # LARGER label size for visibility
    vertex.label.color = "black",
    vertex.label.font = "bold",
    vertex.label.dist = 0.5,
    vertex.label.offset = 0.3,
    edge.color = edge_color,
    edge.width = edge_width,
    bg = "white",
    main = domain_title,
    tooltip = tooltip_text,
    stroke = TRUE
  )
  
  # Save the widget to output directory (HTML only)
  html_file <- file.path(output_dir, paste0("3D_network_", gsub(" ", "_", domain_name), ".html"))
  saveWidget(widget, file = html_file, selfcontained = TRUE)
  
  cat(paste0("    Interactive 3D network (all nodes) saved: ", html_file, "\n"))
  
  # return(list(
  #   graph = g,
  #   layout_3d = layout_3d,
  #   n_nodes = vcount(g),
  #   n_edges = ecount(g)
  # ))
  return(list(graph = g))
}

# ==============================================================================
# SECTION 14: SAVE INTERMEDIARY FILES TO ../data/processed/
# ==============================================================================

save_intermediary_files <- function(g, layout, layout_3d, domain_name, processed_dir, lookup_table) {
  
  # Save lookup table
  lookup_file <- file.path(processed_dir, paste0("lookup_", gsub(" ", "_", domain_name), ".csv"))
  write.csv(lookup_table, file = lookup_file, row.names = FALSE)
  
  # Save edge information
  if(ecount(g) > 0) {
    edge_info <- data.frame(
      from = get.edgelist(g)[,1],
      to = get.edgelist(g)[,2],
      weight = E(g)$weight,
      width = E(g)$width,
      stringsAsFactors = FALSE
    )
    
    edge_file <- file.path(processed_dir, paste0("edges_", gsub(" ", "_", domain_name), ".csv"))
    write.csv(edge_info, file = edge_file, row.names = FALSE)
  }
  
  # Save graph object
  rds_file <- file.path(processed_dir, paste0("graph_", gsub(" ", "_", domain_name), ".rds"))
  saveRDS(g, file = rds_file)
  
  # Save 2D layout
  layout_file <- file.path(processed_dir, paste0("layout_", gsub(" ", "_", domain_name), ".rds"))
  saveRDS(layout, file = layout_file)
  
  # Save 3D layout
  layout_3d_file <- file.path(processed_dir, paste0("layout_3d_", gsub(" ", "_", domain_name), ".rds"))
  saveRDS(layout_3d, file = layout_3d_file)
}

# ==============================================================================
# SECTION 15: CREATE ALL NETWORKS
# ==============================================================================

create_all_networks <- function(g, domain_name, output_dir, processed_dir) {
  # Create labels
  g <- create_node_labels(g)
  
  # Create static plot (PNG saved to output_dir)
  static_result <- create_static_plot(g, domain_name, output_dir)
  
  # Create 3D version (HTML saved to output_dir)
  threejs_result <- create_interactive_3d(g, domain_name, output_dir)
  
  # Create lookup table
  lookup_table <- data.frame(
    domain = domain_name,
    node_type = ifelse(V(g)$is_mechanism, "Mechanism", "Attribute"),
    display_label = V(g)$label,
    full_name = V(g)$full_name,
    stringsAsFactors = FALSE
  )
  
  return(list(
    static = static_result,
    threejs = threejs_result,
    lookup = lookup_table
  ))
}

# ==============================================================================
# SECTION 16: LOLLIPOP PLOT AND CORRELATION MATRIX
# ==============================================================================

create_supplementary_plots <- function(filtered_dat, output_dir, processed_dir) {
  # Theme settings
  theme_set(theme_minimal() + 
              theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    axis.line = element_line(color = "black", size = 0.5),
                    axis.text.y = element_text(size = 10),
                    axis.text.x = element_text(size = 10),
                    axis.title = element_text(size = 11),
                    panel.border = element_blank()))
  
  # Calculate number of authors per mechanism
  author_count_per_mechanism <- filtered_dat |>
    group_by(Mechanism) |>
    summarise(n_authors = n(), .groups = 'drop') |>
    filter(!is.na(Mechanism), Mechanism != "") |>
    arrange(desc(n_authors))
  
  # Lollipop plot - saved to output directory
  lollipop_plot <- ggplot(author_count_per_mechanism, 
                          aes(x = n_authors, y = reorder(Mechanism, n_authors))) +
    geom_segment(aes(x = 0, xend = n_authors, 
                     y = reorder(Mechanism, n_authors), 
                     yend = reorder(Mechanism, n_authors)),
                 color = "steelblue", size = 0.8) +
    geom_point(color = "steelblue", size = 3) +
    labs(x = "Number of Authors", y = "Mechanism")
  
  ggsave(filename = file.path(output_dir, "lollipop_mechanisms_authors.png"),
         plot = lollipop_plot, width = 7, height = 4, dpi = 300, units = "in")
  
  # Save author counts to processed directory
  write.csv(author_count_per_mechanism, 
            file = file.path(processed_dir, "mechanism_author_counts.csv"), 
            row.names = FALSE)
  
  # Correlation matrix - saved to output directory
  png(file.path(output_dir, 'correlation_matrix.png'), 
      width = 7.5, height = 7.5, units = 'in', res = 1000)
  
  cor_data <- filtered_dat |>
    mutate(Mechanism = gsub(' ', '.', substr(Mechanism, 1, 12))) |>
    select(Author, Mechanism, n) |>
    group_by(Author, Mechanism) |>
    summarize(total_n = sum(n, na.rm = TRUE), .groups = 'drop') |>
    pivot_wider(names_from = Mechanism, values_from = total_n, values_fill = 0) |>
    select(-Author) |>
    cor()
  
  corrplot(cor_data, 
           method = "circle",
           type = "lower",
           diag = FALSE,
           tl.col = "black",
           tl.cex = 0.8,
           cl.cex = 0.8,
           number.cex = 0.7,
           addCoef.col = "black",
           number.digits = 2,
           col = colorRampPalette(c("blue", "white", "red"))(200),
           title = "Mechanism Correlation Matrix",
           mar = c(0, 0, 2, 0))
  
  dev.off()
}

# ==============================================================================
# SECTION 17: MAIN EXECUTION
# ==============================================================================

# Get unique SI domains
si_domains <- unique(filtered_dat$SI_Domain)
# Set preferred order in which graphs will be produced
si_domains <- c("productivity", "social", "human condition", "economic", "environment")

network_results <- list()
all_lookups <- tibble()

cat("\n")
cat("============================================================\n")
cat("Network Analysis \n")
cat("============================================================\n")

for (domain in si_domains) {
  cat(paste0("\n  Processing domain: ", domain, "\n"))
  
  # Prepare data
  domain_info <- prepare_domain_data(filtered_dat, domain)
  domain_data <- domain_info$data
  pair_counts <- domain_info$pair_counts
  
  if(nrow(domain_data) == 0) {
    cat(paste0("    No data for domain: ", domain, "\n"))
    next
  }
  
  cat(paste0("    Data rows: ", nrow(domain_data), "\n"))
  
  # Create weighted graph
  g <- create_weighted_graph(domain_data, pair_counts)
  if(is.null(g)) {
    cat(paste0("    No edges found for domain: ", domain, "\n"))
    next
  }
  
  # Create all networks
  result <- tryCatch({
    create_all_networks(g, domain, dir_output, dir_processed)
  }, error = function(e) {
    cat(paste0("    Error: ", e$message, "\n"))
    return(NULL)
  })
  
  if(!is.null(result)) {
    # Collect lookup table
    if(!is.null(result$lookup)) {
      all_lookups <- rbind(all_lookups, result$lookup)
    }
    
    cat(paste0("    Success! Static: ", result$static$n_nodes, " nodes, ", 
               result$static$n_edges, " edges\n"))
    cat(paste0("    3D (all nodes): ", result$threejs$n_nodes, " nodes\n"))
    
    network_results[[domain]] <- result
  } else {
    cat(paste0("    Failed to create network for domain: ", domain, "\n"))
  }
}

# ==============================================================================
# SECTION 18: CREATE SUPPLEMENTARY PLOTS
# ==============================================================================

create_supplementary_plots(filtered_dat, dir_output, dir_processed)

# ==============================================================================
# SECTION 19: COMBINE LOOKUP TABLES
# ==============================================================================

if(nrow(all_lookups) > 0) {
  unique_lookup <- all_lookups |>
    distinct() |>
    arrange(domain, desc(node_type), full_name)
  
  lookup_file <- file.path(dir_output, "combined_lookup_table.csv")
  write.csv(unique_lookup, file = lookup_file, row.names = FALSE)
  cat(paste0("\n  Combined lookup table saved: ", lookup_file, "\n"))
}


# ==============================================================================
# SECTION 20: HTML INDEX - SAVED TO ../output/
# ==============================================================================

# html_index <- file.path(dir_output, "index.html")
html_index <- "../../output/index.html"
html_files <- list.files(dir_output, pattern = "3D_network_.*\\.html$", full.names = FALSE)

if(length(html_files) > 0) {
  html_content <- '<!DOCTYPE html>
<html>
<head>
    <meta charset="UTF-8">
    <title>Interactive 3D Networks - 10+ Occurrence Filter</title>
    <style>
        body { 
            font-family: "Segoe UI", Arial, sans-serif; 
            margin: 0; 
            padding: 20px; 
            background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%);
            min-height: 100vh;
        }
        .container { 
            max-width: 1400px; 
            margin: 0 auto; 
            padding: 20px;
            background: rgba(255,255,255,0.9);
            border-radius: 15px;
            box-shadow: 0 10px 30px rgba(0,0,0,0.1);
        }
        h1 { 
            color: #2c3e50; 
            font-size: 2.5em;
            margin-bottom: 5px;
            border-bottom: 3px solid #3498db;
            padding-bottom: 10px;
        }
        .subtitle {
            color: #7f8c8d;
            font-size: 1.1em;
            margin-top: -5px;
            margin-bottom: 30px;
        }
        .grid { 
            display: grid; 
            grid-template-columns: repeat(auto-fill, minmax(350px, 1fr)); 
            gap: 25px; 
            margin-top: 20px;
        }
        .card { 
            background: white; 
            border-radius: 12px; 
            padding: 20px; 
            box-shadow: 0 4px 15px rgba(0,0,0,0.08);
            transition: transform 0.3s ease, box-shadow 0.3s ease;
            border: 1px solid #e8ecf1;
        }
        .card:hover { 
            transform: translateY(-5px); 
            box-shadow: 0 8px 25px rgba(0,0,0,0.15);
        }
        .card h3 { 
            margin: 0 0 8px 0; 
            color: #2c3e50;
            font-size: 1.3em;
        }
        .card .stats { 
            font-size: 0.9em; 
            color: #7f8c8d; 
            margin: 5px 0 12px 0;
            padding: 8px;
            background: #f8f9fa;
            border-radius: 6px;
        }
        .card .link { 
            display: inline-block; 
            text-decoration: none; 
            color: white; 
            background: #3498db; 
            padding: 10px 20px; 
            border-radius: 6px; 
            font-weight: 600;
            transition: background 0.3s ease;
            margin-top: 5px;
        }
        .card .link:hover { 
            background: #2980b9; 
        }
        .badge {
            display: inline-block;
            padding: 2px 10px;
            border-radius: 12px;
            font-size: 0.75em;
            font-weight: 600;
            margin-right: 5px;
        }
        .badge-mechanism { background: #4A90D9; color: white; }
        .badge-attribute { background: #50C878; color: white; }
        .instructions {
            background: #f8f9fa;
            padding: 15px 20px;
            border-radius: 8px;
            margin: 25px 0;
            border-left: 4px solid #3498db;
        }
        .instructions ul {
            margin: 5px 0;
            padding-left: 20px;
        }
        .instructions li {
            margin: 5px 0;
            color: #34495e;
        }
        .footer {
            margin-top: 30px;
            padding-top: 20px;
            border-top: 1px solid #e8ecf1;
            color: #95a5a6;
            text-align: center;
            font-size: 0.9em;
        }
        .legend-icon {
            display: inline-block;
            width: 14px;
            height: 14px;
            border-radius: 50%;
            margin-right: 5px;
            vertical-align: middle;
        }
        .highlight {
            background: #fff3cd;
            padding: 2px 6px;
            border-radius: 4px;
            font-weight: bold;
        }
    </style>
</head>
<body>
    <div class="container">
        <h1>🌐 Interactive 3D Network Visualizations</h1>
        <div class="subtitle">10+ Occurrence Filter • Sustainable Intensification Mechanisms</div>
        
        <div class="instructions">
            <strong>🖱️ How to interact with the 3D networks:</strong>
            <ul>
                <li><strong>Drag</strong> with mouse to rotate the network in 3D</li>
                <li><strong>Scroll</strong> to zoom in and out</li>
                <li><strong>Hover</strong> over nodes to see full names and codes</li>
                <li><strong><span class="legend-icon" style="background:#4A90D9;"></span> Blue spheres</strong> = Mechanisms (numbered 1, 2, 3...)</li>
                <li><strong><span class="legend-icon" style="background:#50C878;"></span> Green spheres</strong> = Attributes (3-letter codes)</li>
                <li><strong>Edge thickness</strong> represents frequency of co-occurrence</li>
                <li>The <span class="highlight">codes/numbers</span> are clearly displayed on each node</li>
                <li><strong>Full network</strong> showing all nodes</li>
            </ul>
        </div>
        
        <div class="grid">'
  
  # Sort files
  html_files <- sort(html_files)
  
  for (file in html_files) {
    # Extract domain name
    domain <- gsub("3D_network_|\\.html", "", file)
    domain <- gsub("_", " ", domain)
    domain_formatted <- paste0(toupper(substr(domain, 1, 1)), substr(domain, 2, nchar(domain)))
    
    # Get stats
    stats_text <- ""
    mech_count <- 0
    attr_count <- 0
    
    for (domain_name in names(network_results)) {
      if (grepl(gsub(" ", "_", domain_name), file)) {
        result <- network_results[[domain_name]]$static
        if(!is.null(result)) {
          stats_text <- paste0("Nodes: ", result$n_nodes, " | Edges: ", result$n_edges)
          mech_count <- result$n_mechanisms
          attr_count <- result$n_attributes
        }
        break
      }
    }
    
    html_content <- paste0(html_content, '
        <div class="card">
            <h3>', domain_formatted, '</h3>
            <div class="stats">
                <span class="badge badge-mechanism">🔵 ', mech_count, ' M</span>
                <span class="badge badge-attribute">🟩 ', attr_count, ' A</span>
                <span style="margin-left: 8px;">', stats_text, '</span>
            </div>
            <a href="', file, '" target="_blank" class="link">🔬 Open 3D Network</a>
        </div>')
  }
  
  html_content <- paste0(html_content, '
        </div>
        <div class="footer">
            <p>All networks include weighted edges (thicker = stronger relationship)</p>
            <p>PNG uses MaxMin placement (non-overlapping nodes)</p>
            <p>3D uses force-directed layout (all nodes shown)</p>
            <p>Generated with R • ', format(Sys.Date(), "%B %d, %Y"), '</p>
            <p style="font-size: 0.85em; margin-top: 10px;">
                📁 Output: ../output/ • 📊 Intermediary files: ../data/processed/
            </p>
        </div>
    </div>
</body>
</html>')
  
  writeLines(html_content, html_index)
  cat(paste0("\n  HTML index saved: ", html_index, "\n"))
}

# ==============================================================================
# SECTION 21: REPORT OF SCRIPT COMPLETION
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("ANALYSIS COMPLETED SUCCESSFULLY!\n")
cat("============================================================\n")
cat(paste0("Output directory (PNG, HTML, summaries): ", dir_output, "\n"))
cat(paste0("Intermediary directory (RDS, CSV, edge info): ", dir_processed, "\n"))
cat(paste0("Number of domains processed: ", length(network_results), "\n"))
cat("\nDomain summaries:\n")

if(length(network_results) > 0) {
  for (domain in names(network_results)) {
    result <- network_results[[domain]]
    cat(paste0("  - ", domain, ": ", 
               result$static$n_nodes, " nodes (static), ",
               result$threejs$n_nodes, " nodes (3D)\n"))
  }
}

cat("\n")
cat("FILES IN ../output/:\n")
cat("    - network_*.png (static plots using MaxMin layout)\n")
cat("    - 3D_network_*.html (interactive 3D networks - all nodes)\n")
cat("    - index.html (navigation page)\n")
cat("    - combined_lookup_table.csv\n")
cat("    - lollipop_mechanisms_authors.png\n")
cat("    - correlation_matrix.png\n")
cat("\n")
cat("FILES IN ../data/processed/:\n")
cat("    - graph_*.rds (graph objects)\n")
cat("    - layout_*.rds (2D layouts)\n")
cat("    - layout_3d_*.rds (3D layouts)\n")
cat("    - edges_*.csv (edge information)\n")
cat("    - lookup_*.csv (individual domain lookups)\n")
cat("    - mechanism_author_counts.csv\n")
cat("\n")
cat("KEY FEATURES:\n")
cat("  - PNG: MaxMin placement (mechanisms in circle, attributes placed optimally)\n")
cat("  - 3D: Force-directed layout for good 3D visualization (ALL nodes shown)\n")
cat("  - Codes/numbers clearly visible on 3D nodes (vertex.label.cex = 1.5)\n")
cat("  - Mechanisms: Blue spheres numbered 1, 2, 3...\n")
cat("  - Attributes: Green spheres with 3-letter codes\n")
cat("  - Edge thickness = co-occurrence frequency\n")
cat("============================================================\n")

# END OF SCRIPT
# ==============================================================================
"# test" 
