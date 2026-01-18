# ============================================================================
# PAKISTANI PARLIAMENT COAUTHORSHIP NETWORK ANALYSIS
# Comprehensive Network Visualization and Analysis Script
# ============================================================================

# Install and load required packages
if (!require(igraph)) install.packages("igraph")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr)) install.packages("dplyr")
if (!require(tidyr)) install.packages("tidyr")
if (!require(RColorBrewer)) install.packages("RColorBrewer")
if (!require(gridExtra)) install.packages("gridExtra")
if (!require(scales)) install.packages("scales")
if (!require(ggrepel)) install.packages("ggrepel")
install.packages("poweRlaw")

library(ggrepel)
library(igraph)
library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(gridExtra)
library(scales)
library(poweRlaw)    # for power-law fitting
library(gridExtra)
library(grid)
library(gtable)

# Set working directory and create output folders
dir.create("output", showWarnings = FALSE)
dir.create("output/graphs", showWarnings = FALSE)
dir.create("output/data", showWarnings = FALSE)

# ============================================================================
# 1. DATA LOADING AND NETWORK CREATION
# ============================================================================

cat("Loading network data\n")

# Load edges and nodes
edges <- read.csv("edges_cosponsorship.csv", stringsAsFactors = FALSE)
nodes <- read.csv("nodes_politicians.csv", stringsAsFactors = FALSE)

# Create directed graph (even though edges are undirected, we'll convert)
g_early <- graph_from_data_frame(edges[,c("Source", "Target", "Weight")], 
                           directed = FALSE, 
                           vertices = nodes)

# Add node attributes
V(g_early)$party <- nodes$Latest_Party[match(V(g_early)$name, nodes$Id)]
V(g_early)$total_bills <- nodes$Total_Bills[match(V(g_early)$name, nodes$Id)]
V(g_early)$terms <- nodes$Terms_Count[match(V(g_early)$name, nodes$Id)]

# Add edge attributes
E(g_early)$weight <- edges$Weight
E(g_early)$same_party <- edges$Same_Party_Now
E(g_early)$cross_party <- edges$Cross_Party_Then

cat("Network loaded successfully!\n")
cat("Nodes:", vcount(g_early), "\n")
cat("Edges:", ecount(g_early), "\n\n")

cat("Removing isolated nodes (degree = 0)\n")
isolates <- which(degree(g_early) == 0)
cat("Found", length(isolates), "isolated nodes. Removing them for centrality analysis.\n")

# Create a subgraph without isolates
g <- delete_vertices(g_early, isolates)

# Now recalculate everything on the cleaned network
cat("Nodes in main component (after removing isolates):", vcount(g), "\n")
cat("Edges:", ecount(g), "\n\n")

# ============================================================================
# 2. NETWORK METRICS CALCULATION
# ============================================================================

cat("Calculating network metrics\n")

# Basic metrics
density <- edge_density(g)
diameter <- diameter(g, directed = FALSE)
avg_path_length <- mean_distance(g, directed = FALSE)
clustering_coef <- transitivity(g, type = "global")
avg_local_clustering <- transitivity(g, type = "average")

# Degree statistics
degrees <- degree(g)
mean_degree <- mean(degrees)
median_degree <- median(degrees)

# Centrality measures
betweenness <- betweenness(g, normalized = TRUE)
closeness <- closeness(g, normalized = TRUE)
eigenvector <- eigen_centrality(g)$vector
pagerank <- page_rank(g)$vector

# Add to vertices
V(g)$degree <- degrees
V(g)$betweenness <- betweenness
V(g)$closeness <- closeness
V(g)$eigenvector <- eigenvector
V(g)$pagerank <- pagerank

# Print key metrics
cat("\n=== GLOBAL NETWORK METRICS ===\n")
cat(sprintf("Density: %.4f\n", density))
cat(sprintf("Diameter: %d\n", diameter))
cat(sprintf("Average Path Length: %.2f\n", avg_path_length))
cat(sprintf("Global Clustering Coefficient: %.4f\n", clustering_coef))
cat(sprintf("Average Local Clustering: %.4f\n", avg_local_clustering))
cat(sprintf("Mean Degree: %.2f\n", mean_degree))
cat(sprintf("Median Degree: %.0f\n", median_degree))

# ============================================================================
# 3. PARTY ANALYSIS
# ============================================================================

cat("\n=== PARTY-LEVEL ANALYSIS ===\n")

# Party statistics
party_stats <- nodes %>%
  group_by(Latest_Party) %>%
  summarise(
    Members = n(),
    Avg_Bills = mean(Total_Bills, na.rm = TRUE),
    Total_Bills = sum(Total_Bills, na.rm = TRUE)
  ) %>%
  arrange(desc(Members))

print(party_stats)

# Cross-party collaboration
cross_party_edges <- sum(edges$Cross_Party_Then > 0)
same_party_edges <- sum(edges$Same_Party_Now > 0)
cat(sprintf("\nCross-party collaborations: %d (%.1f%%)\n", 
            cross_party_edges, 100*cross_party_edges/nrow(edges)))
cat(sprintf("Same-party collaborations: %d (%.1f%%)\n", 
            same_party_edges, 100*same_party_edges/nrow(edges)))

# ============================================================================
# 4. EXPORT CENTRALITY DATA
# ============================================================================

centrality_df <- data.frame(
  Legislator = V(g)$name,
  Party = V(g)$party,
  Degree = V(g)$degree,
  Betweenness = V(g)$betweenness,
  Closeness = V(g)$closeness,
  Eigenvector = V(g)$eigenvector,
  PageRank = V(g)$pagerank,
  Total_Bills = V(g)$total_bills,
  stringsAsFactors = FALSE
)

write.csv(centrality_df, "output/data/centrality_measures.csv", row.names = FALSE)

# Top influencers
cat("\n=== TOP 10 MOST INFLUENTIAL LEGISLATORS ===\n")
cat("\nBy Degree Centrality:\n")
print(head(centrality_df[order(-centrality_df$Degree), c("Legislator", "Party", "Degree")], 10))

cat("\nBy Betweenness (Brokers):\n")
print(head(centrality_df[order(-centrality_df$Betweenness), c("Legislator", "Party", "Betweenness")], 10))

cat("\nBy PageRank:\n")
print(head(centrality_df[order(-centrality_df$PageRank), c("Legislator", "Party", "PageRank")], 10))

# ============================================================================
# 5. VISUALIZATION SETUP
# ============================================================================

# Party color palette (major parties)
party_colors <- c(
  "PTI" = "#1B5E20",      # Dark Green
  "PML-N" = "#0D47A1",    # Dark Blue
  "PPPP" = "#B71C1C",     # Dark Red
  "MQM" = "#E65100",      # Dark Orange
  "JUI" = "#4A148C",      # Purple
  "JI" = "#006064",       # Teal
  "MMA" = "#5D4037",      # Brown
  "BAP" = "#F57F17",      # Yellow
  "IND" = "#757575",      # Gray
  "Other" = "#BDBDBD"     # Light Gray
)

# Assign colors to vertices
V(g)$color <- ifelse(V(g)$party %in% names(party_colors),
                     party_colors[V(g)$party],
                     party_colors["Other"])

# ============================================================================
# 6. MAIN NETWORK VISUALIZATION
# ============================================================================

cat("\nGenerating main network visualization\n")

png("output/graphs/01_main_network.png", width = 4000, height = 3000, res = 300)

# Size by degree, capped for visibility
node_sizes <- scales::rescale(V(g)$degree, to = c(2, 15))

# Layout
set.seed(123)
layout <- layout_with_fr(g, niter = 500)

par(mar = c(1, 1, 3, 1), bg = "white")
plot(g,
     layout = layout,
     vertex.size = node_sizes,
     vertex.color = V(g)$color,
     vertex.frame.color = NA,
     vertex.label = ifelse(V(g)$degree > quantile(V(g)$degree, 0.95), 
                           V(g)$name, NA),
     vertex.label.cex = 0.5,
     vertex.label.color = "black",
     vertex.label.family = "sans",
     edge.width = 0.3,
     edge.color = rgb(0.5, 0.5, 0.5, 0.2),
     edge.curved = 0.1,
     main = "Pakistani Parliamentary Coauthorship Network\nLegislators colored by party affiliation",
     cex.main = 1.2)

# Legend
legend("topright",
       legend = names(party_colors),
       fill = party_colors,
       title = "Political Party",
       cex = 0.8,
       bty = "n")

dev.off()

# ============================================================================
# 7. Small-World Check, LOCAL CLUSTERING COEFFICIENT DISTRIBUTION & METRICS SUMMARY
# ============================================================================

# Path Length vs. Clustering Coefficient (Small-World Check):
cat("Generating small-world comparison bar plots with fixed Y-scale\n")
set.seed(123)
n_nodes <- vcount(g)
n_edges <- ecount(g)
# --- Random graph (Erdős–Rényi) with same number of edges ---
g_random <- sample_gnm(n_nodes, m = n_edges, directed = FALSE)
L_random <- mean_distance(g_random)
C_random <- transitivity(g_random, type = "global")
# --- Regular lattice (ring) with similar average degree ---
k_reg <- max(2, round(mean_degree))
if (k_reg %% 2!= 0) k_reg <- k_reg + 1 # ensure even for lattice
g_lattice <- sample_k_regular(n_nodes, k_reg, directed = FALSE)
L_lattice <- mean_distance(g_lattice)
C_lattice <- transitivity(g_lattice, type = "global")
# --- Observed network ---
L_obs <- avg_path_length
C_obs <- clustering_coef
# --- Small-world index σ = (C/C_rand) / (L/L_rand) ---
sigma <- (C_obs / C_random) / (L_obs / L_random)
# Data for plot (Original wide format)
sw_df <- data.frame(
  Network = c("Observed\n(Pakistan Parliament)", "Random", "Regular\n(Lattice)"),
  Clustering = c(C_obs, C_random, C_lattice),
  PathLength = c(L_obs, L_random, L_lattice),
  Type = c("Observed", "Random", "Regular")
)
# Reshape data into long format for facet_wrap bar plot
sw_df_long <- data.frame(
  Network = rep(sw_df$Network, 2),
  Type = rep(sw_df$Type, 2),
  Metric = c(rep("Clustering Coefficient (C)", 3), rep("Average Path Length (L)", 3)),
  Value = c(sw_df$Clustering, sw_df$PathLength)
)


png("output/graphs/07_small_world_fixed_scale.png", width = 3500, height = 2500, res = 300)

# Determine the overall maximum value to set a fixed scale for both plots
max_y_value <- max(sw_df_long$Value) * 1.1 # Add 10% padding

ggplot(sw_df_long, aes(x = Network, y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  # Add text labels for clear value display
  geom_text(aes(label = round(Value, 3)), 
            position = position_dodge(width = 0.8), vjust = -0.5, size = 4, fontface = "bold") +
  # Use a fixed scale for the Y-axis across both facets, as requested
  facet_wrap(~ Metric, scales = "fixed") +
  # Set the fixed Y-limit based on the largest value
  coord_cartesian(ylim = c(0, max_y_value)) + 
  scale_fill_manual(values = c("Observed" = "#E74C3C", "Random" = "#3498DB", "Regular" = "#2ECC71")) +
  labs(
    title = "Network Model Comparison: Small-World Metrics",
    subtitle = paste0("Small-world index (σ = ", round(sigma, 2), ")"),
    x = "",
    y = "Value",
    fill = "Network Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "darkgray"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5),
    panel.grid.major.x = element_blank(),
    # Add separating line between facets (panel border)
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5) 
  )

dev.off()
cat(sprintf("Small-world index (σ): %.3f\n", sigma))

# Summary Key Network Metrics:

cat("Generating network metrics summary\n")

metrics_df <- data.frame(
  Metric = c("Average Degree", "Density", "Avg Path Length", "Clustering Coef"),
  Value = c(mean_degree, density, avg_path_length, clustering_coef),
  Label = c(sprintf("%.2f", mean_degree),
            sprintf("%.4f", density),
            sprintf("%.2f", avg_path_length),
            sprintf("%.4f", clustering_coef))
)

print(metrics_df)

# LOCAL CLUSTERING COEFFICIENT DISTRIBUTION
cat("Generating local clustering distribution\n")

# Local clustering coefficients
local_clustering <- transitivity(g, type = "local", isolates = "zero")
V(g)$local_clust <- local_clustering

# Global clustering coefficient (from your earlier code)
global_clust <- transitivity(g, type = "global")  # Already computed as clustering_coef
global_clust <- round(global_clust, 3)

# Remove NA
clust_data <- data.frame(local_clustering = local_clustering[!is.na(local_clustering)])

# Auto Y-limit to remove empty space
bin_counts <- table(cut(clust_data$local_clustering, breaks = seq(0, 1, 0.05)))
y_max <- quantile(bin_counts, 0.95) * 1.3
y_max <- ceiling(y_max / 5) * 5

# Plot
png("output/graphs/09_clustering_distribution.png", 
    width = 3000, height = 2000, res = 300)

ggplot(clust_data, aes(x = local_clustering)) +
  geom_histogram(binwidth = 0.05, fill = "#8E24AA", color = "white", alpha = 0.9,
                 boundary = 0) +
  coord_flip() +
  
  # Cap Y-axis
  scale_y_continuous(
    limits = c(0, y_max),
    breaks = seq(0, y_max, by = 5),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  # === GLOBAL CLUSTERING LINE (solid blue) ===
  geom_vline(xintercept = global_clust, 
             color = "#1976D2", linetype = "solid", size = 1.2) +
  
  # === LABEL at Y = 25, aligned with global line ===
  annotate("text", 
           x = global_clust, 
           y = 20, 
           label = paste("Global Clustering =", global_clust), 
           color = "#1976D2", 
           hjust = -0.1, 
           vjust = -1, 
           size = 5.5, 
           fontface = "bold") +
  
  labs(
    title = "Distribution of Local Clustering Coefficients",
    subtitle = "Global clustering coefficient shown for comparison",
    x = "Local Clustering Coefficient",
    y = "Number of Legislators"
  ) +
  
  scale_x_continuous(breaks = seq(0, 1, 0.1)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.minor = element_blank(),
    axis.title.y = element_text(margin = margin(r = 12))
  )

dev.off()

cat(sprintf("Global clustering coefficient: %.3f\n", global_clust))

# ============================================================================
# 8. PARTY COLLABORATION HEATMAP
# ============================================================================

cat("Generating party collaboration heatmap\n")

# Create party-to-party collaboration matrix
party_collab <- edges %>%
  left_join(nodes %>% select(Id, Latest_Party), by = c("Source" = "Id")) %>%
  rename(Party_Source = Latest_Party) %>%
  left_join(nodes %>% select(Id, Latest_Party), by = c("Target" = "Id")) %>%
  rename(Party_Target = Latest_Party) %>%
  filter(!is.na(Party_Source) & !is.na(Party_Target))

# Focus on major parties
major_parties <- party_stats %>% filter(Members >= 5) %>% pull(Latest_Party)

collab_matrix <- party_collab %>%
  filter(Party_Source %in% major_parties & Party_Target %in% major_parties) %>%
  group_by(Party_Source, Party_Target) %>%
  summarise(Total_Weight = sum(Weight), .groups = "drop")

png("output/graphs/03_party_collaboration_heatmap.png", width = 3000, height = 2500, res = 300)

ggplot(collab_matrix, aes(x = Party_Source, y = Party_Target, fill = Total_Weight)) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(label = Total_Weight), color = "white", size = 4) +
  scale_fill_gradient(low = "#3498DB", high = "#E74C3C", 
                      trans = "log10",
                      labels = comma) +
  labs(title = "Cross-Party Collaboration Intensity",
       subtitle = "Total co-sponsorship weight between parties",
       x = "Party", y = "Party",
       fill = "Collaboration\nWeight") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", size = 16),
        panel.grid = element_blank())

dev.off()

# ============================================================================
# 9. TOP INFLUENCERS BAR CHARTS
# ============================================================================

cat("Generating top influencers charts\n")

# Top 20 by degree
top_degree <- centrality_df %>%
  arrange(desc(Degree)) %>%
  head(20)

png("output/graphs/04_top_degree_centrality.png", width = 3000, height = 2500, res = 300)

ggplot(top_degree, aes(x = reorder(Legislator, Degree), y = Degree, fill = Party)) +
  geom_col(alpha = 0.9) +
  scale_fill_manual(values = party_colors) +
  coord_flip() +
  labs(title = "Top 20 Legislators by number of Unique Co-sponsors (Degree Centrality)",
       subtitle = "Most connected legislators in the network",
       x = "", y = "Degree Centrality") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 16),
        legend.position = "right")

dev.off()

# Top 20 by betweenness (brokers)
top_betweenness <- centrality_df %>%
  arrange(desc(Betweenness)) %>%
  head(20)

png("output/graphs/05_top_betweenness_brokers.png", width = 3000, height = 2500, res = 300)

ggplot(top_betweenness, aes(x = reorder(Legislator, Betweenness), 
                            y = Betweenness, fill = Party)) +
  geom_col(alpha = 0.9) +
  scale_fill_manual(values = party_colors) +
  coord_flip() +
  labs(title = "Top 20 Coalition Builders (Betweenness Centrality)",
       subtitle = "Legislators bridging different groups in the network",
       x = "", y = "Betweenness Centrality") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 16),
        legend.position = "right")

dev.off()

# Get degree of each betweeness point
betweeness_data <- data.frame(
  Legislator = V(g)$name,
  Party = V(g)$party,
  Degree = V(g)$degree,
  Betweenness = V(g)$betweenness,
  stringsAsFactors = FALSE
) %>%
  arrange(desc(betweenness)) %>%
  head(20)  # Top 20

write.csv(betweeness_data, "output/data/top_20_betweeness_centrality.csv", row.names = FALSE)

# ============================================================================
# 10. ARTICULATION POINTS ANALYSIS
# ============================================================================

cat("\nComputing articulation points\n")

# Find articulation points (nodes whose removal increases number of components)
articulation_nodes <- articulation_points(g)

# Get degree of each articulation point
ap_data <- data.frame(
  Legislator = V(g)$name[articulation_nodes],
  Party = V(g)$party[articulation_nodes],
  Degree = V(g)$degree[articulation_nodes],
  stringsAsFactors = FALSE
) %>%
  arrange(desc(Degree)) %>%
  head(20)  # Top 20

# Save data
write.csv(ap_data, "output/data/top_20_articulation_points.csv", row.names = FALSE)

# Plot: Top 20 Articulation Points by Degree
cat("Generating top 20 articulation points plot\n")

png("output/graphs/08_top_articulation_points.png", width = 3000, height = 2500, res = 300)

ggplot(ap_data, aes(x = reorder(Legislator, Degree), y = Degree, fill = Party)) +
  geom_col(width = 0.7, alpha = 0.9) +
  scale_fill_manual(values = party_colors, na.value = "#BDBDBD") +
  coord_flip() +
  labs(
    title = "Top 20 Articulation Points by Degree",
    subtitle = "Critical legislators: removal disconnects the coauthorship network",
    x = "Legislator",
    y = "Degree (Number of Co-sponsors)",
    fill = "Party"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray50"),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    axis.text.y = element_text(size = 10)
  )

dev.off()

cat("Top 20 articulation points saved and plotted.\n")
print(ap_data[, c("Legislator", "Party", "Degree")])

# ============================================================================
# 11.1 EIGENVECTOR CENTRALITY DISTRIBUTION
# ============================================================================

cat("\nGenerating eigenvector centrality distribution\n")

# Extract eigenvector centrality values
eigen_values <- V(g)$eigenvector

# Remove NA (if any) and create data frame
eigen_data <- data.frame(
  Eigenvector = eigen_values[!is.na(eigen_values)]
)

# Compute key stats
mean_eigen <- round(mean(eigen_values, na.rm = TRUE), 5)
median_eigen <- round(median(eigen_values, na.rm = TRUE), 5)
top_eigen <- round(max(eigen_values, na.rm = TRUE), 5)

# Auto-determine reasonable y-limit (95th percentile + buffer)
bin_heights <- table(cut(eigen_data$Eigenvector, breaks = 30))
y_max <- quantile(bin_heights, 0.95) * 1.3
y_max <- ceiling(y_max / 5) * 5

# Save full eigenvector data
eigen_full <- data.frame(
  Legislator = V(g)$name,
  Party = V(g)$party,
  Eigenvector = round(eigen_values, 6)
) %>% arrange(desc(Eigenvector))

write.csv(eigen_full, "output/data/eigenvector_centrality_full.csv", row.names = FALSE)

# Plot: Histogram of Eigenvector Centrality
png("output/graphs/10_eigenvector_distribution.png", width = 3000, height = 2200, res = 300)

ggplot(eigen_data, aes(x = Eigenvector)) +
  geom_histogram(binwidth = 0.01, fill = "#9C27B0", color = "white", alpha = 0.9,
                 boundary = 0) +
  
  # Mean and median lines
  geom_vline(xintercept = mean_eigen, color = "#E91E63", linetype = "dashed", size = 1) +
  geom_vline(xintercept = median_eigen, color = "#388E3C", linetype = "dotted", size = 1) +
  
  # Labels inside plot
  annotate("text", x = mean_eigen + 0.02, y = y_max * 0.9,
           label = paste("Mean =", mean_eigen), color = "#E91E63", hjust = 0, size = 5) +
  annotate("text", x = median_eigen + 0.02, y = y_max * 0.75,
           label = paste("Median =", median_eigen), color = "#388E3C", hjust = 0, size = 5) +
  
  # Highlight top eigenvector
  annotate("text", x = top_eigen, y = y_max * 0.98,
           label = paste("Top =", top_eigen), color = "#D81B60", hjust = 1.1, size = 5, fontface = "bold") +
  
  scale_y_continuous(
    limits = c(0, y_max),
    breaks = seq(0, y_max, by = 5),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_x_continuous(breaks = seq(0, 1, 0.1)) +
  
  labs(
    title = "Distribution of Eigenvector Centrality",
    subtitle = "Influence derived from connections to influential legislators",
    x = "Eigenvector Centrality",
    y = "Number of Legislators"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray50"),
    panel.grid.minor = element_blank(),
    axis.title.y = element_text(margin = margin(r = 12))
  )

dev.off()

cat("Eigenvector centrality distribution saved: 10_eigenvector_distribution.png\n")
cat(sprintf("Top eigenvector: %.5f | Mean: %.5f | Median: %.5f\n", top_eigen, mean_eigen, median_eigen))

# ============================================================================
# 11.2 EIGENVECTOR CENTRALITY - Top 20
# ============================================================================

cat("\nGenerating top 20 eigenvector centrality hierarchy\n")

# Extract valid eigenvector values 
eigen_values <- V(g)$eigenvector
valid_idx <- which(!is.na(eigen_values))
eigen_clean <- eigen_values[valid_idx]

# Full ranked data 
eigen_ranked <- data.frame(
  Legislator = V(g)$name[valid_idx],
  Party = V(g)$party[valid_idx],
  Eigenvector = round(eigen_clean, 6),
  stringsAsFactors = FALSE
) %>%
  arrange(desc(Eigenvector))

# Top 20 only (non-zero) 
top20 <- eigen_ranked %>%
  head(20) %>%
  mutate(
    Label = paste0(Legislator, "\n(", Party, ")"),
    Rank = row_number()
  )

# Save full ranking + top 20 
write.csv(eigen_ranked, "output/data/eigenvector_ranking_full.csv", row.names = FALSE)
write.csv(top20, "output/data/top_20_eigenvector.csv", row.names = FALSE)

# Plot: Top 20 Hierarchy 
png("output/graphs/10_eigenvector_top20_hierarchy.png", width = 3400, height = 2400, res = 300)

ggplot(top20, aes(x = reorder(Label, Rank), y = Eigenvector)) +
  geom_col(aes(fill = Party), width = 0.7, alpha = 0.9) +
  geom_text(aes(label = round(Eigenvector, 4)), 
            hjust = -0.3, size = 4.2, fontface = "bold", color = "black") +
  
  scale_fill_manual(values = party_colors, na.value = "#BDBDBD") +
  scale_y_continuous(breaks = seq(0, 1, 0.1), expand = expansion(mult = c(0, 0.15))) +
  
  coord_flip() +
  
  labs(
    title = "Top 20 Legislators by Eigenvector Centrality",
    subtitle = "Influence derived from connections to influential co-sponsors",
    x = "",
    y = "Eigenvector Centrality",
    fill = "Party"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray50"),
    axis.text.y = element_text(size = 10.5),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    plot.margin = margin(10, 20, 10, 10)
  )

dev.off()

cat("Top 20 eigenvector hierarchy saved: 10_eigenvector_top20_hierarchy.png\n")
print(top20[, c("Legislator", "Party", "Eigenvector")])

# ============================================================================
# 12. BETWEENNESS CENTRALITY - VIOLIN PLOT BY PARTY
# ============================================================================

png("output/graphs/12_betweenness_violin.png", width = 3200, height = 2400, res = 300)

centrality_df %>%
  filter(!is.na(Betweenness), Party %in% names(party_colors)) %>%
  ggplot(aes(x = reorder(Party, Betweenness, median), y = Betweenness, fill = Party)) +
  geom_violin(alpha = 0.8, trim = FALSE) +
  geom_boxplot(width = 0.2, alpha = 0.7, outlier.alpha = 0) +
  scale_fill_manual(values = party_colors) +
  scale_y_log10(labels = comma) +
  labs(title = "Betweenness Centrality Distribution by Party",
       subtitle = "Violin shows density; box shows quartiles (log scale)",
       x = "Party", y = "Betweenness Centrality (log10)") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

dev.off()

# ============================================================================
# 13. COMMUNITY DETECTION
# ============================================================================

cat("Performing community detection\n")

# Fast Greedy community detection
communities <- cluster_fast_greedy(g, weights = E(g)$weight)
V(g)$community <- membership(communities)

cat(sprintf("Detected %d communities\n", length(communities)))

# Community sizes
comm_sizes <- table(V(g)$community)
cat("Community sizes:\n")
print(sort(comm_sizes, decreasing = TRUE))

# ============================================================================
# 14. CROSS-PARTY BROKER IDENTIFICATION
# ============================================================================

cat("\nIdentifying cross-party brokers\n")

# Calculate cross-party edge proportion for each node
cross_party_prop <- sapply(V(g), function(v) {
  neighbors <- neighbors(g, v)
  if(length(neighbors) == 0) return(0)
  
  v_party <- V(g)$party[v]
  neighbor_parties <- V(g)$party[neighbors]
  
  sum(neighbor_parties != v_party) / length(neighbors)
})

V(g)$cross_party_prop <- cross_party_prop

# Identify brokers: high betweenness + high cross-party connections
brokers <- centrality_df %>%
  mutate(CrossPartyProp = cross_party_prop) %>%
  filter(Betweenness > quantile(Betweenness, 0.75) & 
           CrossPartyProp > quantile(CrossPartyProp, 0.75)) %>%
  arrange(desc(Betweenness))

cat("\n TOP CROSS-PARTY BROKERS \n")
print(brokers[, c("Legislator", "Party", "Betweenness", "CrossPartyProp")])

write.csv(brokers, "output/data/cross_party_brokers.csv", row.names = FALSE)

# ============================================================================
# 15. PARTY ISOLATION/INTEGRATION ANALYSIS
# ============================================================================

cat("\nAnalyzing party integration patterns\n")

party_integration <- nodes %>%
  filter(Latest_Party %in% major_parties) %>%
  left_join(centrality_df, by = c("Id" = "Legislator")) %>%
  mutate(CrossPartyProp = cross_party_prop[match(Id, V(g)$name)]) %>%
  group_by(Latest_Party.x) %>%
  summarise(
    Members = n(),
    Avg_Degree = mean(Degree, na.rm = TRUE),
    Avg_Betweenness = mean(Betweenness, na.rm = TRUE),
    Avg_CrossPartyProp = mean(CrossPartyProp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Avg_CrossPartyProp))

cat("\n PARTY INTEGRATION SCORES \n")
print(party_integration)

write.csv(party_integration, "output/data/party_integration.csv", row.names = FALSE)

# ============================================================================
# 16. NETWORK SUMMARY STATISTICS
# ============================================================================

cat("\nExporting comprehensive network statistics\n")

summary_stats <- data.frame(
  Metric = c("Total Legislators", "Total Collaborations", "Network Density",
             "Average Degree", "Diameter", "Average Path Length",
             "Clustering Coefficient", "Number of Communities",
             "Cross-Party Edges (%)", "Same-Party Edges (%)"),
  Value = c(vcount(g), ecount(g), round(density, 4),
            round(mean_degree, 2), diameter, round(avg_path_length, 2),
            round(clustering_coef, 4), length(communities),
            round(100*cross_party_edges/nrow(edges), 1),
            round(100*same_party_edges/nrow(edges), 1))
)

write.csv(summary_stats, "output/data/network_summary.csv", row.names = FALSE)

# ============================================================================
# 17. GIANT COMPONENT
# ============================================================================
# 1. Find all components
components <- clusters(g)

# 2. Size of the giant component (number of nodes/legislators)
giant_size <- max(components$csize)
cat("Size of the giant component:", giant_size, "legislators\n")

# 3. Total number of nodes in the full graph (for percentage)
total_nodes <- vcount(g)
cat("Percentage of legislators in the giant component:", 
    round(100 * giant_size / total_nodes, 2), "%\n")

# 4. (Optional) Create the subgraph and double-check
giant_component_id <- which.max(components$csize)        # which cluster is the biggest
giant_vertices     <- which(components$membership == giant_component_id)

g_giant <- induced_subgraph(g, giant_vertices)

cat("Double-check: vcount(g_giant) =", vcount(g_giant), "\n")

fg_community <- cluster_fast_greedy(g_giant)
wt_community = cluster_walktrap(g_giant)
le_community = cluster_leading_eigen(g_giant)
lp_community = cluster_label_prop(g_giant)

g_density <- edge_density(g_giant)
print(g_density)

# calculate modularity
print(paste("Modularity of Fast Greedy", modularity(fg_community)))
print(paste("Modularity of Walktrap", modularity(wt_community)))
print(paste("Modularity of Leading Eigenvector", modularity(le_community)))
print(paste("Modularity of Label Propagation", modularity(lp_community)))

cat(sprintf("Detected %d communities\n", length(fg_community)))

# ============================================================================
# 17.5 SAVE COMMUNITY MEMBERSHIP + PARTY COMPOSITION (GIANT COMPONENT)
# ============================================================================
cat("\n=== Saving Community Membership and Party Composition ===\n")

# --- Use Fast-Greedy (your best modularity algorithm from earlier) ---
# (You already ran: fg_community <- cluster_fast_greedy(g_giant))

# Fix: you wrote "communities" at the end — should be fg_community
cat(sprintf("Fast-Greedy detected %d communities (modularity = %.4f)\n", 
            length(fg_community), modularity(fg_community)))

# --- Assign community ID to each vertex in the giant component ---
V(g_giant)$community_fg <- membership(fg_community)

# --- Create full membership table ---
membership_df <- data.frame(
  Legislator = V(g_giant)$name,
  Party      = V(g_giant)$party,
  Degree     = degree(g_giant),
  Community_ID = membership(fg_community),
  stringsAsFactors = FALSE
)

# Order by Community_ID then Party
membership_df <- membership_df[order(membership_df$Community_ID, membership_df$Party), ]

# Add readable community labels (C1, C2, ...)
membership_df$Community_Label <- paste0("C", sprintf("%02d", membership_df$Community_ID))

# Reorder columns for clarity
membership_df <- membership_df[, c("Community_Label", "Community_ID", 
                                   "Legislator", "Party", "Degree")]

# --- Save full membership list ---
write.csv(membership_df, "output/data/community_membership_giant.csv", row.names = FALSE)
cat("Full community membership saved → output/data/community_membership_giant.csv\n")

# --- Summary: Size and dominant party of each community ---
comm_summary <- membership_df %>%
  group_by(Community_ID, Community_Label) %>%
  summarise(
    Size = n(),
    Dominant_Party = names(sort(table(Party), decreasing = TRUE))[1],
    Party_Diversity = length(unique(Party)),
    Top_Parties = paste(names(sort(table(Party), decreasing = TRUE)[1:3]), collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(desc(Size))

# Add rank
comm_summary$Rank <- 1:nrow(comm_summary)

# Reorder columns
comm_summary <- comm_summary[, c("Rank", "Community_Label", "Community_ID", 
                                 "Size", "Dominant_Party", "Party_Diversity", "Top_Parties")]

print(comm_summary)

# Save summary
write.csv(comm_summary, "output/data/community_summary_giant.csv", row.names = FALSE)
cat("Community summary saved → output/data/community_summary_giant.csv\n")

# --- Quick top 10 largest communities ---
cat("\nTop 10 Largest Communities:\n")
print(head(comm_summary, 10))

# COMMUNITY DETECTION VISUALIZATION – FAST-GREEDY ON GIANT COMPONENT

cat("\n=== Plotting Fast-Greedy Communities on Giant Component ===\n")

# Ensure we have a good layout (Fruchterman-Reingold works well for ~300 nodes)
set.seed(123)  # for reproducibility
layout_fg <- layout_with_fr(g_giant)

# Color palette: use a qualitative palette that handles many communities
n_comm <- length(fg_community)
if (n_comm <= 10) {
  comm_colors <- RColorBrewer::brewer.pal(max(3, n_comm), "Set3")
} else {
  comm_colors <- sample(rainbow(n_comm))  # fallback for many communities
}
vertex_colors <- comm_colors[membership(fg_community)]

# ------------------------------------------------------------------
# 1. Plot the full network with community colors
# ------------------------------------------------------------------
png("output/graphs/18_fast_greedy_communities_giant.png", 
    width = 3800, height = 3200, res = 320)

par(mar = c(2, 2, 4, 10))  # make room for legend on the right

plot(g_giant,
     layout = layout_fg,
     vertex.color = vertex_colors,
     vertex.size = 8,
     vertex.label = NA,                    # remove labels (too crowded)
     edge.width = 0.8,
     edge.color = adjustcolor("gray70", alpha.f = 0.4),
     main = "Fast-Greedy Communities in the Giant Component\n(Pakistani Parliamentary Coauthorship Network)",
     mark.groups = fg_community,           # optional: draw faint borders around communities
     mark.col = adjustcolor(comm_colors, alpha.f = 0.15),
     mark.border = adjustcolor(comm_colors, alpha.f = 0.3))

# Add legend outside the plot
legend("right", 
       legend = paste("Community", 1:n_comm, paste0("(n=", sizes(fg_community), ")")),
       col = comm_colors, 
       pch = 19, 
       pt.cex = 2,
       cex = 0.9,
       inset = -0.18,      # push legend outside plot area
       xpd = TRUE,
       bty = "n")

title(sub = paste("Modularity =", round(modularity(fg_community), 4), 
                  "| Communities =", n_comm, 
                  "| Nodes =", vcount(g_giant)),
      adj = 0, line = 2, cex.sub = 0.9)

dev.off()

cat("Fast-Greedy community plot saved: output/graphs/18_fast_greedy_communities_giant.png\n")

# ------------------------------------------------------------------
# 2. Plot the dendrogram (hierarchical structure)
# ------------------------------------------------------------------
png("output/graphs/18_fast_greedy_dendrogram_giant.png", 
    width = 3600, height = 2400, res = 300)

plot(fg_community, 
     g_giant,
     layout = layout_fg,
     vertex.color = vertex_colors,
     vertex.size = 6,
     vertex.label = NA,
     main = "Fast-Greedy Dendrogram – Giant Component",
     sub = paste("Modularity =", round(modularity(fg_community), 4)))

dev.off()

cat("Fast-Greedy dendrogram saved: output/graphs/18_fast_greedy_dendrogram_giant.png\n")

# ============================================================================
# 18. NETWORK MODEL COMPARISON – ON GIANT COMPONENT ONLY
# ============================================================================

cat("\n=== NETWORK MODEL COMPARISON (GIANT COMPONENT ONLY) ===\n")
set.seed(123)

# ───── Extract Giant Component (new variables – safe from original g) ─────
comp <- clusters(g)
giant_id <- which.max(comp$csize)                          # ID of largest cluster
giant_vertices <- which(comp$membership == giant_id)       # vertices in giant comp
g_giant <- induced_subgraph(g, giant_vertices)             # the giant subgraph

cat("Giant component size:", vcount(g_giant), "nodes /", ecount(g_giant), "edges\n")

# ───── Observed metrics – GIANT COMPONENT ONLY ─────
n_nodes_g <- vcount(g_giant)
n_edges_g <- ecount(g_giant)
obs_path_length_g   <- mean_distance(g_giant, directed = FALSE)
obs_clustering_g    <- transitivity(g_giant, type = "global")
obs_degree_g        <- degree(g_giant)

# 1–3. Generate the three models (same n & m as giant component)
cat("Generating models with giant component parameters...\n")

g_er_g <- sample_gnm(n_nodes_g, n_edges_g, directed = FALSE)
er_path_length_g <- mean_distance(g_er_g, directed = FALSE)
er_clustering_g  <- transitivity(g_er_g, type = "global")
er_degree_g      <- degree(g_er_g)

k_ws_g <- round(2 * n_edges_g / n_nodes_g)
if (k_ws_g %% 2 != 0) k_ws_g <- k_ws_g + 1
if (k_ws_g < 2) k_ws_g <- 2
g_ws_g <- sample_smallworld(dim = 1, size = n_nodes_g, nei = k_ws_g/2, p = 0.1)
ws_path_length_g <- mean_distance(g_ws_g, directed = FALSE)
ws_clustering_g  <- transitivity(g_ws_g, type = "global")
ws_degree_g      <- degree(g_ws_g)

m_ba_g <- max(1, round(n_edges_g / (n_nodes_g - 1)))
g_ba_g <- sample_pa(n_nodes_g, m = m_ba_g, directed = FALSE)
ba_path_length_g <- mean_distance(g_ba_g, directed = FALSE)
ba_clustering_g  <- transitivity(g_ba_g, type = "global")
ba_degree_g      <- degree(g_ba_g)

# 4. SAFE Power-Law Fitting 
fit_power_law_safe <- function(deg_seq, model_name = "") {
  deg_seq <- deg_seq[deg_seq > 0]
  if (length(deg_seq) < 10) {
    return(list(alpha = NA, xmin = NA, ks_stat = NA))
  }
  tryCatch({
    pl <- displ$new(deg_seq)
    est <- estimate_xmin(pl)
    pl$setXmin(est)
    ks <- if (!is.null(est$KS)) est$KS else NA
    list(alpha = round(pl$pars, 3),
         xmin = pl$xmin,
         ks_stat = round(ks, 4))
  }, error = function(e) {
    message("Power-law fit failed for ", model_name, ": ", e$message)
    list(alpha = NA, xmin = NA, ks_stat = NA)
  })
}

cat("Fitting power-law distributions (giant component)...\n")
pl_obs_g <- fit_power_law_safe(obs_degree_g, "Observed (Giant)")
pl_er_g  <- fit_power_law_safe(er_degree_g,  "Erdős-Rényi")
pl_ws_g  <- fit_power_law_safe(ws_degree_g,  "Watts-Strogatz")
pl_ba_g  <- fit_power_law_safe(ba_degree_g,  "Barabási-Albert")

# 5. Community detection (giant component)
cat("Running Fast Greedy on giant component models...\n")
comm_obs_g <- cluster_fast_greedy(g_giant)
comm_er_g  <- cluster_fast_greedy(g_er_g)
comm_ws_g  <- cluster_fast_greedy(g_ws_g)
comm_ba_g  <- cluster_fast_greedy(g_ba_g)


mod_obs_g <- round(modularity(comm_obs_g), 4)
mod_er_g  <- round(modularity(comm_er_g), 4)
mod_ws_g  <- round(modularity(comm_ws_g), 4)
mod_ba_g  <- round(modularity(comm_ba_g), 4)

# 6. FINAL COMPARISON TABLE (GIANT COMPONENT)
comparison_table_g <- data.frame(
  Metric = c(
    "Number of Nodes (n)",
    "Number of Edges (m)",
    "Average Degree",
    "Average Path Length",
    "Clustering Coefficient",
    "Diameter",
    "Power-Law α (gamma)",
    "Power-Law xmin",
    "Power-Law KS Statistic",
    "Number of Communities (Fast Greedy)",
    "Modularity"
  ),
  `Pakistani Parliament\n(Giant Component)` = c(
    n_nodes_g,
    n_edges_g,
    round(mean(obs_degree_g), 3),
    round(obs_path_length_g, 3),
    round(obs_clustering_g, 4),
    diameter(g_giant, directed = FALSE),
    pl_obs_g$alpha,
    pl_obs_g$xmin %||% "—",
    pl_obs_g$ks_stat %||% "—",
    length(comm_obs_g),
    mod_obs_g
  ),
  `Erdős-Rényi (Random)` = c(
    vcount(g_er_g), ecount(g_er_g),
    round(mean(er_degree_g), 3),
    round(er_path_length_g, 3),
    round(er_clustering_g, 4),
    diameter(g_er_g, directed = FALSE),
    pl_er_g$alpha,
    pl_er_g$xmin %||% "—",
    pl_er_g$ks_stat %||% "—",
    length(comm_er_g),
    mod_er_g
  ),
  `Watts-Strogatz (Small-World)` = c(
    vcount(g_ws_g), ecount(g_ws_g),
    round(mean(ws_degree_g), 3),
    round(ws_path_length_g, 3),
    round(ws_clustering_g, 4),
    diameter(g_ws_g, directed = FALSE),
    pl_ws_g$alpha,
    pl_ws_g$xmin %||% "—",
    pl_ws_g$ks_stat %||% "—",
    length(comm_ws_g),
    mod_ws_g
  ),
  `Barabási-Albert (Scale-Free)` = c(
    vcount(g_ba_g), ecount(g_ba_g),
    round(mean(ba_degree_g), 3),
    round(ba_path_length_g, 3),
    round(ba_clustering_g, 4),
    diameter(g_ba_g, directed = FALSE),
    pl_ba_g$alpha,
    pl_ba_g$xmin %||% "—",
    pl_ba_g$ks_stat %||% "—",
    length(comm_ba_g),
    mod_ba_g
  ),
  stringsAsFactors = FALSE
)

print(comparision_table_g)

`%||%` <- function(a, b) ifelse(is.na(a) | is.null(a), b, a)

colnames(comparison_table_g)[2] <- "Pakistani Parliament\n(Giant Component)"

write.csv(comparison_table_g, "output/data/network_model_comparison_giant.csv", row.names = FALSE)
cat("Comparison table (giant) saved: output/data/network_model_comparison_giant.csv\n")
print(comparison_table_g)

# Visualizations

# 7. Bar chart
plot_data_g <- data.frame(
  Metric = rep(c("Avg Path Length", "Clustering Coef", "Modularity"), each = 4),
  Model = rep(c("Pakistani\nParliament\n(Giant)", "Erdős-Rényi\n(Random)",
                "Watts-Strogatz\n(Small-World)", "Barabási-Albert\n(Scale-Free)"), 3),
  Value = c(
    obs_path_length_g, er_path_length_g, ws_path_length_g, ba_path_length_g,
    obs_clustering_g,er_clustering_g,ws_clustering_g, ba_clustering_g,
    mod_obs_g,mod_er_g,mod_ws_g,mod_ba_g
  )
)
plot_data_g$Model <- factor(plot_data_g$Model,
                            levels = c("Pakistani\nParliament\n(Giant)", "Erdős-Rényi\n(Random)",
                                       "Watts-Strogatz\n(Small-World)", "Barabási-Albert\n(Scale-Free)"))

# 1. Calculate the global maximum value for a fixed scale (e.g., max path length)
max_y_value <- max(plot_data_g$Value, na.rm = TRUE) * 1.1 


png("output/graphs/13_network_model_comparison_giant.png", width = 4000, height = 2500, res = 300)

ggplot(plot_data_g, aes(x = Model, y = Value, fill = Model)) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_text(aes(label = round(Value, 3)), vjust = -0.5, size = 4, fontface = "bold") +
  
  # 2. Use 'scales = "fixed"' to enforce the same Y-axis across all three plots
  facet_wrap(~Metric, scales = "fixed", ncol = 3) + 
  # 3. Explicitly set the Y-limits based on the largest value (Avg Path Length)
  coord_cartesian(ylim = c(0, max_y_value)) +
  
  scale_fill_manual(values = c("#E74C3C", "#3498DB", "#2ECC71", "#9B59B6")) +
  labs(title = "Network Model Comparison: Structural Properties",
       subtitle = paste0("Y-axis scale is fixed (0 to ", round(max_y_value, 1), 
                         ") across all facets to accurately represent the magnitude differences between metrics."),
       x = "", y = "Value") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", size = 16),
        axis.text.x = element_text(size = 10),
        legend.position = "none",
        # 4. Add panel border for separation line
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5))
dev.off()

# 8. Degree density
degree_comparison_g <- data.frame(
  Degree = c(obs_degree_g, er_degree_g, ws_degree_g, ba_degree_g),
  Model = rep(c("Pakistani Parliament (Giant)", "Erdős-Rényi", "Watts-Strogatz", "Barabási-Albert"),
              times = c(length(obs_degree_g), length(er_degree_g), length(ws_degree_g), length(ba_degree_g)))
)

png("output/graphs/14_degree_distribution_comparison_giant.png", width = 4000, height = 2500, res = 300)
ggplot(degree_comparison_g, aes(x = Degree, fill = Model, color = Model)) +
  geom_density(alpha = 0.4, size = 1) +
  scale_fill_manual(values = c("#E74C3C", "#3498DB", "#2ECC71", "#9B59B6")) +
  scale_color_manual(values = c("#E74C3C", "#3498DB", "#2ECC71", "#9B59B6")) +
  labs(title = "Degree Distribution (Giant Component Only)", x = "Degree", y = "Density") +
  theme_minimal(base_size = 14) + theme(legend.position = "bottom")
dev.off()

# 9. Log-log plot
get_degree_freq <- function(deg, model_name) {
  df <- as.data.frame(table(deg))
  colnames(df) <- c("Degree", "Frequency")
  df$Degree <- as.numeric(as.character(df$Degree))
  df$Model <- model_name
  df <- df[df$Degree > 0 & df$Frequency > 0, ]
  df
}

deg_freq_all_g <- rbind(
  get_degree_freq(obs_degree_g, "Pakistani Parliament (Giant)"),
  get_degree_freq(er_degree_g,  "Erdős-Rényi"),
  get_degree_freq(ws_degree_g,  "Watts-Strogatz"),
  get_degree_freq(ba_degree_g,  "Barabási-Albert")
)

png("output/graphs/15_loglog_degree_distribution_giant.png", width = 4000, height = 2500, res = 300)
ggplot(deg_freq_all_g, aes(x = Degree, y = Frequency, color = Model)) +
  geom_point(alpha = 0.7, size = 2.5) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  scale_x_log10() + scale_y_log10() +
  scale_color_manual(values = c("#E74C3C", "#3498DB", "#2ECC71", "#9B59B6")) +
  labs(title = "Log-Log Degree Distribution (Giant Component)", color = "Model") +
  theme_minimal(base_size = 14) + theme(legend.position = "bottom")
dev.off()


# Final summary
cat("\n=== GIANT COMPONENT MODEL COMPARISON COMPLETE ===\n")

# ============================================================================
# 19. HOMOPHILY ANALYSIS: E-I INDEX + VISUALIZATIONS
# ============================================================================
cat("\n=== Calculating E-I Index and Generating Homophily Visualizations ===\n")

# Ensure we're working on the giant component
if (!exists("g_giant")) stop("g_giant not found. Run giant component code first.")

# Required vertex attributes: party (mandatory)
if (is.null(V(g_giant)$party)) stop("Vertex attribute 'party' is missing!")


# DEFINE VALID PARTIES - Only these will appear in visualizations

valid_parties <- c("IND", "JUI", "MMA", "MQM", "PML-N", "PPPP", "PTI", "SIC")

# Check what parties actually exist in the data
cat("Parties found in g_giant:\n")
print(table(V(g_giant)$party))

# Standardize party names (in case of variations like "SCI" vs "SIC")
V(g_giant)$party <- gsub("SCI", "SIC", V(g_giant)$party)  # Fix typo if present

# Map any party not in valid_parties to "Other" or NA
V(g_giant)$party_clean <- ifelse(V(g_giant)$party %in% valid_parties, 
                                 V(g_giant)$party, 
                                 NA)

cat("\nParties after cleaning (only valid parties):\n")
print(table(V(g_giant)$party_clean, useNA = "ifany"))


# Get vertex names properly

vertex_names <- V(g_giant)$name
if (is.null(vertex_names) || all(is.na(vertex_names))) {
  vertex_names <- paste("MP", 1:vcount(g_giant), sep = "")
  V(g_giant)$name <- vertex_names
}


# E-I Index Calculation (using cleaned party labels)

ei_scores <- numeric(vcount(g_giant))

for (i in 1:vcount(g_giant)) {
  neighbors_idx <- neighbors(g_giant, i)
  if (length(neighbors_idx) == 0) {
    ei_scores[i] <- NA
    next
  }
  own_party <- V(g_giant)$party_clean[i]
  if (is.na(own_party)) {
    ei_scores[i] <- NA
    next
  }
  neighbor_parties <- V(g_giant)$party_clean[neighbors_idx]
  
  # Only count neighbors with valid parties
  neighbor_parties <- neighbor_parties[!is.na(neighbor_parties)]
  if (length(neighbor_parties) == 0) {
    ei_scores[i] <- NA
    next
  }
  
  internal <- sum(neighbor_parties == own_party, na.rm = TRUE)
  external <- sum(neighbor_parties != own_party, na.rm = TRUE)
  
  ei_scores[i] <- ifelse((internal + external) == 0, NA, 
                         (external - internal) / (external + internal))
}

V(g_giant)$ei_index <- ei_scores

# Network-wide E-I Index
total_cross <- sum(E(g_giant)$same_party == FALSE, na.rm = TRUE)
total_same  <- sum(E(g_giant)$same_party == TRUE, na.rm = TRUE)
network_ei  <- (total_cross - total_same) / (total_cross + total_same)

cat(sprintf("\nNetwork-wide E-I Index: %.4f (%.1f%% cross-party ties)\n", 
            network_ei, 100 * total_cross / (total_cross + total_same)))


# Create data frame with CLEANED party labels

ei_df <- data.frame(
  Name      = vertex_names,
  Party     = V(g_giant)$party_clean,
  Degree    = degree(g_giant),
  EI_Index  = ei_scores,
  stringsAsFactors = FALSE
)

# Save individual scores (full data)
write.csv(ei_df, "output/data/ei_index_individual_scores.csv", row.names = FALSE)

# Filter to only valid parties for plotting
ei_df_plot <- ei_df %>%
  filter(Party %in% valid_parties & !is.na(EI_Index))

cat("E-I index scores saved. Verifying data:\n")
cat(sprintf("  Unique names in CSV: %d\n", length(unique(ei_df$Name))))
cat(sprintf("  Total rows: %d\n", nrow(ei_df)))
cat(sprintf("  Rows with valid parties for plotting: %d\n", nrow(ei_df_plot)))

# VISUALIZATION 4: Party-to-Party Homophily Heatmap (ONLY VALID PARTIES)

cat("Generating party-to-party heatmap (valid parties only)...\n")

# Use cleaned party labels
party_labels_clean <- V(g_giant)$party_clean

# Filter to only valid parties
valid_idx <- which(party_labels_clean %in% valid_parties)
party_table_clean <- table(party_labels_clean[valid_idx])
unique_parties_clean <- names(party_table_clean)

# Get edge endpoints
edge_list <- ends(g_giant, E(g_giant), names = FALSE)

# Initialize observed matrix with only valid parties
observed <- matrix(0, nrow = length(unique_parties_clean), ncol = length(unique_parties_clean),
                   dimnames = list(unique_parties_clean, unique_parties_clean))

# Fill observed matrix
for (i in 1:nrow(edge_list)) {
  idx1 <- edge_list[i, 1]
  idx2 <- edge_list[i, 2]
  p1 <- party_labels_clean[idx1]
  p2 <- party_labels_clean[idx2]
  
  # Skip if either party is NA or not in valid parties
  if (is.na(p1) || is.na(p2)) next
  if (!(p1 %in% unique_parties_clean) || !(p2 %in% unique_parties_clean)) next
  
  observed[p1, p2] <- observed[p1, p2] + 1
  if (p1 != p2) {
    observed[p2, p1] <- observed[p2, p1] + 1
  }
}

# Expected under random mixing (only for valid party nodes)
n_valid <- length(valid_idx)
m_valid <- sum(observed) / 2  # Divide by 2 since we counted both directions
party_sizes_clean <- as.numeric(party_table_clean[unique_parties_clean])
names(party_sizes_clean) <- unique_parties_clean

expected <- matrix(0, nrow = length(unique_parties_clean), ncol = length(unique_parties_clean),
                   dimnames = list(unique_parties_clean, unique_parties_clean))

for (i in 1:length(unique_parties_clean)) {
  for (j in 1:length(unique_parties_clean)) {
    ni <- party_sizes_clean[i]
    nj <- party_sizes_clean[j]
    if (i == j) {
      expected[i, j] <- 2 * m_valid * (ni * (ni - 1) / 2) / (n_valid * (n_valid - 1) / 2)
    } else {
      expected[i, j] <- 2 * m_valid * (ni * nj) / (n_valid * (n_valid - 1))
    }
  }
}

# Compute ratio
ratio_mat <- observed / expected
ratio_mat[is.na(ratio_mat)] <- 0
ratio_mat[is.infinite(ratio_mat)] <- 0

# Convert to long format
ratio_df <- as.data.frame(as.table(ratio_mat))
colnames(ratio_df) <- c("From", "To", "Ratio")

# VISUALIZATION 4: Party-to-Party Homophily Heatmap (ENHANCED COLORS)

png("output/graphs/22_party_homophily_heatmap_fixed2.png", width = 3600, height = 3200, res = 320)

# The ratio_df contains the Observed / Expected ratios.
# We need to find the maximum value to set intelligent limits.
max_ratio_observed <- max(ratio_df$Ratio, na.rm = TRUE)
# Set a visual cap to prevent the diagonal from washing out the other cells.
# If max_ratio_observed is, for example, 30, we'll cap the color scale at 10.
# The text labels will still show the true values.
visual_cap <- min(max_ratio_observed, 10) 

ggplot(ratio_df, aes(x = To, y = From, fill = Ratio)) +
  geom_tile(color = "white", size = 0.8) +
  # Ensure text labels remain visible (black text should contrast well)
  geom_text(aes(label = round(Ratio, 2)), color = "black", size = 4) +
  scale_fill_gradient2(
    # Set blue for low ratio (<1, avoidance/polarization)
    low = "#3498DB", 
    # Set white for ratio near 1 (random chance/neutral)
    mid = "white", 
    # Set dark red/maroon for high ratio (>1, homophily/alliance)
    high = "#8E0000", 
  ) +
  
  labs(title = "Party-to-Party Homophily: Observed vs Expected Collaboration",
       subtitle = paste0("Ratio > 1 = Homophily (more ties than expected) | Ratio < 1 = Avoidance (fewer ties). (Color Scale Capped at ", round(visual_cap, 1), ")"),
       fill = "Observed /\nExpected Ratio",
       x = "Co-Sponsor Party", y = "Sponsor Party") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
        axis.text.y = element_text(size = 11),
        plot.title = element_text(face = "bold", size = 16))
dev.off()

cat("Saved: output/graphs/22_party_homophily_heatmap_fixed2.png\n")

# Final Summary
cat("\n=== Homophily Analysis Complete ===\n")
cat("Valid parties included:", paste(valid_parties, collapse = ", "), "\n")
cat("Files saved:\n")
cat(" - output/data/ei_index_individual_scores.csv\n")
cat(" - output/graphs/19_ei_index_by_party.png\n")
cat(" - output/graphs/20_ei_vs_degree_scatter.png\n")
cat(" - output/graphs/21_network_homophily_edges.png\n")
cat(" - output/graphs/22_party_homophily_heatmap.png\n")

# ============================================================================
# COMPLETION MESSAGE
# ============================================================================

cat("\n" , rep("=", 70), "\n", sep = "")
cat("ANALYSIS COMPLETE!\n")
cat(rep("=", 70), "\n\n", sep = "")
cat("Generated outputs:\n")
cat("  - 9 visualization plots in output/graphs/\n")  # ← Now 9
cat("  - 7 data files in output/data/\n")              # ← Now 7
cat("\nKey findings:\n")
cat(sprintf("  - Network has %.1f%% cross-party collaboration\n", 
            100*cross_party_edges/nrow(edges)))
cat(sprintf("  - Average legislator has %.1f co-sponsors\n", mean_degree))
cat(sprintf("  - %d distinct communities detected\n", length(communities)))
cat(sprintf("  - %d identified as key cross-party brokers\n", nrow(brokers)))
cat(sprintf("  - Only %d true articulation points\n", length(articulation_nodes)))
cat(sprintf("  - Top degree: %s (%s, %d)\n", 
            top_degree_full$Legislator[1], top_degree_full$Party[1], top_degree_full$Degree[1]))
cat(sprintf("  - Top eigenvector: %.5f\n", top_eigen))
cat("\n" , rep("=", 70), "\n", sep = "")