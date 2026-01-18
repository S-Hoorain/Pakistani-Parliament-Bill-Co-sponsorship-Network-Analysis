# Pakistani Parliament Co-Sponsorship Network Analysis

## Overview

This R project performs a rigorous **Social Network Analysis (SNA)** of the Pakistani Parliament based on bill co-sponsorship data. The script reconstructs the legislative network to measure political polarization, identify cross-party brokers, and test the network against theoretical models (Small-World, Scale-Free).

It goes beyond basic metrics by isolating the **Giant Component** for robust statistical testing and calculating the **E-I Index** to quantify homophily (the tendency of legislators to collaborate only within their own party).

## Key Features

### 1. Advanced Network Metrics
* **Centrality Measures:** Calculates Degree, Betweenness (Brokers), Eigenvector (Influence), and PageRank.
* **Articulation Points:** Identifies "keystone" legislators whose removal would fragment the network.
* **Party Integration:** Generates heatmaps of cross-party collaboration.

### 2. Statistical Model Testing
* **Small-Worldness:** Compares the observed network against random (Erdős–Rényi) and regular lattice models to calculate the Small-World Index ($\sigma$).
* **Scale-Free Fit:** Fits the degree distribution to a Power-Law and compares it against Barabási-Albert and Watts-Strogatz models using the Kolmogorov-Smirnov test.

### 3. Sociometric Analysis
* **Homophily (E-I Index):** Calculates Krackhardt’s E-I Index to measure internal vs. external party ties.
* **Community Detection:** Uses the Fast-Greedy algorithm to detect modular communities within the Giant Component.

---

## Prerequisites

Ensure you have **R (4.0+)** installed. The script automatically installs missing packages, but relies on:

* `igraph` (Network manipulation)
* `poweRlaw` (Statistical fitting)
* `ggplot2` & `ggrepel` (Visualization)
* `dplyr` & `tidyr` (Data manipulation)
* `gridExtra` (Composite plots)

## Input Data Requirements

The script expects two CSV files in the root directory:

### 1. `edges_cosponsorship.csv`
Defines the connections between legislators.
* **Source:** ID of legislator A
* **Target:** ID of legislator B
* **Weight:** Strength of connection (number of co-sponsored bills)
* **Same_Party_Now:** Boolean/Integer (optional for specific stats)
* **Cross_Party_Then:** Boolean/Integer (optional for specific stats)

### 2. `nodes_politicians.csv`
Defines the attributes of the legislators.
* **Id:** Matching the Source/Target IDs.
* **Latest_Party:** Political party affiliation (e.g., PTI, PML-N, PPPP).
* **Total_Bills:** Total bills sponsored.
* **Terms_Count:** Number of terms served.

---

## Output Structure

The script automatically creates an `output/` directory with the following structure:

### 📂 `output/graphs/` (9 Visualizations)
| File | Description |
|------|-------------|
| `01_main_network.png` | Force-directed graph colored by party. |
| `03_party_collaboration_heatmap.png` | Matrix showing collaboration intensity between parties. |
| `07_small_world_fixed_scale.png` | Comparison of clustering/path length vs Random models. |
| `10_eigenvector_distribution.png` | Histogram of influence scores. |
| `13_network_model_comparison_giant.png` | Bar chart comparing the Giant Component to theoretical models. |
| `15_loglog_degree_distribution_giant.png` | Log-Log plot checking for Power-Law behavior. |
| `18_fast_greedy_communities_giant.png` | Visualization of detected communities. |
| `22_party_homophily_heatmap_fixed2.png` | Observed vs. Expected collaboration ratios. |

### 📂 `output/data/` (7 Datasets)
* **Centrality & Rankings:** `centrality_measures.csv`, `top_20_betweeness_centrality.csv`, `top_20_eigenvector.csv`.
* **Structural Analysis:** `articulation_points.csv`, `cross_party_brokers.csv`.
* **Model Data:** `network_model_comparison_giant.csv` (Statistical fit results).
* **Homophily:** `ei_index_individual_scores.csv` (Polarization scores per MP).

---

## How to Run

1. Place `edges_cosponsorship.csv` and `nodes_politicians.csv` in your project folder.
2. Open the script in RStudio.
3. Run the entire script (`Ctrl + Alt + R` or `Cmd + Shift + R`).
4. Check the `output/` folder for results.

## Notes on Methodology
* **Isolates:** Isolated nodes (legislators with 0 connections) are removed before calculating global metrics to prevent skewing path lengths.
* **Giant Component:** Model comparisons (Scale-Free/Small-World) are performed strictly on the Giant Component to ensure mathematical validity.
