# analysis.R

# Climate Migration Agent-Based Model (ABM) - Comprehensive Analysis and Experimentation

# Load common libraries
# Ensure these packages are installed: install.packages(c("igraph", "ggplot2", "dplyr", "tidyr", "parallel", "foreach", "doParallel", "gridExtra", "viridis"))
library(igraph)
library(ggplot2)
library(dplyr)
library(tidyr)
library(parallel) # For parallel computing utilities
library(foreach)  # For parallel loops
library(doParallel) # Backend for foreach package
library(gridExtra) # For arranging multiple ggplot objects on a grid (dashboard)
library(viridis)   # For color palettes in visualizations

# Set random seed for reproducibility of simulation results
set.seed(42)

# =============================================================================
# SOURCE ALL R FUNCTIONS FROM 'R/' DIRECTORY
# =============================================================================

# This section loads all function definitions from the 'R/' subdirectory.
# Ensure that the 'R' directory exists in your working directory and
# contains all the necessary .R files for the model.
source("R/params.R") # checked
source("R/network.R") # checked
source("R/agents.R") # checked
source("R/disaster.R") # checked
source("R/vulnerability.R") # checked
source("R/behavior.R") # checked 
source("R/simulation.R") # checked
source("R/visualizations.R") # Contains plotting functions, including the dashboard
source("R/experiments.R")   # Contains functions for running scenario and sensitivity analyses

# =============================================================================
# MAIN EXECUTION AND RESULTS SAVING
# =============================================================================

#' @title Run Full ABM Analysis
#' @description Orchestrates the entire analysis workflow, including running
#'   population collapse experiments, environmental gap analysis, network topology
#'   analysis, generating plots, and saving all results to disk.
#' @param save_results Logical, if TRUE, all generated data and plots will be saved to disk.
#' @param collapse_reps Number of replications to run for population collapse experiments.
#' @param gap_reps Number of replications to run for environmental gap analysis.
#' @param network_reps Number of replications to run for network topology analysis.
#' @param output_dir Character string, the name of the directory where all
#'   results (data and plots) will be saved.
#' @return A list containing all generated results and plots for programmatic access.
run_full_analysis <- function(save_results = TRUE,
                              collapse_reps = 20,
                              gap_reps = 10,
                              network_reps = 15,
                              output_dir = "abm_results") {
  
  # Create the output directory if it does not already exist
  if (save_results && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  cat("=== CLIMATE MIGRATION ABM ANALYSIS ===\n")
  cat("Starting full analysis...\n\n")
  
  # 1. Execute population collapse experiments
  cat("1. Running population collapse experiments...\n")
  collapse_results <- run_population_collapse_experiments(n_reps = collapse_reps)
  
  if (save_results) {
    write.csv(collapse_results,
              file.path(output_dir, "population_collapse_results.csv"),
              row.names = FALSE)
    cat("Population collapse results saved to:", file.path(output_dir, "population_collapse_results.csv"), "\n")
  }
  
  # 2. Execute environmental gap analysis
  cat("\n2. Running environmental gap analysis...\n")
  gap_results <- run_environmental_gap_analysis(n_reps = gap_reps)
  
  if (save_results) {
    write.csv(gap_results,
              file.path(output_dir, "environmental_gap_results.csv"),
              row.names = FALSE)
    cat("Environmental gap results saved to:", file.path(output_dir, "environmental_gap_results.csv"), "\n")
  }
  
  # 3. Execute network topology analysis
  cat("\n3. Running network topology analysis...\n")
  network_results <- run_network_topology_analysis(n_reps = network_reps)
  
  if (save_results) {
    write.csv(network_results,
              file.path(output_dir, "network_topology_results.csv"),
              row.names = FALSE)
    cat("Network topology results saved to:", file.path(output_dir, "network_topology_results.csv"), "\n")
  }
  
  # 4. Generate and display summary statistics
  cat("\n=== SUMMARY STATISTICS ===\n")
  
  # Population collapse summary
  cat("\nPopulation Collapse Summary:\n")
  collapse_summary <- collapse_results %>%
    group_by(scenario) %>%
    summarise(
      collapse_rate = mean(collapsed),
      avg_time_to_collapse = mean(time_to_collapse, na.rm = TRUE),
      avg_final_migration = mean(final_p_M),
      avg_max_adaptation = mean(max_p_A),
      .groups = 'drop'
    )
  print(collapse_summary)
  
  # Environmental gap summary
  cat("\nEnvironmental Gap Summary:\n")
  gap_summary <- gap_results %>%
    group_by(P_mu_A_scale, D_mu_A_scale, evol_steps) %>%
    summarise(
      collapse_rate = mean(collapsed),
      avg_final_migration = mean(final_p_M),
      .groups = 'drop'
    ) %>%
    arrange(desc(collapse_rate)) %>%
    head(10) # Show top 10 combinations with highest collapse rates
  print(gap_summary)
  
  # Network topology summary
  cat("\nNetwork Topology Summary:\n")
  network_summary <- network_results %>%
    group_by(network_config, network_type) %>%
    summarise(
      collapse_rate = mean(collapsed),
      avg_final_migration = mean(final_p_M),
      avg_max_adaptation = mean(max_p_A),
      avg_adaptation_speed = mean(adaptation_speed, na.rm = TRUE),
      .groups = 'drop'
    )

  # Save summary statistics
  if (save_results) {
    write.csv(collapse_summary,
              file.path(output_dir, "collapse_summary.csv"),
              row.names = FALSE)
    write.csv(gap_summary,
              file.path(output_dir, "gap_summary.csv"),
              row.names = FALSE)
    write.csv(network_summary,
              file.path(output_dir, "network_summary.csv"),
              row.names = FALSE)
  }
  
  # Generate basic visualization plots (if visualization functions exist)
  cat("\n4. Generating visualization plots...\n")
  
  # Basic collapse rate plot
  collapse_plot <- ggplot(collapse_summary, aes(x = scenario, y = collapse_rate)) +
    geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
    geom_text(aes(label = sprintf("%.2f", collapse_rate)), 
              vjust = -0.5, size = 3) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Population Collapse Rate by Scenario",
         x = "Scenario", y = "Collapse Rate",
         subtitle = "Proportion of simulations resulting in >95% migration") +
    ylim(0, 1)
  
  # Environmental gap heatmap
  gap_plot <- gap_results %>%
    group_by(P_mu_A_scale, D_mu_A_scale) %>%
    summarise(collapse_rate = mean(collapsed), .groups = 'drop') %>%
    ggplot(aes(x = P_mu_A_scale, y = D_mu_A_scale, fill = collapse_rate)) +
    geom_tile() +
    scale_fill_viridis_c(name = "Collapse\nRate") +
    theme_minimal() +
    labs(title = "Population Collapse Risk by Environmental Gap",
         x = "Disaster Probability Scale Factor",
         y = "Disaster Severity Scale Factor",
         subtitle = "Higher values = greater gap between expected vs actual conditions")
  
  # Network comparison plot
  network_plot <- ggplot(network_summary, aes(x = network_config, y = collapse_rate, fill = network_type)) +
    geom_bar(stat = "identity", alpha = 0.7) +
    geom_text(aes(label = sprintf("%.2f", collapse_rate)), 
              vjust = -0.5, size = 3) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Population Collapse Rate by Network Configuration",
         x = "Network Configuration", y = "Collapse Rate",
         fill = "Network Type") +
    ylim(0, 1)
  
  # Save plots
  if (save_results) {
    ggsave(file.path(output_dir, "collapse_rates_by_scenario.png"),
           collapse_plot, width = 12, height = 8)
    ggsave(file.path(output_dir, "environmental_gap_heatmap.png"),
           gap_plot, width = 10, height = 8)
    ggsave(file.path(output_dir, "network_collapse_comparison.png"),
           network_plot, width = 12, height = 8)
    cat("Visualization plots saved to:", output_dir, "\n")
  }
  
  # Save the complete R workspace for full reproducibility
  if (save_results) {
    save.image(file.path(output_dir, "abm_workspace.RData"))
    cat("\nWorkspace saved to:", file.path(output_dir, "abm_workspace.RData"), "\n")
  }
  
  cat("\n=== ANALYSIS COMPLETE ===\n")
  cat("All results saved to:", output_dir, "\n")
  
  # Return all generated objects for interactive analysis
  invisible(list(
    collapse_results = collapse_results,
    gap_results = gap_results,
    network_results = network_results,
    collapse_summary = collapse_summary,
    gap_summary = gap_summary,
    network_summary = network_summary,
    collapse_plot = collapse_plot,
    gap_plot = gap_plot,
    network_plot = network_plot
  ))
}

# =============================================================================
# RUNNING THE ANALYSIS
# =============================================================================

# To quickly test the model and view the dashboard visualizations for a single simulation run:
# NOTE: This requires the test_model_dashboard function to exist in visualizations.R
cat("\nTesting basic simulation functionality...\n")

# Test with default parameters
test_params <- create_default_params()
cat("Testing single simulation run...\n")
test_result <- run_simulation(test_params, verbose = TRUE)
cat("Single simulation completed successfully!\n")
cat(sprintf("Final results: p_L=%.3f, p_A=%.3f, p_M=%.3f\n", 
            tail(test_result$p_L, 1), tail(test_result$p_A, 1), tail(test_result$p_M, 1)))

# If test_model_dashboard function exists, uncomment the line below:
# test_result <- test_model_dashboard(show_plots = TRUE, save_plots = TRUE)

# To execute the full analysis (population collapse, environmental gap, and network topology analysis),
# uncomment the lines below. This process can be time-consuming depending on
# the number of replications and core availability.
# Results (data and plots) will be saved to the specified output directory.
# 
cat("\nTo run full analysis, uncomment the lines below:\n")
cat("# full_analysis_results <- run_full_analysis(\n")
cat("#   save_results = TRUE,\n")
cat("#   collapse_reps = 5,    # Number of replications for population collapse experiments\n")
cat("#   gap_reps = 3,         # Number of replications for environmental gap analysis\n")
cat("#   network_reps = 5,     # Number of replications for network topology analysis\n")
cat("#   output_dir = \"climate_abm_results\"\n")
cat("# )\n")

# Uncomment below to actually run the analysis:
# full_analysis_results <- run_full_analysis(
#   save_results = TRUE,
#   collapse_reps = 5,    # Number of replications for population collapse experiments
#   gap_reps = 3,         # Number of replications for environmental gap analysis  
#   network_reps = 5,     # Number of replications for network topology analysis
#   output_dir = "climate_abm_results"
# )

cat("\nABM analysis script loaded. Run the analysis functions as needed.\n")