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
source("R/params.R")
source("R/network.R")
source("R/agents.R")
source("R/disaster.R")
source("R/vulnerability.R")
source("R/behavior.R")
source("R/simulation.R")
source("R/visualizations.R") # Contains plotting functions, including the dashboard
source("R/experiments.R")   # Contains functions for running scenario and sensitivity analyses

# =============================================================================
# MAIN EXECUTION AND RESULTS SAVING
# =============================================================================

#' @title Run Full ABM Analysis
#' @description Orchestrates the entire analysis workflow, including running
#'   scenario experiments, sensitivity analysis, generating plots, and saving
#'   all results to disk.
#' @param save_results Logical, if TRUE, all generated data and plots will be saved to disk.
#' @param scenario_reps Number of replications to run for each defined scenario.
#' @param sensitivity_reps Number of replications to run for each parameter
#'   value in the sensitivity analysis.
#' @param output_dir Character string, the name of the directory where all
#'   results (data and plots) will be saved.
#' @return A list containing all generated results and plots for programmatic access.
run_full_analysis <- function(save_results = TRUE,
                              scenario_reps = 20,
                              sensitivity_reps = 10,
                              output_dir = "abm_results") {
  
  # Create the output directory if it does not already exist
  if (save_results && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  cat("=== CLIMATE MIGRATION ABM ANALYSIS ===\n")
  cat("Starting full analysis...\n\n")
  
  # 1. Execute scenario experiments
  cat("1. Running scenario experiments...\n")
  scenario_results <- run_scenario_experiments(n_reps = scenario_reps)
  
  if (save_results) {
    write.csv(scenario_results,
              file.path(output_dir, "scenario_results.csv"),
              row.names = FALSE)
    cat("Scenario results saved to:", file.path(output_dir, "scenario_results.csv"), "\n")
  }
  
  # Generate and save scenario comparison plots
  cat("   Generating scenario plots...\n")
  scenario_plots <- plot_scenario_comparison(scenario_results)
  
  if (save_results) {
    ggsave(file.path(output_dir, "scenario_behavior_dynamics.png"),
           scenario_plots$behavior_dynamics, width = 12, height = 8)
    ggsave(file.path(output_dir, "scenario_disaster_probability.png"),
           scenario_plots$disaster_probability, width = 10, height = 6)
    ggsave(file.path(output_dir, "scenario_avg_vulnerability.png"),
           scenario_plots$avg_vulnerability, width = 10, height = 6)
    cat("Scenario plots saved to:", output_dir, "\n")
  }
  
  # 2. Execute sensitivity analysis
  cat("\n2. Running sensitivity analysis...\n")
  sensitivity_results <- run_sensitivity_analysis(n_reps = sensitivity_reps)
  
  if (save_results) {
    write.csv(sensitivity_results,
              file.path(output_dir, "sensitivity_results.csv"),
              row.names = FALSE)
    cat("Sensitivity results saved to:", file.path(output_dir, "sensitivity_results.csv"), "\n")
  }
  
  # Generate and save sensitivity analysis plot
  cat("   Generating sensitivity plots...\n")
  sensitivity_plot <- plot_sensitivity_analysis(sensitivity_results)
  
  if (save_results) {
    ggsave(file.path(output_dir, "sensitivity_analysis_pM_final.png"),
           sensitivity_plot, width = 14, height = 10)
    cat("Sensitivity plot saved to:", file.path(output_dir, "sensitivity_analysis_pM_final.png"), "\n")
  }
  
  # 3. Generate and display summary statistics
  cat("\n=== SUMMARY STATISTICS ===\n")
  
  # Calculate and print final migration proportion summary for scenarios
  final_scenario_summary <- scenario_results %>%
    group_by(scenario, replication) %>%
    filter(time == max(time)) %>% # Get results from the last time step of each replication
    ungroup() %>%
    group_by(scenario) %>%
    summarise(
      mean_final_migration = mean(p_M),
      sd_final_migration = sd(p_M),
      mean_total_disasters = mean(n_disasters),
      mean_final_vulnerability = mean(avg_vulnerability),
      .groups = 'drop'
    )
  
  cat("\nFinal Scenario Summary:\n")
  print(final_scenario_summary)
  
  if (save_results) {
    write.csv(final_scenario_summary,
              file.path(output_dir, "final_scenario_summary.csv"),
              row.names = FALSE)
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
    scenario_results = scenario_results,
    sensitivity_results = sensitivity_results,
    scenario_plots = scenario_plots,
    sensitivity_plot = sensitivity_plot,
    final_scenario_summary = final_scenario_summary
  ))
}

# =============================================================================
# RUNNING THE ANALYSIS
# =============================================================================

# To quickly test the model and view the dashboard visualizations for a single simulation run:
cat("\nRunning a single test simulation with dashboard visualization...\n")
test_result <- test_model_dashboard(show_plots = TRUE, save_plots = TRUE)

# To execute the full analysis (scenario experiments and sensitivity analysis),
# uncomment the line below. This process can be time-consuming depending on
# the number of replications and core availability.
# Results (data and plots) will be saved to the specified output directory.
'''
cat("\nRunning full analysis (this may take a while)...\n")
full_analysis_results <- run_full_analysis(
  save_results = TRUE,
  scenario_reps = 1,    # Number of replications for each scenario
  sensitivity_reps = 1, # Number of replications for each sensitivity parameter value
  output_dir = "climate_abm_results"
)
'''
cat("\nABM analysis script execution finished. Check 'test_plots/' for dashboard plots or uncomment `run_full_analysis()` for comprehensive results.\n")