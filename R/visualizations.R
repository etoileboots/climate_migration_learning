# R/visualization.R

# Load required libraries for plotting
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra) # For grid.arrange
library(viridis)   # For color palettes

# =============================================================================
# VISUALIZATION FUNCTIONS
# =============================================================================

#' @title Plot Scenario Comparison
#' @description Creates plots to compare behavioral dynamics and disaster
#'   probability across different simulation scenarios.
#' @param scenario_results A data.frame containing simulation results from multiple
#'   scenario runs. Expected columns include: time, p_L, p_A, p_M, pi_t,
#'   avg_vulnerability, scenario, replication.
#' @return A list of ggplot objects representing the comparison plots.
plot_scenario_comparison <- function(scenario_results) {
  
  # Plot 1: Behavioral Dynamics (p_L, p_A, p_M over time)
  behavior_data <- scenario_results %>%
    pivot_longer(cols = c(p_L, p_A, p_M),
                 names_to = "behavior", values_to = "proportion") %>%
    mutate(
      behavior = case_when(
        behavior == "p_L" ~ "Legacy",
        behavior == "p_A" ~ "Adaptive",
        behavior == "p_M" ~ "Migrated",
        TRUE ~ behavior
      ),
      scenario = factor(scenario) # Ensure scenario is treated as a factor
    )
  
  p_behavior <- ggplot(behavior_data, aes(x = time, y = proportion, color = behavior, group = interaction(behavior, replication))) +
    geom_line(alpha = 0.3) + # Light lines for individual replications
    stat_summary(aes(group = behavior), fun = mean, geom = "line", linewidth = 1.2, linetype = "solid", alpha = 0.8) + # Solid line for mean
    facet_wrap(~ scenario, ncol = 2) +
    labs(title = "Scenario Comparison: Behavioral Dynamics",
         subtitle = "Solid line shows mean across replications",
         x = "Time Steps", y = "Proportion of Population",
         color = "Behavior Type") +
    scale_color_manual(values = c("Legacy" = "#d73027", "Adaptive" = "#1a9850", "Migrated" = "#313695")) +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 11, color = "gray60"))
  
  # Plot 2: Disaster Probability (pi_t over time)
  p_disaster_prob <- ggplot(scenario_results, aes(x = time, y = pi_t, group = replication)) +
    geom_line(alpha = 0.3, color = "#fc8d59") +
    stat_summary(fun = mean, geom = "line", linewidth = 1.2, linetype = "solid", color = "#d73027") +
    facet_wrap(~ scenario, ncol = 2) +
    labs(title = "Scenario Comparison: Disaster Probability (pi_t)",
         subtitle = "Solid line shows mean across replications",
         x = "Time Steps", y = "Disaster Probability") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 11, color = "gray60"))
  
  # Plot 3: Average Vulnerability (avg_vulnerability over time)
  p_avg_vulnerability <- ggplot(scenario_results, aes(x = time, y = avg_vulnerability, group = replication)) +
    geom_line(alpha = 0.3, color = "#91bfdb") +
    stat_summary(fun = mean, geom = "line", linewidth = 1.2, linetype = "solid", color = "#4575b4") +
    facet_wrap(~ scenario, ncol = 2) +
    labs(title = "Scenario Comparison: Average Vulnerability",
         subtitle = "Solid line shows mean across replications",
         x = "Time Steps", y = "Average Vulnerability") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 11, color = "gray60"))
  
  
  return(list(behavior_dynamics = p_behavior,
              disaster_probability = p_disaster_prob,
              avg_vulnerability = p_avg_vulnerability))
}


#' @title Plot Sensitivity Analysis
#' @description Creates plots to show the impact of varying single parameters
#'   on the final proportion of migrated agents.
#' @param sensitivity_results A data.frame containing results from sensitivity analysis.
#'   Expected columns include: parameter, value, replication, p_M_final.
#' @return A ggplot object visualizing the sensitivity analysis.
plot_sensitivity_analysis <- function(sensitivity_results) {
  # Calculate mean and standard deviation of final migration proportion for each parameter value
  summary_data <- sensitivity_results %>%
    group_by(parameter, value) %>%
    summarise(
      mean_p_M = mean(p_M_final),
      sd_p_M = sd(p_M_final),
      .groups = 'drop'
    ) %>%
    mutate(value_factor = as.factor(value)) # Convert parameter value to factor for discrete display
  
  ggplot(summary_data, aes(x = value_factor, y = mean_p_M, group = 1)) +
    geom_line(color = "#3182bd", linewidth = 1) + # Line connecting mean values
    geom_point(color = "#3182bd", size = 3) +     # Points for mean values
    geom_errorbar(aes(ymin = mean_p_M - sd_p_M, ymax = mean_p_M + sd_p_M),
                  width = 0.2, color = "#3182bd") + # Error bars for standard deviation
    facet_wrap(~ parameter, scales = "free_x", ncol = 2) + # Separate plot for each parameter
    labs(title = "Sensitivity Analysis: Impact on Final Migration Proportion",
         x = "Parameter Value", y = "Mean Final Migration Proportion (p_M) ± SD") +
    theme_minimal() +
    theme(plot.title = element_text(size = 16, face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
          strip.text = element_text(size = 12, face = "bold"))
}


#' @title Test Model Dashboard
#' @description Runs a single simulation with default parameters and displays
#'   a dashboard of key results. Plots can optionally be saved to disk.
#' @param show_plots Logical, if TRUE, displays the generated plots.
#' @param save_plots Logical, if TRUE, saves the generated plots to a 'test_plots' directory.
#' @return The simulation result data.frame for the single test run.
test_model_dashboard <- function(show_plots = TRUE, save_plots = FALSE) {
  cat("Testing model with default parameters for dashboard visualization...\n")
  params <- create_default_params()
  params$T <- 200  # Set a moderate simulation length for visualization
  params$N <- 200  # Set a moderate population size for testing to keep plots manageable
  
  # Explicitly set disaster parameters if they differ from default or need emphasis
  params$use_sigmoid <- TRUE # Ensure sigmoid is used for disaster probability
  params$k_sigmoid <- 10
  params$tau_sigmoid <- 0.5
  params$beta <- 0.2 # Impact of proportion Legacy on disaster severity standard deviation
  
  cat("Running simulation...\n")
  result <- run_simulation(params, verbose = TRUE)
  
  cat("Test completed successfully!\n")
  cat("Final migration proportion:", tail(result$p_M, 1), "\n")
  cat("Total disasters:", tail(result$n_disasters, 1), "\n")
  
  if (show_plots) {
    cat("Creating visualizations...\n")
    
    # --- Plot 1: Behavioral Dynamics ---
    behavior_data <- result %>%
      select(time, p_L, p_A, p_M) %>% # Select proportions of TOTAL population
      pivot_longer(cols = c(p_L, p_A, p_M),
                   names_to = "behavior", values_to = "proportion") %>%
      mutate(behavior = case_when(
        behavior == "p_L" ~ "Legacy",
        behavior == "p_A" ~ "Adaptive",
        behavior == "p_M" ~ "Migrated"
      ))
    
    p1 <- ggplot(behavior_data, aes(x = time, y = proportion, color = behavior)) +
      geom_line(linewidth = 1.2) +
      labs(title = "Behavioral Dynamics Over Time",
           subtitle = "Evolution of agent behaviors during simulation",
           x = "Time Steps", y = "Proportion of Population",
           color = "Behavior Type") +
      scale_color_manual(values = c("Legacy" = "#d73027",
                                    "Adaptive" = "#1a9850",
                                    "Migrated" = "#313695")) +
      theme_minimal() +
      theme(legend.position = "bottom",
            plot.title = element_text(size = 14, face = "bold"),
            plot.subtitle = element_text(size = 11, color = "gray60"))
    
    # --- Plot 2: Disaster Probability (pi_t) ---
    p2 <- ggplot(result, aes(x = time, y = pi_t)) +
      geom_line(color = "#fc8d59", linewidth = 1) +
      # Mark actual disaster events on the probability line
      geom_point(data = filter(result, disaster_severity > 0), aes(y = pi_t), color = "red", size = 2) +
      labs(title = "Disaster Probability Over Time",
           subtitle = "Red points indicate actual disaster events",
           x = "Time Steps", y = "Disaster Probability (pi_t)") +
      ylim(0,1) + # Probability ranges from 0 to 1
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold"),
            plot.subtitle = element_text(size = 11, color = "gray60"))
    
    # --- Plot 3: Average Vulnerability Trajectory ---
    p3 <- ggplot(result, aes(x = time, y = avg_vulnerability)) +
      geom_line(color = "#4575b4", linewidth = 1.2) +
      # Add a horizontal line for the migration threshold
      geom_hline(yintercept = params$v_mig, linetype = "dashed", color = "red") +
      annotate("text", x = max(result$time)*0.8, y = params$v_mig + 0.05,
               label = "Migration Threshold", color = "red", size = 3) +
      labs(title = "Average Agent Vulnerability Over Time",
           subtitle = paste0("Migration Threshold: ", params$v_mig),
           x = "Time Steps", y = "Average Vulnerability") +
      ylim(0, max(1, params$v_mig + 0.1)) + # Adjust y-axis limit to show threshold
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold"),
            plot.subtitle = element_text(size = 11, color = "gray60"))
    
    # --- Plot 4: Disaster Events and Severity ---
    disaster_events <- result %>% filter(disaster_severity > 0)
    p4 <- ggplot(result, aes(x = time, y = disaster_severity)) +
      geom_point(data = disaster_events, aes(x = time, y = disaster_severity),
                 color = "darkred", size = 3, shape = 18) + # Mark disaster events
      geom_line(color = "gray", linetype = "dotted") + # Dotted line to show time progression
      labs(title = "Disaster Events and Severity",
           x = "Time Steps", y = "Disaster Severity") +
      ylim(0, max(result$disaster_severity, 0.1)) + # Ensure y-axis starts at 0
      theme_minimal() +
      theme(plot.title = element_text(size = 14, face = "bold"))
    
    # --- Plot 5: Phase Diagram (Average Vulnerability vs. Proportion Adaptive) ---
    p5 <- ggplot(result, aes(x = p_A, y = avg_vulnerability, color = time)) +
      geom_path(arrow = arrow(type = "open", length = unit(0.2, "cm")), linewidth = 1) + # Path with arrows to show progression
      geom_point(size = 2) + # Points for each time step
      scale_color_viridis_c(option = "plasma", direction = -1) + # Color gradient for time
      labs(title = "Phase Diagram: Vulnerability vs. Adaptive Proportion",
           x = "Proportion of Adaptive Agents (p_A)",
           y = "Average Vulnerability",
           color = "Time") +
      xlim(0, 1) + ylim(0, max(1, params$v_mig + 0.1)) + # Set axis limits
      theme_minimal() +
      theme(legend.position = "right",
            plot.title = element_text(size = 14, face = "bold"))
    
    # --- Plot 6: Summary Statistics ---
    final_stats <- result %>%
      filter(time == max(time)) %>% # Get values from the last time step
      select(p_L, p_A, p_M, n_disasters, avg_vulnerability) %>%
      pivot_longer(everything(), names_to = "metric", values_to = "value") %>%
      mutate(metric = case_when( # Rename metrics for better display
        metric == "p_L" ~ "Final Legacy (%)",
        metric == "p_A" ~ "Final Adaptive (%)",
        metric == "p_M" ~ "Final Migrated (%)",
        metric == "n_disasters" ~ "Total Disasters",
        metric == "avg_vulnerability" ~ "Final Avg. Vulnerability",
        TRUE ~ metric
      ))
    
    final_stats$value[1:3] <- final_stats$value[1:3] * 100 # Convert proportions to percentages
    
    p6 <- ggplot(final_stats, aes(x = metric, y = value, fill = metric)) +
      geom_col() + # Bar plot for summary statistics
      geom_text(aes(label = round(value, ifelse(metric %in% c("Final Legacy (%)", "Final Adaptive (%)", "Final Migrated (%)"), 1, 2))),
                vjust = -0.5, size = 3) + # Display values on bars
      labs(title = "Summary Statistics at End of Simulation",
           x = NULL, y = "Value") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
            legend.position = "none", # No legend needed for this plot
            plot.title = element_text(size = 14, face = "bold"))
    
    # Display plots using grid.arrange for a dashboard layout
    dashboard <- grid.arrange(p1, p2, p3, p4, p5, p6,
                              ncol = 2, nrow = 3,
                              top = "Climate Migration ABM - Test Results Dashboard")
    print(dashboard) # Display the combined dashboard
    
    if (save_plots) {
      if (!dir.exists("test_plots")) dir.create("test_plots") # Create directory if it doesn't exist
      ggsave("test_plots/dashboard.png", dashboard, width = 16, height = 12)
      ggsave("test_plots/behavior_dynamics.png", p1, width = 12, height = 8)
      ggsave("test_plots/disaster_probability.png", p2, width = 10, height = 6)
      ggsave("test_plots/vulnerability_trajectory.png", p3, width = 10, height = 6)
      ggsave("test_plots/disaster_events.png", p4, width = 10, height = 6)
      ggsave("test_plots/phase_diagram.png", p5, width = 8, height = 8)
      ggsave("test_plots/summary_stats.png", p6, width = 6, height = 4)
      cat("Test plots saved to 'test_plots/' directory\n")
    }
  }
  
  return(result)
}