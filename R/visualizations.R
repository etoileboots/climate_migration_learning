# R/visualizations.R

# Load required libraries for plotting
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(viridis)
library(RColorBrewer)
library(scales)

# =============================================================================
# VISUALIZATION FUNCTIONS - UPDATED FOR POPULATION COLLAPSE ANALYSIS
# =============================================================================

#' @title Plot Population Collapse Analysis
#' @description Creates comprehensive visualizations for population collapse
#'   experiments focusing on environmental gaps and social learning effects.
#' @param collapse_results A data.frame from run_population_collapse_experiments().
#' @return A list of ggplot objects for collapse analysis.
plot_population_collapse_analysis <- function(collapse_results) {
  
  # Plot 1: Collapse Rate by Scenario
  collapse_by_scenario <- collapse_results %>%
    group_by(scenario) %>%
    summarise(
      collapse_rate = mean(collapsed),
      avg_time_to_collapse = mean(time_to_collapse, na.rm = TRUE),
      se_collapse = sqrt(collapse_rate * (1 - collapse_rate) / n()),
      n_reps = n(),
      .groups = 'drop'
    ) %>%
    mutate(scenario = reorder(scenario, collapse_rate))
  
  p1 <- ggplot(collapse_by_scenario, aes(x = scenario, y = collapse_rate, fill = scenario)) +
    geom_col(alpha = 0.8) +
    geom_errorbar(aes(ymin = pmax(0, collapse_rate - se_collapse), 
                      ymax = pmin(1, collapse_rate + se_collapse)),
                  width = 0.3, color = "black") +
    geom_text(aes(label = paste0(round(collapse_rate * 100, 1), "%")), 
              vjust = -0.5, size = 3, fontface = "bold") +
    scale_y_continuous(labels = percent_format(), limits = c(0, 1.1)) +
    scale_fill_viridis_d(option = "plasma", name = "Scenario") +
    labs(title = "Population Collapse Rate by Scenario",
         subtitle = "Proportion of simulations resulting in ≥95% migration",
         x = "Scenario", y = "Collapse Rate") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          legend.position = "none",
          plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 11, color = "gray60"))
  
  # Plot 2: Environmental Gap vs Collapse Rate
  gap_analysis <- collapse_results %>%
    mutate(env_gap = P_mu_A_scale + D_mu_A_scale - 2) %>%
    group_by(scenario, env_gap) %>%
    summarise(collapse_rate = mean(collapsed), .groups = 'drop')
  
  p2 <- ggplot(gap_analysis, aes(x = env_gap, y = collapse_rate, color = scenario)) +
    geom_point(size = 4, alpha = 0.8) +
    geom_smooth(method = "loess", se = TRUE, alpha = 0.3) +
    scale_y_continuous(labels = percent_format()) +
    scale_color_brewer(type = "qual", palette = "Set2", name = "Scenario") +
    labs(title = "Environmental Gap vs Population Collapse",
         subtitle = "Combined effect of underestimating disaster probability and severity",
         x = "Environmental Gap (P_scale + D_scale - 2)",
         y = "Collapse Rate") +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 11, color = "gray60"))
  
  # Plot 3: Time to Collapse Distribution
  collapse_timing <- collapse_results %>%
    filter(collapsed == TRUE, !is.na(time_to_collapse))
  
  p3 <- ggplot(collapse_timing, aes(x = time_to_collapse, fill = scenario)) +
    geom_histogram(bins = 20, alpha = 0.7, position = "identity") +
    facet_wrap(~ scenario, scales = "free_y", ncol = 2) +
    scale_fill_viridis_d(option = "plasma", name = "Scenario") +
    labs(title = "Distribution of Time to Population Collapse",
         subtitle = "Among simulations that experienced collapse",
         x = "Time Steps to Collapse", y = "Frequency") +
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 11, color = "gray60"))
  
  # Plot 4: Adaptation vs Collapse Relationship
  p4 <- ggplot(collapse_results, aes(x = max_p_A, y = final_p_M, color = collapsed)) +
    geom_point(alpha = 0.6, size = 2) +
    geom_hline(yintercept = 0.95, linetype = "dashed", color = "red", size = 1) +
    annotate("text", x = 0.8, y = 0.97, label = "Collapse Threshold", 
             color = "red", size = 3) +
    scale_color_manual(values = c("FALSE" = "#1a9850", "TRUE" = "#d73027"),
                       labels = c("No Collapse", "Collapse"),
                       name = "Outcome") +
    facet_wrap(~ scenario, ncol = 3) +
    labs(title = "Maximum Adaptation vs Final Migration Rate",
         subtitle = "Higher adaptation generally reduces collapse risk",
         x = "Maximum Proportion Adaptive", y = "Final Migration Proportion") +
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 11, color = "gray60"))
  
  # Plot 5: Social Learning Effect
  social_learning_effect <- collapse_results %>%
    group_by(r, kappa_avg) %>%
    summarise(collapse_rate = mean(collapsed), 
              n_sims = n(), .groups = 'drop') %>%
    filter(n_sims >= 10) # Only include combinations with sufficient data
  
  p5 <- ggplot(social_learning_effect, aes(x = r, y = kappa_avg, fill = collapse_rate)) +
    geom_tile() +
    geom_text(aes(label = round(collapse_rate, 2)), color = "white", size = 3) +
    scale_fill_viridis_c(option = "plasma", name = "Collapse\nRate",
                         labels = percent_format()) +
    labs(title = "Social Learning Parameters vs Collapse Rate",
         subtitle = "Effect of learning rate (r) and adaptation barriers (kappa)",
         x = "Social Learning Rate (r)", y = "Average Adaptation Barrier (kappa)") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 11, color = "gray60"))
  
  return(list(
    collapse_by_scenario = p1,
    environmental_gap = p2,
    collapse_timing = p3,
    adaptation_vs_collapse = p4,
    social_learning_effect = p5
  ))
}


#' @title Plot Environmental Gap Heatmap
#' @description Creates a heatmap showing collapse rates across different
#'   combinations of environmental underestimation parameters.
#' @param gap_results A data.frame from run_environmental_gap_analysis().
#' @return A ggplot object showing the environmental gap analysis.
plot_environmental_gap_heatmap <- function(gap_results) {
  
  # Aggregate results by parameter combination
  gap_summary <- gap_results %>%
    group_by(P_mu_A_scale, D_mu_A_scale, T_onset) %>%
    summarise(
      collapse_rate = mean(collapsed),
      n_reps = n(),
      .groups = 'drop'
    )
  
  # Create main heatmap (average across T_onset values)
  gap_main <- gap_summary %>%
    group_by(P_mu_A_scale, D_mu_A_scale) %>%
    summarise(avg_collapse_rate = mean(collapse_rate), .groups = 'drop')
  
  p_main <- ggplot(gap_main, aes(x = factor(P_mu_A_scale), y = factor(D_mu_A_scale), 
                                 fill = avg_collapse_rate)) +
    geom_tile(color = "white", size = 0.5) +
    geom_text(aes(label = round(avg_collapse_rate, 2)), 
              color = "white", size = 3, fontface = "bold") +
    scale_fill_viridis_c(option = "plasma", name = "Collapse\nRate",
                         labels = percent_format(), 
                         limits = c(0, 1)) +
    labs(title = "Population Collapse Risk Landscape",
         subtitle = "Collapse rate by environmental underestimation (averaged across onset times)",
         x = "Disaster Probability Scale (P_mu_A_scale)",
         y = "Disaster Severity Scale (D_mu_A_scale)") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 11, color = "gray60"),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 11, face = "bold"),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 9),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  return(p_main)
}
