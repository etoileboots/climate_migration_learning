# Import required libraries
# Install this if you don't have devtools.
install.packages("devtools")
devtools::install_github("css4s/socmod")

library(ggnetwork)
library(purrr)
library(magrittr)
library(ggplot2)
library(igraph)
library(socmod)

# Intialize a small world graph with size N and degree k 
N <- 20 # Let's start with the small number 20 
k <- 6 # Degree k = 6, each individual will have 6 connections
small_world_graph <- make_small_world(N, k, p = 0.2)

# Plot the graph 
plot(small_world_graph)

abm <- make_abm(graph = small_world_graph)
# Initialize 20% (4) agents with adaptive behavior w/ 
# an adaptive fitness 1.2x of non-adaptive legacy behavior
initialize_agents(abm, initial_prevalence = 0.2, adaptive_fitness = 1.2)

# Check table of behaviors to count them
behaviors <- purrr::map_vec(abm$agents, \(a) a$get_behavior())
table(behaviors)

# Check initial adoption and network
plot_network_adoption(abm)

# Run a trial and visualize results
trial <- run_trial(abm, stop = socmod::fixated)
trial$get_outcomes()
print(trial$get_observations(), n=20)

# Analyse the prevalence of adaptive behavior across trials 
prevalence_summary <- summarise_prevalence(trial)

plot_prevalence(trial, tracked_behaviors = "Adaptive")

class(trial)
# trials will be like...
list(trial, trial, trial)

# Commenting this out for right now 
prototype_generator <- function(parameter_row) {
  N <- parameter_row$N; k <- parameter_row$k; p <- parameter_row$rewire_prob 
  
  connected <- FALSE
  while (!connected) {
    parameter_row$graph <- make_small_world(N, k, p)
    connected <- igraph::is_connected(parameter_row$graph)
  }
  return (
    do.call(make_abm, parameter_row) %>% initialize_agents
  )
}
# Alter the probability. of rewiring 
rewire_prob_vals <- c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)
trials <- run_trials(
  # First argument: model generator function
  prototype_generator,
  
  # Model parameters
  N = 50,
  k = 10,
  rewire_prob = rewire_prob_vals,
  # Trial metadata/metaparameters
  n_trials_per_param = 10,
  stop = socmod::fixated,
  
  # System configuration
  .progress = TRUE,
  overwrite = T,
  syncfile = "save-progress.RData"
)

# Summarize within trials only; rename and factor rewire probability for display
summary <- summarise_prevalence(
  trials, input_parameters = "rewire_prob", across_trials = F
) %>% 
  dplyr::mutate(
    `Rewire Probability` = factor(rewire_prob, rewire_prob_vals)
  )

p <- ggplot(
  summary,
  aes(x=Step, y=Prevalence,
      color=`Rewire Probability`,
      linetype=`Rewire Probability`,
      group = trial_id)) +
  geom_line(linewidth=1.4, alpha = 0.875) + theme_classic(base_size = 16) +
  ggplot2::scale_color_manual(values = unname(SOCMOD_PALETTE))

p

# We've got 60 trials total 
length(trials)

outcomes <- summarise_outcomes(
  trials, 
  input_parameters = "rewire_prob", 
  outcome_measures = c("success_rate", "mean_fixation_steps")
) 

max_fix_time <- max(outcomes$Value[outcomes$Measure == "mean_fixation_steps"])
# Normalize to calculate mean fixation time as a fraction of maximum
outcomes_norm <- outcomes %>%
  dplyr::mutate(Value = dplyr::case_when(
    Measure == "mean_fixation_steps" ~ Value / max_fix_time,
    TRUE ~ Value
  ))
# Rename and set order of Measure factors to avoid messing with the legend in plotting
outcomes_norm$Measure[outcomes_norm$Measure == "success_rate"] <- "Success rate"
outcomes_norm$Measure[outcomes_norm$Measure == "mean_fixation_steps"] <- "Normalized fixation time"
outcomes_norm$Measure <- factor(outcomes_norm$Measure, levels = c(
  "Success rate", "Normalized fixation time"
))

# Use a custom socmod line color
line_color <- SOCMOD_PALETTE_CVD["pink"]
outcomes_norm %>%
  ggplot2::ggplot(aes(x = adaptive_fitness, y = Value, linetype = Measure)) +
  geom_line(color = line_color, linewidth=1.5) + 
  scale_x_continuous(breaks = adaptive_fitness_vals) +
  theme_classic(base_size = 16) + xlab("Adaptive fitness") + ylab("Value")







