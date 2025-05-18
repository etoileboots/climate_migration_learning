# devtools::install_github("css4s/socmod")

library(socmod)
library(magrittr)
library(ggplot2)

graph <- make_small_world(20, 6, p = 0.2)
plot(graph)


abm <- make_abm(graph = graph)
plot(abm$get_network())


initialize_agents(abm, initial_prevalence = 0.2, adaptive_fitness = 1.2,
                  legacy_fitness = 1.0)


# Check table of behaviors to count them
behaviors <- purrr::map_vec(abm$agents, \(a) a$get_behavior())
table(behaviors)
# Check initial adoption and network
plot_network_adoption(abm)

# Run a trial and visualize results

trial <- run_trial(abm, stop = 10)
trial$get_outcomes()
trial$get_observations()

prevalence_summary <- summarise_prevalence(trial)

plot_prevalence(trial, tracked_behaviors = "Adaptive")

class(trial)
# trials will be like...
list(trial, trial, trial)

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

rewire_prob_vals <- c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)
trials <- run_trials(
  # First argument: model generator function
  prototype_generator,
  
  # Model parameters
  N = 50,
  k = 10,
  rewire_prob = rewire_prob_vals,
  stop = socmod::fixated,
  # Trial metadata/metaparameters
  n_trials_per_param = 10,
  
  # System configuration
  .progress = TRUE,
  overwrite = T,
  syncfile = "save-progress.RData"
)

plot_prevalence(trials[[35]])

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
  ggplot(aes(x = rewire_prob, y = Value, linetype = Measure)) +
  geom_line(color = line_color, linewidth=1.5) + 
  scale_x_continuous(breaks = rewire_prob_vals) +
  theme_classic(base_size = 16) + 
  xlab("Rewire probability") + ylab("Value")








