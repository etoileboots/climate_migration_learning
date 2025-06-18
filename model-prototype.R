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

# Initialization of variables/params
N <- 20 # Agents
k <-  4 # Mean degree
# Fitness of various behaviors 
f_A <- 1.1 
f_L <- 1
f_M <- 0.8
# Initial prevalence of individuals of different behaviors (so, there should be 4 adaptive, 14 legacy, and 2 migrated)
p_A<- 0.2
p_L <- 0.7
p_M <- 0.1
p_D <- 0.05 # On average, there is a disaster every 50 steps, with the p(D) = 5% 

# Intialize a small world graph with size N and degree k 
small_world_graph <- make_small_world(N, k, p = 0.2)

# Plot the graph 
plot(small_world_graph)

abm <- make_abm(graph = small_world_graph)

# Write a function to pipe initial_agents into initialize_migration and return 
# agents with either migratory, adaptive, or legacy behavior 
initialize_3D_agents <- function(model, 
                                 initial_adaptive_prevalence = 0.1, 
                                 initial_migrated_prevalence = 0.05, 
                                 adaptive_behavior = "Adaptive",
                                 migrated_behavior = "Migrated",
                                 legacy_behavior = "Legacy",
                                 adaptive_fitness = 1.2,
                                 legacy_fitness = 1.0,
                                 migrated_fitness = 0.8) {
    # Get number of each type of agent
    n_agents <- model$get_parameter("n_agents")
    # Handle either double- or integer-valued (i.e. % or count) initial_adaptive_prevalence
    if (is.numeric(initial_adaptive_prevalence)) {
      if (initial_adaptive_prevalence <= 1) {
        n_adaptive <- round(n_agents * initial_adaptive_prevalence)
      } else {
        n_adaptive <- as.integer(initial_adaptive_prevalence)
      }
    } else {
      stop("initial_adaptive_prevalence must be a numeric proportion (<=1) or integer count")
    }
    
    if (n_adaptive > n_agents) {
      stop("Number of adaptive agents exceeds total agents")
    }
    # Handle either double- or integer-valued (i.e. % or count) initial_migrated_prevalence
    if (is.numeric(initial_migrated_prevalence)) {
      if (initial_migrated_prevalence <= 1) {
        n_migrated <- round(n_agents * initial_migrated_prevalence)
      } else {
        n_migrated <- as.integer(initial_migrated_prevalence)
      }
    } else {
      stop("initial_migrated_prevalence must be a numeric proportion (<=1) or integer count")
    }
    
    if (n_migrated > n_agents) {
      stop("Number of migrated agents exceeds total agents")
    }
    if (n_adaptive + n_migrated > n_agents) {
      stop("Sum of adaptive and migrated agents exceeds total number of agents")
    }
    # Number of legacy agents is the difference between total and adaptive counts
    n_legacy <- n_agents - n_migrated - n_adaptive
    
    # Specify agent behaviors and fitnesses, assigned to agents below
    ids <- 1:n_agents
    adaptive_ids <- sample(ids, n_adaptive)
    remaining_ids <- setdiff(ids, adaptive_ids)
    migrated_ids <- sample(remaining_ids, n_migrated)
    legacy_ids <- setdiff(remaining_ids, migrated_ids)
    
    # Each row here specifies one agent's attributes
    agent_spec <- tibble::tibble(
      id = c(adaptive_ids, legacy_ids, migrated_ids),
      behavior = c(rep(adaptive_behavior, n_adaptive), 
                   rep(legacy_behavior, n_legacy), 
                   rep(migrated_behavior, n_migrated)),
      fitness = c(rep(adaptive_fitness, n_adaptive), 
                  rep(legacy_fitness, n_legacy),
                  rep(migrated_fitness, n_migrated))
    )
    
    # Set agent attributes using purrr::pwalk
    purrr::pwalk(agent_spec, \(id, behavior, fitness) {
      model$get_agent(id)$set_behavior(behavior)$set_fitness(fitness)
    })
    
    # Return the model to continue down the pipeline.
    return (invisible(model))
  }


# Initialize agents: 
initialize_3D_agents(abm, 
  initial_adaptive_prevalence = p_A, 
  initial_migrated_prevalence = p_M, 
  adaptive_behavior = "Adaptive",
  migrated_behavior = "Migrated",
  legacy_behavior = "Legacy",
  adaptive_fitness = f_A,
  legacy_fitness = f_L,
  migrated_fitness = f_M)

plot_3D_network_adoption <- function(
    x, layout = NULL, behaviors = c("Adaptive", "Legacy", "Migrated"),
    behavior_colors = SOCMOD_PALETTE[c(3, 2,1)], node_size = 6,
    label = FALSE, plot_mod = identity, edgewidth = 1
) {
  
  if (inherits(x, "Trial")) {
    model <- x$model
  } else if (inherits(x, "AgentBasedModel")) {
    model <- x
  } else {
    stop("Input must be a Trial or AgentBasedModel.")
  }
  
  net <- model$get_network()
  behavior_vec <- vapply(model$agents, function(a) a$get_behavior(), character(1))
  net <- igraph::set_vertex_attr(net, "Behavior", value = behavior_vec)
  
  use_size_aes <- FALSE
  if (is.list(node_size)) {
    stopifnot(length(node_size) == 1, !is.null(names(node_size)))
    result <- .compute_node_size_measure(net, node_size)
    net <- result$net
    measure_name <- result$measure_name
    use_size_aes <- TRUE
  } else {
    stopifnot(is.numeric(node_size), length(node_size) == 1)
  }
  df <- tibble::tibble()
  if (is.null(layout)) {
    df <- ggnetwork::ggnetwork(net)
  } else {
    df <- ggnetwork::ggnetwork(net, layout = layout)
  }
  aes_base <- ggplot2::aes(x = x, y = y, xend = xend, yend = yend)
  if (use_size_aes) {
    aes_base$size <- rlang::sym(measure_name)
  }
  
  p <- 
    ggplot2::ggplot(df, mapping = aes_base) +
    ggnetwork::geom_edges(color="lightgrey", linewidth = edgewidth) +
    (if (use_size_aes) ggnetwork::geom_nodes(aes(color = Behavior)) 
     else ggnetwork::geom_nodes(aes(color = Behavior), size = node_size)) +
    
    ggplot2::scale_color_manual(values = setNames(behavior_colors, behaviors), 
                                limits = behaviors, na.value = "gray80") +
    ggnetwork::theme_blank()
  
  if (label) {
    p <- 
      p + 
      ggnetwork::geom_nodelabel_repel(ggplot2::aes(label = name), size = 1.5)
  }
  
  return (plot_mod(p))
}


# Check table of behaviors to count them
behaviors <- purrr::map_vec(abm$agents, \(a) a$get_behavior())
table(behaviors)

# Plot initial state 
plot_3D_network_adoption(abm)

run_trial <- function(model,
                      stop = socmod::fixated,
                      legacy_behavior = "Legacy",
                      adaptive_behavior = "Adaptive",
                      metadata = list()) {
  
  # Initialize, run, and return a new Trial object.
  return (
    Trial$new(model = model, metadata = metadata)$run(
      stop = stop, legacy_behavior = legacy_behavior,
      adaptive_behavior = adaptive_behavior
    )
  )
}




#' Run a grid of trial ensembles with parameter metadata
#'
#' Runs trial ensembles across a parameter grid. All scalar and function-valued parameters
#' used in model construction or trial dynamics are included in metadata for transparency.
#' @param model_generator Function that returns a new AgentBasedModel instance according to model_parameters, a named list of parameter label-value pairs.
#' @param n_trials_per_param Number of trials per parameter combination.
#' @param stop Stopping condition (number or function).
#' @param .progress Whether to show progressbar when running the trials.
#' @param ... List of parameter label-value pairs; vector or singleton values.
#'
#' @return A list of Trial objects 
#' @examples
#' agents = c(Agent$new(1), Agent$new(2))
#' mod_gen <- function(mparam_list) { 
#'   return (
#'     make_abm(
#'       make_model_parameters(
#'         # The first three positional ModelParameters fields go first.
#'         success_biased_learning_strategy, graph,
#'         # Then any auxiliary label-value pairs may be flexibly added here.
#'         adaptive_fitness = mparam_list$adaptive_fitness
#'       ), 
#'       agents = agents
#'     )
#'   )
#' }
#' # Run 2 trials per parameter setting, stopping after 10 time steps. 
#' trials <- run_trials(mod_gen, n_trials_per_param = 2, stop = 10,
#'   learning_strategy = success_bias_learning_strategy,
#'   adaptive_fitness = c(0.8, 1.0, 1.2)
#' )  # With this we'll have six total trials, two for each adaptive_fitness.
#' @export
run_3D_trials <- function(model_generator, n_trials_per_param = 10,
                       stop = 10, .progress = TRUE, 
                       syncfile = NULL, overwrite = FALSE, ...) {
  
  # Check if syncfile is given...
  if (!is.null(syncfile)) {
    # ...and load it if it exists and we aren't overwriting existing
    if (file.exists(syncfile) && !overwrite) {
      cat("\nLoading trials from syncfile:", syncfile, "\n\n")
      load(syncfile)
      return (trials)
    }
  }
  
  
  # Initialize dataframe where each row is a set of model parameters
  model_parameters <- c(list(...), 
                        list(replication_id = 1:(n_trials_per_param)))
  
  parameter_grid <- tidyr::crossing(!!!model_parameters)
  
  legacy_behavior <- "Legacy"
  adaptive_behavior <- "Adaptive"
  migrated_behavior <- "Migrated"
  if ("legacy_behavior" %in% model_parameters) {
    legacy_behavior <- model_parameters$legacy_behavior
  }
  if ("adaptive_behavior" %in% model_parameters) {
    adaptive_behavior <- model_parameters$adaptive_behavior
  }
  if ("migrated_behavior" %in% model_parameters) {
    migrated_behavior <- model_parameters$migrated_behavior
  }
  
  
  # Create a list of trials, each trial initialized with a param list from the grid 
  trials <- purrr::pmap(
    parameter_grid, function(...) {
      param_list <- list(...)
      model <- model_generator(param_list)
      
      run_trial( 
        model, stop, legacy_behavior, adaptive_behavior, migrated_behavior,
        metadata = list(replication_id = param_list$replication_id)
      )
    }, 
    .progress = .progress
  )
  
  
  # Check if syncfile is given...
  if (!is.null(syncfile)) {
    # ...and write if it hasn't been synced or overite is TRUE
    if (!file.exists(syncfile) || overwrite) {
      save(trials, file = syncfile)
    }
  }
  
  
  return (trials)
}

  
# Run a trial and visualize results
trial <- run_3D_trials(abm, stop = socmod::fixated)
trial$get_outcomes()
print(trial$get_observations(), n=20)

# Analyse the prevalence of adaptive behavior across trials 
prevalence_summary <- summarise_prevalence(trial)

plot_prevalence(trial, tracked_behaviors = "Adaptive")


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
# Alter the probability of disaster 
disaster_probability <- c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)
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







