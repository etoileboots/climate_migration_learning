# R/experiments.R

# Load required libraries for parallel processing
library(parallel)   # For detectCores
library(foreach)    # For parallel loops
library(doParallel) # Backend for foreach

# =============================================================================
# EXPERIMENTATION FUNCTIONS
# =============================================================================

#' @title Run Scenario Experiments
#' @description Executes the ABM simulation for multiple predefined scenarios,
#'   running each scenario for a specified number of replications.
#' @param n_reps Number of replications for each scenario.
#' @return A data.frame containing combined results from all scenarios and replications.
run_scenario_experiments <- function(n_reps = 20) {
  cat(sprintf("Running scenario experiments with %d replications per scenario...\n", n_reps))
  
  # Define different simulation scenarios by overriding default parameters
  scenarios <- list(
    "Baseline" = list(use_sigmoid = TRUE, k_sigmoid = 1.5, tau_sigmoid = 0.85, # soft, slow rise in risk starting when p_L is high 
                      beta = 1, c = 0.05, gamma = 0.05, theta_comm = 0.5),
    "High Risk, Low Adaptation" = list(use_sigmoid = TRUE,  k_sigmoid = 1.5, tau_sigmoid = 0.85, # Higher disaster risk profile
                                       beta = 1, c = 0.03, gamma = 0.03, theta_comm = 0.5), # Lower adaptation/protection benefits
    "Low Risk, High Adaptation" = list(use_sigmoid = TRUE, k_sigmoid = 5, tau_sigmoid = 0.6, # Lower disaster risk profile
                                       beta = 1, c = 0.07, gamma = 0.07, theta_comm = 0.5) # Higher adaptation/protection benefits
  )
  
  all_results <- list() # List to store results from all scenarios
  
  # Set up parallel processing
  num_cores <- 2 #parallel::detectCores() - 1 # Use all but one core to leave one for system tasks
  if (num_cores < 1) num_cores <- 1 # Ensure at least one core is used
  doParallel::registerDoParallel(num_cores)
  cat(sprintf("Using %d cores for parallel processing.\n", num_cores))
  
  # Initialize progress bar
  pb <- utils::txtProgressBar(min = 0, max = length(scenarios) * n_reps, style = 3)
  progress_counter <- 0
  
  for (s_name in names(scenarios)) {
    scenario_params_override <- scenarios[[s_name]]
    cat(sprintf("\n  Running scenario: %s\n", s_name))
    
    # Run replications for the current scenario in parallel
    scenario_reps_results <- foreach(i = 1:n_reps, .combine = rbind,
                                     .packages = c("dplyr", "igraph", "tidyr")) %dopar% {
                                       # Create a fresh set of default parameters for each replication
                                       current_params <- create_default_params()
                                       # Override default parameters with scenario-specific values
                                       for (param_name in names(scenario_params_override)) {
                                         current_params[[param_name]] <- scenario_params_override[[param_name]]
                                       }
                                       
                                       # Run the simulation
                                       sim_result <- run_simulation(current_params, verbose = FALSE)
                                       
                                       # Add scenario and replication identifiers to the results
                                       sim_result$scenario <- s_name
                                       sim_result$replication <- i
                                       
                                       sim_result
                                     }
    all_results[[s_name]] <- scenario_reps_results
    
    # Update progress bar
    progress_counter <- progress_counter + n_reps
    utils::setTxtProgressBar(pb, progress_counter)
  }
  
  doParallel::stopImplicitCluster() # Stop parallel cluster
  gc()
  final_df <- do.call(rbind, all_results) # Combine results from all scenarios
  cat("\nScenario experiments completed.\n")
  return(final_df)
}


#' @title Run Sensitivity Analysis
#' @description Runs the ABM simulation by systematically varying a single parameter
#'   over a predefined range of values, for a specified number of replications.
#' @param n_reps Number of replications for each parameter value.
#' @return A data.frame containing the final migration proportions for each
#'   parameter value and replication.
run_sensitivity_analysis <- function(n_reps = 10) {
  cat(sprintf("Running sensitivity analysis with %d replications per parameter value...\n", n_reps))
  
  # Define parameters to test and their specific ranges of values
  sensitivity_params <- list(
    "k_sigmoid" = c(5, 10, 15, 20),         # Steepness of disaster probability sigmoid
    "tau_sigmoid" = c(0.2, 0.4, 0.6, 0.8),  # Midpoint of disaster probability sigmoid
    "beta" = c(0.1, 0.2, 0.3),              # Legacy behavior impact on disaster severity std
    "r" = c(0.5, 0.7, 0.9),                 # Social learning adoption probability
    "epsilon" = c(0.005, 0.01, 0.02),       # Stochastic transition probability
    "c" = c(0.03, 0.05, 0.07),              # Cost/reduction in vulnerability from community protection
    "gamma" = c(0.03, 0.05, 0.07),          # Additional reduction for adaptive agents
    "theta_comm" = c(0.3, 0.5, 0.7),        # Community protection threshold
    "v_mig" = c(0.6, 0.8, 1.0)              # Migration threshold
  )
  
  sensitivity_results <- list() # List to store results from all parameter variations
  
  # Set up parallel processing
  num_cores <- parallel::detectCores() - 1
  if (num_cores < 1) num_cores <- 1
  doParallel::registerDoParallel(num_cores)
  cat(sprintf("Using %d cores for parallel processing.\n", num_cores))
  
  # Calculate total number of runs for progress bar
  total_runs <- sum(sapply(sensitivity_params, length)) * n_reps
  pb <- utils::txtProgressBar(min = 0, max = total_runs, style = 3)
  progress_counter <- 0
  
  for (param_name in names(sensitivity_params)) {
    param_values <- sensitivity_params[[param_name]]
    cat(sprintf("\n  Varying parameter: %s\n", param_name))
    
    # Run simulations for each value of the current parameter in parallel
    param_runs_results <- foreach(val = param_values, .combine = rbind,
                                  .packages = c("dplyr", "igraph", "tidyr")) %dopar% {
                                    val_results <- list()
                                    for (i in 1:n_reps) {
                                      # Create a fresh set of default parameters for each run
                                      current_params <- create_default_params()
                                      current_params[[param_name]] <- val # Override the specific parameter being tested
                                      
                                      sim_result <- run_simulation(current_params, verbose = FALSE)
                                      
                                      # Extract the final migration proportion (p_M)
                                      final_p_M <- tail(sim_result$p_M, 1)
                                      
                                      val_results[[length(val_results) + 1]] <- data.frame(
                                        parameter = param_name,
                                        value = val,
                                        replication = i,
                                        p_M_final = final_p_M
                                      )
                                    }
                                    do.call(rbind, val_results) # Combine results for this parameter value
                                  }
    sensitivity_results[[param_name]] <- param_runs_results
    
    # Update progress bar
    progress_counter <- progress_counter + (length(param_values) * n_reps)
    utils::setTxtProgressBar(pb, progress_counter)
  }
  
  doParallel::stopImplicitCluster() # Stop parallel cluster
  gc()
  final_df <- do.call(rbind, sensitivity_results) # Combine results from all parameters
  cat("\nSensitivity analysis completed.\n")
  return(final_df)
}
