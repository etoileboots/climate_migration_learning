# R/simulation.R

# =============================================================================
# SIMULATION CORE
# =============================================================================

#' @title Run the Agent-Based Model Simulation
#' @description Executes the ABM simulation for a given set of parameters
#'   over a specified number of time steps.
#' @param params A list of simulation parameters.
#' @param verbose Logical, if TRUE, prints progress updates during the simulation.
#' @return A data.frame containing simulation results over time for key metrics.
run_simulation <- function(params, verbose = TRUE) {
  # Initialize agents and network based on parameters
  init_state <- initialize_agents(params) # This function returns a list with agents and network
  agents <- init_state$agents
  network <- init_state$network

  # Initialize data frame to store simulation results
  results <- data.frame(
    time = integer(),
    p_L = numeric(),            # Proportion of Legacy agents (relative to total N)
    p_A = numeric(),            # Proportion of Adaptive agents (relative to total N)
    p_M = numeric(),            # Proportion of Migrated agents (relative to total N)
    pi_t = numeric(),           # Disaster probability at each time step
    avg_vulnerability = numeric(), # Average vulnerability of active agents
    n_disasters = integer(),    # Cumulative number of disaster events
    disaster_severity = numeric(), # Severity of disaster at current time step (0 if no disaster)
    n_migrations = integer()    # Number of new migrations that occurred in the current time step
    # ! FUTURE PLANNING: the proportion of individuals that change status, and to what status they change could be interesting
  )
  
  disaster_count <- 0 # Counter for cumulative disaster events
  
  # Simulation Loop
  for (t in 1:params$T) {
    # Store the number of migrated agents before this step's migration phase
    migrated_before_step <- sum(agents$migrated)
    adapted_before_step <- sum(agents$behavior == "A")
    # 1. Sample disaster occurrence and severity for the current time step
    disaster <- sample_disaster(agents, params)
    
    if (disaster$occurs) {
      disaster_count <- disaster_count + 1
    }
    
    # 2. Update agent vulnerabilities
    agents <- update_vulnerabilities(agents, network, disaster, params)
    
    # 3. Apply behavioral transitions (social learning and stochastic "mutation")
    agents <- apply_social_learning(agents, network, params) # social
    agents <- apply_stochastic_transitions(agents, params) # mutation 
    
    # 4. Apply migration based on updated vulnerabilities
    agents <- apply_migration(agents, params)
    
    # Calculate the number of agents who newly migrated in this time step
    new_migrations_this_step <- sum(agents$migrated) - migrated_before_step
    
    # Calculate current state metrics for recording
    active_agents_current_step <- agents[!agents$migrated, ]
    N_active_current_step <- nrow(active_agents_current_step)
    
    # Proportions of the total initial population (N)
    p_L_total <- sum(agents$behavior == "L" & !agents$migrated) / params$N
    p_A_total <- sum(agents$behavior == "A" & !agents$migrated) / params$N
    p_M_total <- sum(agents$migrated) / params$N
    
    # Average vulnerability of only the currently active agents
    if (N_active_current_step > 0) {
      avg_vuln_active <- mean(active_agents_current_step$vulnerability)
    } else {
      avg_vuln_active <- 0 # If no active agents, average vulnerability is 0
    }
    
    # Check for equilibrium state (no change in adaptors or migrations)
    new_adaptors <- sum(agents$behavior == "A") - adapted_before_step
    if (new_adaptors == 0 && new_migrations_this_step==0){
      params$adaptation_switch = TRUE 
    }
    # Record current time step's results
    results <- rbind(results, data.frame(
      time = t,
      p_L = p_L_total,
      p_A = p_A_total,
      p_M = p_M_total,
      pi_t = disaster$pi_t,
      avg_vulnerability = avg_vuln_active,
      n_disasters = disaster_count,
      disaster_severity = disaster$severity,
      n_migrations = new_migrations_this_step
    ))
    
    # Print verbose output at specified intervals (every 50 time steps)
    if (verbose && t %% 50 == 0) {
      cat(sprintf("Time=%d: p_L=%.3f, p_A=%.3f, p_M=%.3f, Disasters=%d, Active_Agents=%d\n",
                  t, p_L_total, p_A_total, p_M_total, disaster_count, N_active_current_step))
    }
    
    # Check for simulation convergence or termination condition
    if (N_active_current_step == 0 || p_L_total == 0 || p_A_total == 0) {
      if (verbose) {
        cat(sprintf("All agents migrated or inactive at Time = %d\n", t))
      }
      break  # End simulation if all agents have migrated or relevant proportions are zero
    }
    
  }
  return(data.frame(results))
}