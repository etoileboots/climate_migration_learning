# R/experiments.R

library(parallel)
library(foreach)
library(doParallel)

run_population_collapse_experiments <- function(n_reps = 50) {
  cat(sprintf("Running population collapse experiments with %d replications per scenario...\n", n_reps))
  
  scenarios <- list(
    # -------------------- Experiment 1.a --------------------
    "E1a_Severe_Overestimate_Prob" = list(P_mu_A_scale = 0.3, P_sigma_A_scale = 1, D_mu_A_scale = 1, D_sigma_A_scale = 1, evol_steps = 25),
    "E1a_Mild_Overestimate_Prob"   = list(P_mu_A_scale = 0.7, P_sigma_A_scale = 1, D_mu_A_scale = 1, D_sigma_A_scale = 1, evol_steps = 25),
    "E1a_Mild_Underestimate_Prob"  = list(P_mu_A_scale = 1.3, P_sigma_A_scale = 1, D_mu_A_scale = 1, D_sigma_A_scale = 1, evol_steps = 25),
    "E1a_Severe_Underestimate_Prob"= list(P_mu_A_scale = 4.0, P_sigma_A_scale = 1, D_mu_A_scale = 1, D_sigma_A_scale = 1, evol_steps = 25),
    
    # -------------------- Experiment 1.b --------------------
    "E1b_Severe_Overestimate_ProbSD" = list(P_mu_A_scale = 1, P_sigma_A_scale = 0.3, D_mu_A_scale = 1, D_sigma_A_scale = 1, evol_steps = 25),
    "E1b_Mild_Overestimate_ProbSD"   = list(P_mu_A_scale = 1, P_sigma_A_scale = 0.7, D_mu_A_scale = 1, D_sigma_A_scale = 1, evol_steps = 25),
    "E1b_Mild_Underestimate_ProbSD"  = list(P_mu_A_scale = 1, P_sigma_A_scale = 1.3, D_mu_A_scale = 1, D_sigma_A_scale = 1, evol_steps = 25),
    "E1b_Severe_Underestimate_ProbSD"= list(P_mu_A_scale = 1, P_sigma_A_scale = 2.0, D_mu_A_scale = 1, D_sigma_A_scale = 1, evol_steps = 25),
    
    # -------------------- Experiment 2.a --------------------
    "E2a_Severe_Overestimate_Severity" = list(P_mu_A_scale = 1, P_sigma_A_scale = 1, D_mu_A_scale = 0.3, D_sigma_A_scale = 1, evol_steps = 25),
    "E2a_Mild_Overestimate_Severity"   = list(P_mu_A_scale = 1, P_sigma_A_scale = 1, D_mu_A_scale = 0.7, D_sigma_A_scale = 1, evol_steps = 25),
    "E2a_Mild_Underestimate_Severity"  = list(P_mu_A_scale = 1, P_sigma_A_scale = 1, D_mu_A_scale = 1.3, D_sigma_A_scale = 1, evol_steps = 25),
    "E2a_Severe_Underestimate_Severity"= list(P_mu_A_scale = 1, P_sigma_A_scale = 1, D_mu_A_scale = 2.0, D_sigma_A_scale = 1, evol_steps = 25),
    
    # -------------------- Experiment 2.b --------------------
    "E2b_Severe_Overestimate_SeveritySD" = list(P_mu_A_scale = 1, P_sigma_A_scale = 1, D_mu_A_scale = 1, D_sigma_A_scale = 0.3, evol_steps = 25),
    "E2b_Mild_Overestimate_SeveritySD"   = list(P_mu_A_scale = 1, P_sigma_A_scale = 1, D_mu_A_scale = 1, D_sigma_A_scale = 0.7, evol_steps = 25),
    "E2b_Mild_Underestimate_SeveritySD"  = list(P_mu_A_scale = 1, P_sigma_A_scale = 1, D_mu_A_scale = 1, D_sigma_A_scale = 1.3, evol_steps = 25),
    "E2b_Severe_Underestimate_SeveritySD"= list(P_mu_A_scale = 1, P_sigma_A_scale = 1, D_mu_A_scale = 1, D_sigma_A_scale = 2.0, evol_steps = 25),
    
    # -------------------- Experiment 3 --------------------
    "E3_Instantaneous" = list(P_mu_A_scale = 1, P_sigma_A_scale = 1, D_mu_A_scale = 1, D_sigma_A_scale = 1, evol_steps = 0),
    "E3_Rapid"         = list(P_mu_A_scale = 1, P_sigma_A_scale = 1, D_mu_A_scale = 1, D_sigma_A_scale = 1, evol_steps = 10),
    "E3_Medium"        = list(P_mu_A_scale = 1, P_sigma_A_scale = 1, D_mu_A_scale = 1, D_sigma_A_scale = 1, evol_steps = 30),
    "E3_Gradual"       = list(P_mu_A_scale = 1, P_sigma_A_scale = 1, D_mu_A_scale = 1, D_sigma_A_scale = 1, evol_steps = 100)
  )
  
  # Set consistent network/social learning parameters
  for (name in names(scenarios)) {
    scenarios[[name]]$network_type <- "small_world"
    scenarios[[name]]$rewire_prob <- 0.1
    scenarios[[name]]$r <- 0.8
    scenarios[[name]]$kappa_min <- 0.3
    scenarios[[name]]$kappa_max <- 0.7
  }
  
  all_results <- list()
  num_cores <- min(parallel::detectCores() - 1, 8)
  if (num_cores < 1) num_cores <- 1
  doParallel::registerDoParallel(num_cores)
  cat(sprintf("Using %d cores for parallel processing.\n", num_cores))
  
  total_runs <- length(scenarios) * n_reps
  pb <- utils::txtProgressBar(min = 0, max = total_runs, style = 3)
  progress_counter <- 0
  
  for (s_name in names(scenarios)) {
    scenario_params_override <- scenarios[[s_name]]
    cat(sprintf("\nRunning scenario: %s\n", s_name))
    
    scenario_reps_results <- foreach(i = 1:n_reps, .combine = rbind,
                                     .packages = c("dplyr", "igraph", "tidyr"),
                                     .export = c("create_default_params", "run_simulation",
                                                 "initialize_agents", "sample_disaster", "update_vulnerabilities",
                                                 "apply_social_learning", "apply_stochastic_transitions", "apply_migration")
    ) %dopar% {
      current_params <- create_default_params()
      for (param_name in names(scenario_params_override)) {
        current_params[[param_name]] <- scenario_params_override[[param_name]]
      }
      sim_result <- run_simulation(current_params, verbose = FALSE)
      final_p_M <- tail(sim_result$p_M, 1)
      collapsed <- final_p_M >= 0.95
      time_to_collapse <- NA
      if (collapsed) {
        collapse_idx <- which(sim_result$p_M >= 0.95)[1]
        if (!is.na(collapse_idx)) {
          time_to_collapse <- sim_result$time[collapse_idx]
        }
      }
      max_p_A <- max(sim_result$p_A, na.rm = TRUE)
      time_to_max_adaptation <- sim_result$time[which.max(sim_result$p_A)]
      total_disasters <- tail(sim_result$n_disasters, 1)
      avg_disaster_severity <- mean(sim_result$disaster_severity[sim_result$disaster_severity > 0], na.rm = TRUE)
      if (is.nan(avg_disaster_severity)) avg_disaster_severity <- 0
      
      data.frame(
        scenario = s_name,
        replication = i,
        collapsed = collapsed,
        final_p_M = final_p_M,
        time_to_collapse = time_to_collapse,
        max_p_A = max_p_A,
        time_to_max_adaptation = time_to_max_adaptation,
        total_disasters = total_disasters,
        avg_disaster_severity = avg_disaster_severity,
        P_mu_A_scale = current_params$P_mu_A_scale,
        P_sigma_A_scale = current_params$P_sigma_A_scale,
        D_mu_A_scale = current_params$D_mu_A_scale,
        D_sigma_A_scale = current_params$D_sigma_A_scale,
        evol_steps = current_params$evol_steps
      )
    }
    
    all_results[[s_name]] <- scenario_reps_results
    progress_counter <- progress_counter + n_reps
    utils::setTxtProgressBar(pb, progress_counter)
  }
  
  doParallel::stopImplicitCluster()
  close(pb)
  
  final_df <- do.call(rbind, all_results)
  cat("\n=== POPULATION COLLAPSE EXPERIMENT SUMMARY ===\n")
  collapse_summary <- final_df %>%
    group_by(scenario) %>%
    summarise(
      collapse_rate = mean(collapsed),
      avg_time_to_collapse = mean(time_to_collapse, na.rm = TRUE),
      avg_max_adaptation = mean(max_p_A),
      .groups = 'drop'
    )
  print(collapse_summary)
  
  cat("\nPopulation collapse experiments completed.\n")
  return(final_df)
}

run_population_collapse_experiments(n_reps=1)