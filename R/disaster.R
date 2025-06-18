# R/disaster.R

# =============================================================================
# DISASTER DYNAMICS
# =============================================================================

#' @title Compute Disaster Probability
#' @description Calculates the probability of a disaster occurring based on the proportion
#'   of Legacy agents and specified model parameters.
#' @param p_L Numeric, the proportion of Legacy agents among active agents.
#' @param params List, simulation parameters including `use_sigmoid`, `k_sigmoid`,
#'   `tau_sigmoid`, `pi_0`, and `alpha`.
#' @return Numeric, the calculated probability of disaster (pi_t).
compute_disaster_probability <- function(p_L, params) {
  if (params$use_sigmoid) {
    # Sigmoid model: Probability increases with p_L, controlled by steepness and threshold
    1 / (1 + exp(-params$k_sigmoid * (p_L - params$tau_sigmoid))) # may want to change this to a delayed response model 
  } else {
    # Linear model: Probability increases linearly with p_L
    pmin(1, pmax(0, params$pi_0 + params$alpha * p_L))
  }
}

#' @title Sample Disaster Occurrence and Severity
#' @description Determines if a disaster occurs in the current time step and, if so,
#'   calculates its severity based on agent states and parameters.
#' @param agents A data.frame of agents, including their `migrated` and `behavior` status.
#' @param params List, simulation parameters including `mu_0`, `sigma_0`, and `beta`.
#' @return A list containing:
#'   - `occurs`: Logical, TRUE if a disaster occurred, FALSE otherwise.
#'   - `severity`: Numeric, the severity of the disaster (0 if no disaster).
#'   - `pi_t`: Numeric, the calculated disaster probability for this step.
#'   - `p_L`: Numeric, the proportion of Legacy agents.
#'   - `p_A`: Numeric, the proportion of Adaptive agents.
sample_disaster <- function(agents, params) {
  # Identify and count active (non-migrated) agents
  active_agents <- agents[!agents$migrated, ]
  N_active <- nrow(active_agents)
  
  if (N_active == 0) {
    # If no active agents, no disaster can occur
    return(list(occurs = FALSE, severity = 0, pi_t = 0, p_L = 0, p_A = 0))
  }
  
  # Calculate proportions of Legacy and Adaptive agents among active agents
  p_L <- sum(active_agents$behavior == "L") / N_active
  p_A <- sum(active_agents$behavior == "A") / N_active
  
  # Calculate disaster probability (pi_t) based on p_L
  pi_t <- compute_disaster_probability(p_L, params)
  
  # Determine if a disaster occurs based on pi_t
  disaster_occurs <- rbinom(1, 1, pi_t) == 1
  
  severity <- 0 # Default severity to 0
  
  if (disaster_occurs) {
    # Calculate disaster severity from a normal distribution
    # Mean severity (mu_t) is fixed, but standard deviation (sigma_t) increases with p_L
    mu_t <- params$mu_0
    sigma_t <- params$sigma_0 + params$beta * p_L
    severity <- rnorm(1, mu_t, sigma_t)
    severity <- pmax(0, severity)  # Ensure severity is non-negative
  }
  
  list(
    occurs = disaster_occurs,
    severity = severity,
    pi_t = pi_t,
    p_L = p_L,
    p_A = p_A
  )
}