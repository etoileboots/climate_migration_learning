# R/agents.R

# =============================================================================
# AGENT INITIALIZATION
# =============================================================================

#' @title Initialize Agents and Network
#' @description Initializes the data.frame of agents with their attributes and
#'   creates the agent network based on simulation parameters.
#'   This function returns both the initialized agents data.frame and the network object.
#' @param params List of simulation parameters (N, theta_min/max, network_type,
#'   rewire_prob, k_nearest).
#' @return A list containing the initialized `agents` data.frame and the `network` igraph object.
initialize_agents <- function(params) {
  N <- params$N
  theta_min <- params$theta_min  # Lower bound of social threshold for learning 
  theta_max <- params$theta_max # Upper bound of social threshold for learning 
  kappa_min <- params$kappa_min # Lower bound of the barrier to transition to adaptive behaviors 
  kappa_max <- params$kappa_max # Upper bound of the barrier to transition to adaptive behavior
  p_L_0 <- params$p_L_0 # Proportion of individuals with legacy behavior at t=0
  p_A_0 <- params$p_A_0 # Proportion of individuals with adaptive behavior at t=0
  L_vulnerability <- params$L_vulnerability
  A_vulnerability <- params$A_vulnerability
  # Assign behaviors
  behavior <- sample(c("L", "A"), N, replace = TRUE, prob = c(p_L_0, p_A_0))
  
  # Assign vulnerability based on behavior type
  vulnerability <- ifelse(
    behavior == "L",
    L_vulnerability,  #Assumption: legacy agents are more vulnerable, later make this a unif or normal 
    A_vulnerability   # Adaptive agents are less vulnerable
  )
  
  # Build the agents data frame
  agents <- data.frame(
    id = 1:N,
    behavior = behavior,
    vulnerability = vulnerability,
    migrated = FALSE,
    migrated_at_time = NA,
    theta_i = theta_min, # LET's KEEP THIS CONSTANT FOR NOW runif(N, theta_min, theta_max), # Social learning threshold
    kappa_i = kappa_min # LETS KEEP THIS CONSTANT FOR NOW runif(N, kappa_min, kappa_max) # Adaptive behavior adoption threhsold 
  )
    
  # Create the network for agents
  g <- create_network(N = params$N,
                      network_type = params$network_type, 
                      rewire_prob = params$rewire_prob,
                      k_nearest = params$k_nearest)
  
  return(list(agents = agents, network = g))
}