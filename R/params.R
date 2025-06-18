# R/params.R

# =============================================================================
# MODEL PARAMETERS STRUCTURE
# =============================================================================

#' @title Create Default Parameters for the ABM
#' @description Initializes a list of default parameters for the Agent-Based Model,
#'   tailored to the model's disaster, vulnerability, and behavioral dynamics.
#' @return A list of simulation parameters.
create_default_params <- function() {
  list(
    # Basic simulation parameters
    N = 100,                    # Number of agents
    T = 200,                     # Total simulation time steps
    p_L_0 = 0.8, # Proportion of individuals with legacy behavior at t=0
    p_A_0 = 0.2, # Proportion of individuals with adaptive behavior at t=0

    # Network parameters
    network_type = "small_world", # "small_world", "lattice", "random"
    rewire_prob = 0.1,           # Rewiring probability for small-world networks
    k_nearest = 6,               # Average degree for lattice/small-world networks
    
    # Agent parameters (heterogeneous)
    theta_min = 0.5,             # Minimum proportion of neighbors performing a behavior to consider switching 
    theta_max = 0.7,             # Maximum proportion of neighbors performing a behavior to consider switching 
    L_vulnerability = 0.5, # Initial vulnerability value of legacy agents 
    A_vulnerability = 0.2, # Initial vulnerability of adaptive agents 
    
    # Social learning parameters
    r = 0.8,                     # Base probability of adopting a new behavior when social conditions are met
    epsilon = 0.1,              # Probability of stochastic behavior transition for agents (mutation)
    kappa_min = 0.3,             # Minimum individual adaptation cost barrier (0.0 means no barrier)
    kappa_max = 0.7,             # Maximum individual adaptation cost barrier (0.5 means up to 50% reduction in adoption probability)
    
    # Disaster dynamics parameters
    pi_0 = 0.1,                 # Base disaster probability (for linear model)
    alpha = 0.3,                 # Linear disaster frequency coefficient (for linear model)
    mu_0 = 0.1,                 # Base disaster severity mean (for normal distribution)
    sigma_0 = 0.05,              # Base disaster severity standard deviation (for normal distribution)
    beta = 1,                  # Coefficient for legacy behavior impact on disaster severity uncertainty
    
    use_sigmoid = TRUE,          # Flag to use sigmoid vs. linear model for disaster probability
    k_sigmoid = 1.5,              # Steepness parameter for sigmoid disaster probability model
    tau_sigmoid = 0.85,           # Threshold parameter for sigmoid disaster probability model
    
    # Vulnerability dynamics parameters
    v_mig = 1,                 # Migration threshold for agents' vulnerability
    c = 0.05,                    # Reduction in vulnerability for agents under community protection
    gamma = 0.05,                # Reduction in vulnerability from adaptation
    theta_comm = 0.5             # Community protection threshold (proportion of adaptive neighbors needed for protection)
  )
}