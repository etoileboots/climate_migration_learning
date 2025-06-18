# R/behavior.R

# =============================================================================
# BEHAVIORAL TRANSITIONS
# =============================================================================

#' @title Apply Social Learning
#' @description Updates agent behavior based on social learning, where Legacy agents
#'   can switch to Adaptive if a sufficient proportion of their active neighbors
#'   are Adaptive and a stochastic condition is met. This incorporates an
#'   individual adaptation barrier (`kappa_i`) which reduces the effective
#'   probability of adoption.
#' @param agents A data.frame of agents, including `migrated`, `behavior`, `theta_i`, and `kappa_i`.
#' @param network The igraph network object.
#' @param params List, simulation parameters including `r` (adoption probability).
#' @return The updated agents data.frame.

apply_social_learning <- function(agents, network, params) {
  N <- nrow(agents)
  
  for (i in 1:N) {
    # Only Legacy agents who have not migrated can potentially switcj
    if (agents$migrated[i]) {
      next
    }
    
    # Get all neighbors of the current agent 'i'
    neighbors_idx <- igraph::neighbors(network, i, mode = "all")
    
    if (length(neighbors_idx) == 0) { 
      next # No neighbors, so no social learning
    }
    
    # Filter to only consider active (non-migrated) neighbors
    active_neighbors <- neighbors_idx[!agents$migrated[neighbors_idx]]
    
    if (length(active_neighbors) == 0) {
      next # No active neighbors means no social learning
    }
    
    # Count active neighbors' behaviors
    adaptive_neighbors_count <- sum(agents$behavior[active_neighbors] == "A")
    legacy_neighbors_count <- sum(agents$behavior[active_neighbors] == "L")
    total_active_neighbors_count <- length(active_neighbors)
    
    # Avoid division by zero
    if (total_active_neighbors_count > 0) {
      adaptive_fraction <- adaptive_neighbors_count / total_active_neighbors_count
      legacy_fraction <- legacy_neighbors_count / total_active_neighbors_count
      current_behavior <- agents$behavior[i]
      theta <- agents$theta_i[i]
      
      # In the case of switching from Legacy to Adaptive
      if (current_behavior == "L" && adaptive_fraction >= theta) {
        # Adoption of Adaptive behavior is dependent on r and weighted by 
        # kappa_i, the base adoption probability and personal barrier to adoption 
        r_effective <- params$r * (1 - agents$kappa_i[i])
        r_effective <- max(0, min(1, r_effective))  # Clamp to [0, 1]
        if (runif(1) < r_effective) {
          agents$behavior[i] <- "A"
        }
      }
      
      # In the case of switching from Adaptive back to Legacy
      else if (current_behavior == "A" && legacy_fraction >= theta) {
        # Adoption of legacy behavior is dependent on r, base adoption probability
        if (runif(1) < params$r) {
          agents$behavior[i] <- "L"
        }
      }
    }
  }
  agents
}
