# R/network.R

# =============================================================================
# NETWORK GENERATION
# =============================================================================

#' @title Create Agent Network
#' @description Generates an igraph network based on specified parameters.
#' @param N Number of agents in the network.
#' @param network_type Type of network to generate ("small_world", "lattice", "random").
#' @param rewire_prob Rewiring probability for small-world networks.
#' @param k_nearest Number of nearest neighbors for lattice or small-world networks.
#' @return An igraph object representing the agent network.
create_network <- function(N, network_type, rewire_prob, k_nearest) {
  g <- switch(
    network_type,
    "small_world" = igraph::sample_smallworld(1, N, k_nearest, rewire_prob),
    "lattice" = igraph::sample_smallworld(1, N, k_nearest, 0), # Small-world with no rewiring for lattice
    "random" = igraph::sample_gnp(N, log(N)/N), # Erdos-Renyi G(n,p) with p = log(N)/N for connectivity
    stop("Invalid network_type. Choose 'small_world', 'lattice', or 'random'.")
  )
  return(g)
}