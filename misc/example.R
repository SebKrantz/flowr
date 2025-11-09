library(fastverse)
fastverse_extend(flowr, sf, igraph)

process_od_matrix <- function(od_matrix_directory, cargo_type, period = NULL) {
  files <- list.files(od_matrix_directory)
  files <- files[files %ilike% period]
  # cargo_type = c("Container", "Drybulk", "Liquidbulk", "General", "HighValue")
  # period = "2019"
  sapply(cargo_type, function(x) {
    fread(paste(od_matrix_directory, files[files %ilike% x][1], sep = "/")) |> qM(1)
  }, simplify = FALSE)
}

# GCC Scenario
od_matrix <- process_od_matrix("data/OD_Matrix", "Container", "2019")
# base_network <- st_read("data/network/base_network.shp")
network_gc_cnt <- st_read("data/network/base_network_with_gc_containerized")
zone_nodes <- st_read("data/zone_nodes/network_nodes.shp")

mapview::mapview(network_gc_cnt) + mapview::mapview(zone_nodes, col.regions = "red")

graph_df <- network_gc_cnt |>
  linestring_to_graph() |>
  add_vars(cost = network_gc_cnt$generalize) |>
  create_undirected_graph()

nodes_df <- nodes_from_graph(graph_df)

# Mapping zones to nodes
nearest_nodes <- st_nearest_feature(zone_nodes, st_as_sf(nodes_df, coords = c("X", "Y"), crs = 4326))

# Process OD Matrix
od_matrix <- od_matrix$Container
if(!identical(dim(od_matrix), rep(nrow(zone_nodes), 2))) stop("Zones and OD-Matrix Matrix Mismatch")

od_matrix_long <- data.frame(from = rep.int(nearest_nodes, ncol(od_matrix)),
                             to = rep(nearest_nodes, each = nrow(od_matrix)),
                             flow = vec(od_matrix)) |>
                  fsubset(is.finite(flow) & flow > 0)

# Create Graph igraph: best so far
g <- graph_from_data_frame(graph_df |> fselect(from, to), directed = FALSE)
g <- delete_vertex_attr(g, "name")
igraph_options(return.vs.es = FALSE) # sparsematrices = TRUE

# Distance Matrix
dmat <- distances(g, mode = "out", weights = graph_df$cost)
if(!identical(dim(dmat), rep(nrow(nodes_df), 2))) stop("Nodes and Distance Matrix Mismatch")

# Test Shortest Paths
shortest_paths(g, from = 1, to = 1:100, weights = graph_df$cost, output = "epath")$epath

# Some objects
beta_PSL <- -1 # Path-Sized Logit parameter
cost <- graph_df$cost # Edge cost
# Edge incidence across selected routes
delta_ks <- integer(nrow(graph_df))
# Final flows vector
final_flows <- numeric(nrow(graph_df))

attach(od_matrix_long)
# Now iterating across OD-pairs
# TODO: could restrict that other nodes must be in the direction of travel and not behind destination node
system.time({
for (i in seq_row(od_matrix_long)) {
  d_ij <- dmat[from[i], to[i]]
  d_ikj <- dmat[from[i], ] + dmat[, to[i]] # from i to all other nodes k and from these nodes k to j (basically dmat + t(dmat)?)
  short_detour_ij <- d_ikj < 1.5 * d_ij
  short_detour_ij[d_ikj <= d_ij + .Machine$double.eps*1e3] <- FALSE # Exclude nodes k that are on the shortest path
  # which(d_ij == d_ikj) # These are the nodes on the direct path from i to j which yield the shortest distance.
  ks <- which(short_detour_ij)
  cost_ks <- d_ikj[ks]
  # Now Path-Sized Logit: Need to compute overlap between routes
  # Could still optimize calls to shortest_paths(), e.g., go to C directly.
  # We add the shortest path at the end of paths1
  paths1 <- shortest_paths(g, from = from[i], to = c(ks, to[i]), weights = cost, output = "epath")$epath
  paths2 <- shortest_paths(g, from = to[i], to = ks, weights = cost, output = "epath")$epath
  shortest_path <- paths1[[length(paths1)]]

  # # Check
  # cost_ks[k] == sum(cost[paths1[[k]]]) + sum(cost[paths2[[k]]])

  # Get indices of paths that do not contain duplicate edges
  no_dups <- flowr:::check_path_duplicates(paths1, paths2, delta_ks)

  # # Number of routes in choice set that use link j
  # for (k in no_dups) {
  #   delta_ks[paths1[[k]]] <- delta_ks[paths1[[k]]] + 1L
  #   delta_ks[paths2[[k]]] <- delta_ks[paths2[[k]]] + 1L
  # }
  # delta_ks[shortest_path] <- delta_ks[shortest_path] + 1L
  #
  # # Correction factors for each route k
  # gamma_ks <- sapply(no_dups, function(k) {
  #   path <- c(paths1[[k]], paths2[[k]])
  #   sum(cost[path] / delta_ks[path]) / cost_ks[k]
  # })
  # gamma_1 <- sum(cost[shortest_path] / delta_ks[shortest_path]) / d_ij
  #
  # # Now the PS-MNL
  # prob_ks <- proportions(exp(c(cost_ks[no_dups], d_ij) + beta_PSL * c(gamma_ks, gamma_1)))
  #
  # # Need to reset delta_ks
  # delta_ks[] <- 0L
  #
  # # Assign result to edges:
  # for (k in no_dups) {
  #   final_flows[paths1[[k]]] <- final_flows[paths1[[k]]] + flow[i] * prob_ks[k]
  # }
  # final_flows[shortest_path] <- final_flows[shortest_path] + flow[i] * prob_ks[length(prob_ks)]
  flowr:::compute_path_sized_logit(paths1, paths2, no_dups, shortest_path,
                                     cost, cost_ks, d_ij, lambda_PSL, beta_PSL,
                                     flow[i], delta_ks, final_flows)
}
})
detach(od_matrix_long)

descr(final_flows)

network_gc_cnt$final_flows <- NA_real_
network_gc_cnt$final_flows[attr(graph_df, "group.starts")] <- final_flows
mapview::mapview(network_gc_cnt, zcol = "final_flows") +
  mapview::mapview(zone_nodes, col.regions = "red")
