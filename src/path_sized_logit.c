#include <R.h>
#include <Rinternals.h>

#ifndef SEXPPTR_RO
#define SEXPPTR_RO(x) ((const SEXP *)DATAPTR_RO(x))  // to avoid overhead of looped VECTOR_ELT
#endif

/**
 * compute_path_sized_logit
 *
 * Compute path-size logit probabilities for a set of alternative and shortest paths,
 * and attributes flows to network edges for each alternative. Used in transit assignment
 * and route choice modeling with explicit path-overlap correction via the path-size logit model.
 *
 * Parameters
 * ----------
 * paths1 : SEXP (list of numeric vectors)
 *      The first segment of the path. List of alternatives, each a numeric vector (1-based edge indices).
 * paths2 : SEXP (list of numeric vectors)
 *      The second segment of the path. List paralleling paths1, each a numeric vector (1-based edge indices).
 * no_dups : SEXP (integer vector)
 *      Indices (1-based) of paths to consider (those with no duplicate edges).
 * shortest_path : SEXP (numeric vector)
 *      Edge ids (1-based) forming the shortest path for this OD pair.
 * cost : SEXP (numeric vector)
 *      Cost of each edge in the network.
 * cost_ks : SEXP (numeric vector)
 *      Total cost for each alternative path with no duplicate edges.
 * d_ij : SEXP (length 1, double)
 *      Cost of the shortest path for this OD pair.
 * beta_PSL : SEXP (length 1, double)
 *      Path-size logit parameter. Determines strength of path-size correction.
 * flow : SEXP (length 1, double)
 *      OD matrix flow value (amount of travelers/objects).
 * delta_ks : SEXP (integer vector)
 *      An integer vector, same length as number of edges + 1, used as temporary workspace for edge overlap counts.
 * final_flows : SEXP (double vector)
 *      A vector, same length as number of edges, accumulates the assigned flow to each edge across all paths.
 * free_delta_ks : SEXP (logical scalar)
 *      Whether to reset delta_ks to 0 for all affected edges at end (usually TRUE).
 *
 * Returns
 * -------
 * prob_ks : SEXP (double vector of length (n_no_dups + 1))
 *      Probability vector. The first element corresponds to the shortest path, and the next n_no_dups elements
 *      correspond to the alternative paths given by no_dups.
 *
 * Details
 * -------
 * Computes path overlap factors (gamma) for alternatives and the shortest path.
 * Calculates logit probabilities with path-size correction. Updates a temporary vector delta_ks to count the overlap
 * of network edges among candidate paths. Assigns OD flow to edges in final_flows by the chosen probabilities.
 * Resets delta_ks if requested.
 *
 * Edge numbering: All edge ids in the paths and shortest_path are assumed to be 1-based.
 */
SEXP compute_path_sized_logit(SEXP paths1, SEXP paths2, SEXP no_dups, SEXP shortest_path,
                              SEXP cost, SEXP cost_ks, SEXP d_ij, SEXP beta_PSL,
                              SEXP flow, SEXP delta_ks, SEXP final_flows, SEXP free_delta_ks) {

  int n_no_dups = length(no_dups);
  const SEXP *paths1_ptr = SEXPPTR_RO(paths1);
  const SEXP *paths2_ptr = SEXPPTR_RO(paths2);
  int *no_dups_ptr = INTEGER(no_dups);
  double *shortest_path_ptr = REAL(shortest_path);
  int shortest_path_len = length(shortest_path);
  double *cost_ptr = REAL(cost);
  double *cost_ks_ptr = REAL(cost_ks);
  double d_ij_val = REAL(d_ij)[0];
  double beta_PSL_val = asReal(beta_PSL);
  double flow_val = asReal(flow);
  int *delta_ptr = INTEGER(delta_ks);
  double *final_flows_ptr = REAL(final_flows);

  // Step 1: Update delta_ks for paths in no_dups
  // delta_ks stores, for each edge, how many times it appears in the alternatives under consideration
  for (int idx = 0; idx < n_no_dups; idx++) {
    int k = no_dups_ptr[idx] - 1; // Convert to 0-based
    int len1 = length(paths1_ptr[k]);
    int len2 = length(paths2_ptr[k]);
    double *p1 = REAL(paths1_ptr[k]);
    double *p2 = REAL(paths2_ptr[k]);
    for (int i = 0; i < len1; i++) delta_ptr[(int)p1[i]]++;
    for (int i = 0; i < len2; i++) delta_ptr[(int)p2[i]]++;
  }
  // Update delta_ks for shortest_path
  for (int i = 0; i < shortest_path_len; i++) delta_ptr[(int)shortest_path_ptr[i]]++;

  // Step 2: Compute gamma_ks and gamma_1 (path-size factors for each alternative and the shortest path)
  double *gamma_ks = (double *) R_alloc(n_no_dups, sizeof(double));
  double gamma_1 = 0.0;

  for (int idx = 0; idx < n_no_dups; idx++) {
    int k = no_dups_ptr[idx] - 1;
    int len1 = length(paths1_ptr[k]);
    int len2 = length(paths2_ptr[k]);
    double *p1 = REAL(paths1_ptr[k]);
    double *p2 = REAL(paths2_ptr[k]);
    double sum = 0.0;
    for (int i = 0; i < len1; i++) {
      int edge = (int)p1[i];
      sum += cost_ptr[edge-1] / delta_ptr[edge];
    }
    for (int i = 0; i < len2; i++) {
      int edge = (int)p2[i];
      sum += cost_ptr[edge-1] / delta_ptr[edge];
    }
    gamma_ks[idx] = sum / cost_ks_ptr[k];
    // Ensure gamma is positive to avoid log(0) or log(negative)
    if (gamma_ks[idx] <= 0.0) gamma_ks[idx] = 1e-10;
  }

  // Compute gamma_1 for shortest_path
  for (int i = 0; i < shortest_path_len; i++) {
    int edge = (int)shortest_path_ptr[i];
    gamma_1 += cost_ptr[edge-1] / delta_ptr[edge];
  }
  gamma_1 /= d_ij_val;
  // Ensure gamma_1 is positive to avoid log(0) or log(negative)
  if (gamma_1 <= 0.0) gamma_1 = 1e-10;

  // Step 3: Compute prob_ks using proportions
  SEXP prob_ks = PROTECT(allocVector(REALSXP, n_no_dups + 1));
  double *prob_ptr = REAL(prob_ks);
  memset(prob_ptr, 0, (n_no_dups + 1) * sizeof(double));
  double sum_exp = 0.0;
  for (int idx = 0; idx < n_no_dups; idx++) {
    prob_ptr[idx+1] = exp(-cost_ks_ptr[no_dups_ptr[idx]-1] + beta_PSL_val * log(gamma_ks[idx]));
    sum_exp += prob_ptr[idx+1];
  }
  prob_ptr[0] = exp(-d_ij_val + beta_PSL_val * log(gamma_1));
  sum_exp += prob_ptr[0];

  // Normalize to get probabilities
  for (int i = 0; i <= n_no_dups; i++) {
    prob_ptr[i] /= sum_exp;
  }

  // Step 4: Reset delta_ks if requested (to reuse buffer for next OD pair)
  if(LOGICAL(free_delta_ks)[0]) {
    for (int idx = 0; idx < n_no_dups; idx++) {
      int k = no_dups_ptr[idx] - 1;
      int len1 = length(paths1_ptr[k]);
      int len2 = length(paths2_ptr[k]);
      double *p1 = REAL(paths1_ptr[k]);
      double *p2 = REAL(paths2_ptr[k]);
      for (int i = 0; i < len1; i++) delta_ptr[(int)p1[i]] = 0;
      for (int i = 0; i < len2; i++) delta_ptr[(int)p2[i]] = 0;
    }
    for (int i = 0; i < shortest_path_len; i++) delta_ptr[(int)shortest_path_ptr[i]] = 0;
  }

  // Step 5: Update final_flows with fractional flow assigned to each edge, for each alternative (including shortest path)
  for (int idx = 0; idx < n_no_dups; idx++) {
    int k = no_dups_ptr[idx] - 1;
    int len1 = length(paths1_ptr[k]);
    int len2 = length(paths2_ptr[k]);
    double *p1 = REAL(paths1_ptr[k]);
    double *p2 = REAL(paths2_ptr[k]);
    double prob_val = flow_val * prob_ptr[idx+1];
    for (int i = 0; i < len1; i++) final_flows_ptr[(int)p1[i] - 1] += prob_val;
    for (int i = 0; i < len2; i++) final_flows_ptr[(int)p2[i] - 1] += prob_val;
  }
  sum_exp = flow_val * prob_ptr[0];
  for (int i = 0; i < shortest_path_len; i++) {
    final_flows_ptr[(int)shortest_path_ptr[i] - 1] += sum_exp;
  }

  UNPROTECT(1);
  return prob_ks;
}
