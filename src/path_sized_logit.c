#include <R.h>
#include <Rinternals.h>

#undef SEXPPTR_RO
#define SEXPPTR_RO(x) ((const SEXP *)DATAPTR_RO(x))  // to avoid overhead of looped VECTOR_ELT


SEXP compute_path_sized_logit(SEXP paths1, SEXP paths2, SEXP no_dups, SEXP shortest_path,
                              SEXP cost, SEXP cost_ks, SEXP d_ij, SEXP beta_PSL, SEXP flow,
                              SEXP delta_ks, SEXP final_flows) {

  int n_no_dups = length(no_dups);
  const SEXP *paths1_ptr = SEXPPTR_RO(paths1);
  const SEXP *paths2_ptr = SEXPPTR_RO(paths2);
  int *no_dups_ptr = INTEGER(no_dups);
  double *shortest_path_ptr = REAL(shortest_path);
  int shortest_path_len = length(shortest_path);
  double *cost_ptr = REAL(cost);
  double *cost_ks_ptr = REAL(cost_ks);
  double d_ij_val = REAL(d_ij)[0];
  double beta_PSL_val = REAL(beta_PSL)[0];
  double flow_val = REAL(flow)[0];
  int *delta_ptr = INTEGER(delta_ks);
  double *final_flows_ptr = REAL(final_flows);

  // Step 1: Update delta_ks for paths in no_dups
  for (int idx = 0; idx < n_no_dups; idx++) {
    int k = no_dups_ptr[idx] - 1; // Convert to 0-based
    int len1 = length(paths1_ptr[k]);
    int len2 = length(paths2_ptr[k]);
    double *p1 = REAL(paths1_ptr[k]);
    double *p2 = REAL(paths2_ptr[k]);
    for (int i = 0; i < len1; i++) delta_ptr[(int)p1[i] - 1]++;
    for (int i = 0; i < len2; i++) delta_ptr[(int)p2[i] - 1]++;
  }
  // Update delta_ks for shortest_path
  for (int i = 0; i < shortest_path_len; i++) delta_ptr[(int)shortest_path_ptr[i] - 1]++;

  // Step 2: Compute gamma_ks and gamma_1
  double *gamma_ks = (double *) R_alloc(n_no_dups, sizeof(double));
  double gamma_1 = 0.0;
  double sum_shortest = 0.0;

  for (int idx = 0; idx < n_no_dups; idx++) {
    int k = no_dups_ptr[idx] - 1;
    int len1 = length(paths1_ptr[k]);
    int len2 = length(paths2_ptr[k]);
    double *p1 = REAL(paths1_ptr[k]);
    double *p2 = REAL(paths2_ptr[k]);
    double sum = 0.0;
    for (int i = 0; i < len1; i++) {
      int edge = (int)p1[i] - 1;
      sum += cost_ptr[edge] / delta_ptr[edge];
    }
    for (int i = 0; i < len2; i++) {
      int edge = (int)p2[i] - 1;
      sum += cost_ptr[edge] / delta_ptr[edge];
    }
    gamma_ks[idx] = sum / cost_ks_ptr[idx];
  }

  // Compute gamma_1 for shortest_path
  for (int i = 0; i < shortest_path_len; i++) {
    int edge = (int)shortest_path_ptr[i] - 1;
    sum_shortest += cost_ptr[edge] / delta_ptr[edge];
  }
  gamma_1 = sum_shortest / d_ij_val;

  // Step 3: Compute prob_ks using proportions
  double *exp_vals = (double *) R_alloc(n_no_dups + 1, sizeof(double));
  double sum_exp = 0.0;
  for (int idx = 0; idx < n_no_dups; idx++) {
    exp_vals[idx] = exp(cost_ks_ptr[idx] + beta_PSL_val * gamma_ks[idx]);
    sum_exp += exp_vals[idx];
  }
  exp_vals[n_no_dups] = exp(d_ij_val + beta_PSL_val * gamma_1);
  sum_exp += exp_vals[n_no_dups];

  // Normalize to get probabilities
  SEXP prob_ks = PROTECT(allocVector(REALSXP, n_no_dups + 1));
  double *prob_ptr = REAL(prob_ks);
  for (int i = 0; i <= n_no_dups; i++) {
    prob_ptr[i] = exp_vals[i] / sum_exp;
  }

  // Step 4: Reset delta_ks
  for (int idx = 0; idx < n_no_dups; idx++) {
    int k = no_dups_ptr[idx] - 1;
    int len1 = length(paths1_ptr[k]);
    int len2 = length(paths2_ptr[k]);
    double *p1 = REAL(paths1_ptr[k]);
    double *p2 = REAL(paths2_ptr[k]);
    for (int i = 0; i < len1; i++) delta_ptr[(int)p1[i] - 1] = 0;
    for (int i = 0; i < len2; i++) delta_ptr[(int)p2[i] - 1] = 0;
  }
  for (int i = 0; i < shortest_path_len; i++) delta_ptr[(int)shortest_path_ptr[i] - 1] = 0;

  // Step 5: Update final_flows
  for (int idx = 0; idx < n_no_dups; idx++) {
    int k = no_dups_ptr[idx] - 1;
    int len1 = length(paths1_ptr[k]);
    int len2 = length(paths2_ptr[k]);
    double *p1 = REAL(paths1_ptr[k]);
    double *p2 = REAL(paths2_ptr[k]);
    double prob_val = flow_val * prob_ptr[idx];
    for (int i = 0; i < len1; i++) final_flows_ptr[(int)p1[i] - 1] += prob_val;
    for (int i = 0; i < len2; i++) final_flows_ptr[(int)p2[i] - 1] += prob_val;
  }
  double prob_shortest = flow_val * prob_ptr[n_no_dups];
  for (int i = 0; i < shortest_path_len; i++) {
    final_flows_ptr[(int)shortest_path_ptr[i] - 1] += prob_shortest;
  }

  UNPROTECT(1);
  return prob_ks;
}
