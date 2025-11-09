#include <R.h>
#include <Rinternals.h>

#undef SEXPPTR_RO
#define SEXPPTR_RO(x) ((const SEXP *)DATAPTR_RO(x))  // to avoid overhead of looped VECTOR_ELT


/**
 * Check if paths have duplicated edges
 *
 * @param paths1 List of integer vectors (edge numbers for first part of paths)
 * @param paths2 List of integer vectors (edge numbers for second part of paths)
 * @param delta_ks Integer vector used as hash table (must be large enough to index all edge numbers)
 * @return Logical vector of length length(paths1) with TRUE for no duplicates, FALSE if duplicates exist
 */
SEXP check_path_duplicates(SEXP paths1, SEXP paths2, SEXP delta_ks) {

  int n_paths = length(paths2);
  if (length(paths1) < n_paths) {
    error("paths1 must be at least as long as paths2");
  }

  // Get pointer to delta_ks for direct indexing
  int *delta_ptr = INTEGER(delta_ks);

  // Allocate buffer for results
  int *buf = (int *) R_alloc(n_paths, sizeof(int)), j = 0;

  // Iterate over each path
  const SEXP *paths1_ptr = SEXPPTR_RO(paths1);
  const SEXP *paths2_ptr = SEXPPTR_RO(paths2);

  // Check inputs
  for (int k = 0; k < n_paths; k++) {

    int len1 = length(paths1_ptr[k]);
    int len2 = length(paths2_ptr[k]);
    double *path1_ptr = REAL(paths1_ptr[k]);
    double *path2_ptr = REAL(paths2_ptr[k]);

    // First pass: check for duplicates
    int has_duplicate = 0;

    // Check edges in path1
    for (int i = 0; i < len1; i++) delta_ptr[(int)path1_ptr[i] - 1] = 1; // Mark edge as seen

    // check path2 for duplicates with path1
    for (int i = 0; i < len2; i++) {
        // Convert to 0-based for C array indexing
        int edge = (int)path2_ptr[i] - 1;
        if (delta_ptr[edge] > 0) {
            has_duplicate = 1;
            break; // Found duplicate
        }
        delta_ptr[edge] = 1; // Mark edge as seen
    }

    // Second pass: clear the hash table
    for (int i = 0; i < len1; i++) delta_ptr[(int)path1_ptr[i] - 1] = 0;
    for (int i = 0; i < len2; i++) delta_ptr[(int)path2_ptr[i] - 1] = 0;

    // Set result: TRUE if no duplicates, FALSE if duplicates
    if(!has_duplicate) buf[j++] = k+1;
  }

  SEXP result = PROTECT(allocVector(INTSXP, j));
  if(j) memcpy(INTEGER(result), buf, sizeof(int) * j);
  UNPROTECT(1);
  return result;
}


/**
 * Mark edges as traversed by incrementing counts in edges_traversed
 *
 * @param paths List of numeric/integer vectors (edge numbers for paths)
 * @param edges_traversed Integer vector to be modified in place (must be large enough to index all edge numbers)
 * @return The modified edges_traversed vector
 */
SEXP mark_edges_traversed(SEXP paths, SEXP edges_traversed) {

  int n_paths = length(paths);

  // Get pointer to edges_traversed for direct indexing
  int *edges_ptr = INTEGER(edges_traversed);

  // Iterate over each path
  const SEXP *paths_ptr = SEXPPTR_RO(paths);

  for (int k = 0; k < n_paths; k++) {
    int path_len = length(paths_ptr[k]);
    if (path_len == 0) continue; // Skip empty paths

    double *path_ptr = REAL(paths_ptr[k]);

    // Increment count for each edge in the path
    for (int i = 0; i < path_len; i++) edges_ptr[(int)path_ptr[i] - 1]++;
  }

  return edges_traversed;
}





/**
 * Set all delta_ks values for visited edges to zero
 *
 * This function resets to zero the values in the integer vector delta_ks corresponding to
 * all edges traversed by the given set of paths. For each index in no_dups, retrieves the
 * corresponding path from paths1 and paths2, and for each edge in these paths, sets its delta_ks
 * entry to zero. Finally, for each edge in shortest_path, the delta_ks entry is also set to zero.
 *
 * Intended for use after edge count tallies (delta_ks) are no longer needed for those paths.
 *
 * @param delta_ks    Integer vector to be zeroed in-place for traversed edges
 * @param no_dups     Integer vector of indices (1-based) of non-duplicate paths
 * @param paths1      List of vectors; primary part of alternative paths (double vectors of edge IDs)
 * @param paths2      List of vectors; secondary part of alternative paths (double vectors of edge IDs)
 * @param shortest_path Numeric vector of edge IDs for the shortest path
 * @return            The modified delta_ks vector (as SEXP)
 */
SEXP free_delta_ks(SEXP delta_ks, SEXP no_dups, SEXP paths1, SEXP paths2, SEXP shortest_path) {
  int n_no_dups = length(no_dups);
  const SEXP *paths1_ptr = SEXPPTR_RO(paths1);
  const SEXP *paths2_ptr = SEXPPTR_RO(paths2);
  int *no_dups_ptr = INTEGER(no_dups);
  double *shortest_path_ptr = REAL(shortest_path);
  int shortest_path_len = length(shortest_path);
  int *delta_ptr = INTEGER(delta_ks);

  // Zero out delta_ks entries for all edges in paths1 and paths2 for non-duplicate paths
  for (int idx = 0; idx < n_no_dups; idx++) {
    int k = no_dups_ptr[idx] - 1;
    int len1 = length(paths1_ptr[k]);
    int len2 = length(paths2_ptr[k]);
    double *p1 = REAL(paths1_ptr[k]);
    double *p2 = REAL(paths2_ptr[k]);
    for (int i = 0; i < len1; i++) delta_ptr[(int)p1[i]] = 0;
    for (int i = 0; i < len2; i++) delta_ptr[(int)p2[i]] = 0;
  }

  // Zero out delta_ks entries for all edges in the shortest path
  for (int i = 0; i < shortest_path_len; i++) delta_ptr[(int)shortest_path_ptr[i]] = 0;

  return delta_ks;
}
