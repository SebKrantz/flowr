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



