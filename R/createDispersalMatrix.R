createDispersalMatrix <- function(distMatrix,
                                     decayRate){
  
  # distribute the fraction of fish among the other possible
  # populations:

  # Cooper and Mangel 1999 eq (2):
  stray_mat <- exp(-decayRate * distMatrix)

  # so no fish or lost or gained:
  normalization <- rowSums(stray_mat)
  # to match the desired overall straying rate by stream:
  stray_mat <- stray_mat / normalization
  return(stray_mat)
}