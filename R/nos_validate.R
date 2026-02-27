#' Validate NOS dataset
#' @export
nos_validate <- function(data, design) {
  
  required <- if (design == "Cross-sectional") {
    c("Study", paste0("D",1:7))
  } else {
    c("Study", paste0("D",1:8))
  }
  
  missing <- setdiff(required, names(data))
  
  if (length(missing) > 0) {
    stop("Missing columns: ", paste(missing, collapse = ", "))
  }
  
  data
}