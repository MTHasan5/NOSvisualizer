#' Calculate total NOS score
#' @export
nos_total_score <- function(data) {
  data$Total <- rowSums(dplyr::select(data, starts_with("D")), na.rm = TRUE)
  data
}