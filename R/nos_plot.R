#' Full NOS plot
#' @export
nos_plot <- function(data, design) {
  
  data <- nos_validate(data, design)
  data <- nos_total_score(data)
  data <- nos_grade(data, design)
  data <- nos_major_domains(data, design)
  
  p1 <- nos_traffic_light(data, design)
  p2 <- nos_summary_plot(data, design)
  
  p1 / p2
}