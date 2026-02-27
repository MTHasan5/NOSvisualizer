#' Assign NOS quality grade
#' @export
nos_grade <- function(data, design) {
  
  data$Quality <- dplyr::case_when(
    
    design == "Cross-sectional" & data$Total >= 9 ~ "Very good",
    design == "Cross-sectional" & data$Total >= 7 ~ "Good",
    design == "Cross-sectional" & data$Total >= 5 ~ "Satisfactory",
    design == "Cross-sectional" ~ "Unsatisfactory",
    
    data$Total >= 7 ~ "Good",
    data$Total >= 5 ~ "Fair",
    TRUE ~ "Poor"
  )
  
  data
}