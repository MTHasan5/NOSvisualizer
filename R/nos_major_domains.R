#' Calculate NOS major domains
#' @export
nos_major_domains <- function(data, design) {
  
  if (design == "Cohort") {
    
    data <- dplyr::mutate(data,
                          Selection = D1+D2+D3+D4,
                          Comparability = D5,
                          Outcome = D6+D7+D8
    )
    
  } else if (design == "Case-control") {
    
    data <- dplyr::mutate(data,
                          Selection = D1+D2+D3+D4,
                          Comparability = D5,
                          Exposure = D6+D7+D8
    )
    
  } else {
    
    data <- dplyr::mutate(data,
                          Selection = D1+D2+D3+D4,
                          Comparability = D5,
                          Outcome = D6+D7
    )
  }
  
  data
}