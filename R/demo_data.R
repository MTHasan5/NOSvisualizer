#' Demo NOS dataset
#'
#' Example dataset for Newcastle-Ottawa Scale visualization.
#'
#' @format A data frame with 8 studies and NOS domains
#' @export
demo_data <- data.frame(
  Study = c(
    "Abdullah (2021)", "Andersen (2009)", "Bedi (2007)",
    "Brenyo (2011)", "Cantillon (2012)", "Darma (2021)",
    "Dogan (2024)", "Efimova (2017)"
  ),
  D1 = c(1,1,1,1,1,1,1,1),
  D2 = c(1,0,1,1,1,1,0,1),
  D3 = c(1,1,1,1,1,1,1,1),
  D4 = c(0,1,0,1,1,1,1,1),
  D5 = c(1,1,1,2,1,2,1,2),
  D6 = c(1,1,1,1,1,1,1,1),
  D7 = c(1,0,1,1,1,1,0,1),
  D8 = c(0,0,0,0,1,1,0,1),
  Quality = c(
    "Fair","Poor","Fair","Good",
    "Good","Good","Poor","Good"
  )
)
