#' NOS summary bar plot
#' @export
nos_summary_plot <- function(data, design) {
  
  domains <- if (design == "Case-control") {
    c("Selection","Comparability","Exposure")
  } else {
    c("Selection","Comparability","Outcome")
  }
  
  sum_data <- tidyr::pivot_longer(
    dplyr::select(data, Study, dplyr::all_of(domains)),
    -Study,
    names_to="Domain",
    values_to="Score"
  )
  
  ggplot2::ggplot(sum_data,
                  ggplot2::aes(Study, Score, fill=Domain)) +
    ggplot2::geom_bar(stat="identity") +
    ggplot2::coord_flip() +
    ggplot2::theme_classic()
}