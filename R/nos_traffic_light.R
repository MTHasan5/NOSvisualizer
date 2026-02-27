#' NOS traffic light plot
#' @export
nos_traffic_light <- function(data, design) {
  
  tl <- tidyr::pivot_longer(data,
                            dplyr::starts_with("D"),
                            names_to="Domain",
                            values_to="Score")
  
  ggplot2::ggplot(tl, ggplot2::aes(Domain, Study)) +
    ggplot2::geom_point(ggplot2::aes(fill=factor(Score)),
                        shape=21, size=8, colour="black") +
    ggplot2::geom_text(ggplot2::aes(label=Score), size=3) +
    ggplot2::theme_classic()
}