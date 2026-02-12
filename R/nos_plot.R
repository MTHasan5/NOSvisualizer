# Bind global variables to avoid R CMD check notes
Study <- Domain <- Score <- Quality <- NULL
D1 <- D2 <- D3 <- D4 <- D5 <- D6 <- D7 <- D8 <- NULL
Selection <- Comparability <- Outcome <- Major_Domain <- NULL

#' Newcastle-Ottawa Scale (NOS) Quality Plot
#'
#' Creates a Cochrane-style traffic-light plot for the 8 NOS domains
#' along with a stacked bar chart summarizing the three major domains
#' (Selection, Comparability, Outcome).
#'
#' @param data A data frame containing:
#'   \itemize{
#'     \item Study: Study name
#'     \item D1-D8: Domain scores (0–2)
#'     \item Quality: Overall quality label
#'   }
#' @param save Logical; whether to save the plot.
#' @param filename Output filename (PNG).
#' @param width Width in inches.
#' @param height Height in inches.
#' @param dpi Resolution in DPI.
#'
#' @details
#' Domain scoring:
#' \itemize{
#'   \item Selection = D1 + D2 + D3 + D4
#'   \item Comparability = D5
#'   \item Outcome = D6 + D7 + D8
#' }
#'
#' @return A patchwork ggplot object.
#' @export
#'
#' @examples
#' demo_data <- data.frame(
#'   Study = c("Study A", "Study B"),
#'   D1 = c(1,1), D2 = c(1,0), D3 = c(1,1), D4 = c(1,1),
#'   D5 = c(2,1),
#'   D6 = c(1,1), D7 = c(1,0), D8 = c(1,1),
#'   Quality = c("High", "Moderate")
#' )
#' nos_plot(demo_data)

nos_plot <- function(
    data,
    save = FALSE,
    filename = "NOS_quality.png",
    width = 12,
    height = 16,
    dpi = 300
) {

  # ---- input validation ----
  required_cols <- c("Study", paste0("D", 1:8), "Quality")
  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # ---- process data ----
  nos_processed <- data |>
    dplyr::mutate(
      Selection = D1 + D2 + D3 + D4,
      Comparability = D5,
      Outcome = D6 + D7 + D8,
      Study = factor(Study, levels = rev(unique(Study)))
    )

  plot_tl_data <- data |>
    tidyr::pivot_longer(
      dplyr::starts_with("D"),
      names_to = "Domain",
      values_to = "Score"
    ) |>
    dplyr::mutate(
      Study = factor(Study, levels = rev(unique(data$Study)))
    )

  # ---- traffic-light plot ----
  p1 <- ggplot2::ggplot(
    plot_tl_data,
    ggplot2::aes(x = Domain, y = Study)
  ) +
    ggplot2::geom_point(
      ggplot2::aes(fill = factor(Score)),
      size = 9, shape = 21, color = "black"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = Score),
      size = 3, fontface = "bold"
    ) +
    ggplot2::geom_label(
      data = nos_processed,
      ggplot2::aes(x = "Quality", y = Study, label = Quality),
      size = 3.5,
      label.size = NA
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "0" = "#d7191c",
        "1" = "#a6d96a",
        "2" = "#1a9641"
      ),
      name = "Score"
    ) +
    ggplot2::scale_x_discrete(
      limits = c(paste0("D", 1:8), "Quality")
    ) +
    ggplot2::labs(
      title = "Newcastle-Ottawa Scale Quality Assessment",
      x = "",
      y = ""
    ) +
    ggplot2::theme_classic()

  # ---- summary bar plot ----
  plot_sum_data <- nos_processed |>
    dplyr::select(
      Study, Selection, Comparability, Outcome
    ) |>
    tidyr::pivot_longer(
      -Study,
      names_to = "Major_Domain",
      values_to = "Score"
    )

  p2 <- ggplot2::ggplot(
    plot_sum_data,
    ggplot2::aes(
      x = Study,
      y = Score,
      fill = Major_Domain
    )
  ) +
    ggplot2::geom_bar(
      stat = "identity",
      width = 0.6
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = "",
      y = "Score",
      fill = "Domain"
    ) +
    ggplot2::theme_classic()

  # ---- combine plots ----
  final_plot <- patchwork::wrap_plots(
    p1, p2,
    ncol = 1,
    heights = c(1.5, 1)
  )

  # ---- save if requested ----
  if (isTRUE(save)) {
    ggplot2::ggsave(
      filename,
      plot = final_plot,
      width = width,
      height = height,
      dpi = dpi
    )
  }

  return(final_plot)
}
