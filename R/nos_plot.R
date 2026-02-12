utils::globalVariables(c(
  "D1","D2","D3","D4","D5","D6","D7","D8",
  "Study","Domain","Score","Quality",
  "Selection","Comparability","Outcome",
  "Major_Domain"
))

#' Newcastle-Ottawa Scale (NOS) Quality Plot
#'
#' Creates a traffic-light style NOS plot with a summary score bar chart.
#'
#' @param data A data frame with columns Study, D1-D8, and Quality
#' @param save Logical; save the plot to file?
#' @param filename Output filename (PNG)
#' @param width Plot width in inches
#' @param height Plot height in inches
#' @param dpi Resolution in DPI
#'
#' @return A ggplot object
#' @export
#'
#' @examples
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
      Study = factor(Study, levels = rev(Study))
    )

  plot_tl_data <- data |>
    tidyr::pivot_longer(
      dplyr::starts_with("D"),
      names_to = "Domain",
      values_to = "Score"
    ) |>
    dplyr::mutate(
      Study = factor(Study, levels = rev(data$Study))
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
      ggplot2::aes(x = 9, label = Quality),
      size = 3.5,
      label.size = NA
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "0" = "#d7191c",
        "1" = "#a6d96a",
        "2" = "#1a9641"
      ),
      guide = "none"
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
    ggplot2::theme_classic()

  # ---- combine plots (CRAN-safe) ----
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
