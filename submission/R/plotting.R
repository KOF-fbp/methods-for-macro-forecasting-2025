# Plotting utilities

utils::globalVariables(c("lower", "upper", "median", "ar2", "value"))
lower <- upper <- median <- ar2 <- value <- NULL

plot_gdp_forecasts <- function(fc_gdp, ar_gdp, out_dir) {
  stopifnot(nrow(fc_gdp) > 0)

  plot_df <- fc_gdp |>
    dplyr::mutate(time = as.Date(time))

  ar_df <- ar_gdp |>
    dplyr::mutate(time = as.Date(time)) |>
    dplyr::filter(time <= max(plot_df$time))

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = time)) +
    ggplot2::geom_ribbon(ggplot2::aes(x = time, ymin = lower, ymax = upper, fill = "MF-VAR"), alpha = 0.25, inherit.aes = FALSE) +
    ggplot2::geom_line(ggplot2::aes(x = time, y = median, colour = "MF-VAR"), linewidth = 1, inherit.aes = FALSE) +
    ggplot2::geom_line(data = ar_df, ggplot2::aes(x = time, y = ar2, colour = "AR(2)"), linewidth = 1, linetype = "dashed", inherit.aes = FALSE) +
    ggplot2::scale_colour_manual(name = NULL, values = c("MF-VAR" = "#1b9e77", "AR(2)" = "#d95f02")) +
    ggplot2::scale_fill_manual(name = NULL, values = c("MF-VAR" = "#1b9e77")) +
    ggplot2::labs(
      title = "GDP growth forecasts",
      subtitle = "Comparison of MF-VAR and AR(2) benchmark",
      x = "Quarter",
      y = "Annualised percentage"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "top")

  out_path <- file.path(out_dir, "forecast_gdp_growth.png")
  ggplot2::ggsave(out_path, p, width = 8, height = 4.5, dpi = 120)
  out_path
}

plot_gdp_forecasts_with_history <- function(fc_gdp, ar_gdp, qdat, out_dir) {
  stopifnot(nrow(fc_gdp) > 0)

  history_df <- tibble::tibble(
    time = zoo::as.Date(qdat$qtr, frac = 1),
    value = qdat$gdp_growth
  ) |>
    dplyr::filter(time >= as.Date("2023-01-01"))

  forecast_df <- fc_gdp |>
    dplyr::mutate(time = as.Date(time))

  ar_df <- ar_gdp |>
    dplyr::mutate(time = as.Date(time)) |>
    dplyr::filter(time <= max(forecast_df$time))

  last_actual <- max(history_df$time)

  p <- ggplot2::ggplot(history_df, ggplot2::aes(x = time, y = value)) +
    ggplot2::geom_line(colour = "#4c4c4c") +
    ggplot2::geom_vline(xintercept = last_actual, linetype = "dotted", colour = "#4c4c4c") +
    ggplot2::geom_ribbon(
      data = forecast_df,
      ggplot2::aes(x = time, ymin = lower, ymax = upper, fill = "MF-VAR"),
      alpha = 0.2,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_line(
      data = forecast_df,
      ggplot2::aes(x = time, y = median, colour = "MF-VAR"),
      linewidth = 1,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_line(
      data = ar_df,
      ggplot2::aes(x = time, y = ar2, colour = "AR(2)"),
      linewidth = 1,
      linetype = "dashed",
      inherit.aes = FALSE
    ) +
    ggplot2::scale_colour_manual(name = NULL, values = c("MF-VAR" = "#1b9e77", "AR(2)" = "#d95f02")) +
    ggplot2::scale_fill_manual(name = NULL, values = c("MF-VAR" = "#1b9e77")) +
    ggplot2::labs(
      title = "GDP growth: history and forecasts",
      subtitle = "Shaded area shows MF-VAR 80% interval; dashed line is AR(2)",
      x = "Quarter",
      y = "Annualised percentage"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "top")

  out_path <- file.path(out_dir, "forecast_gdp_growth_context.png")
  ggplot2::ggsave(out_path, p, width = 8, height = 4.5, dpi = 120)
  out_path
}
