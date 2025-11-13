# Plotting utilities

utils::globalVariables(c("time", "lower", "upper", "median", "ar2", "value"))
time <- lower <- upper <- median <- ar2 <- value <- NULL

plot_target_forecasts <- function(fc_df, ar_df, out_dir, title, subtitle, y_label, file_name) {
  stopifnot(nrow(fc_df) > 0)

  plot_df <- fc_df |>
    dplyr::mutate(time = as.Date(time))

  ar_plot <- ar_df |>
    dplyr::mutate(time = as.Date(time)) |>
    dplyr::filter(time <= max(plot_df$time))

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = time)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper, fill = "MF-VAR"), alpha = 0.25) +
    ggplot2::geom_line(ggplot2::aes(y = median, colour = "MF-VAR"), linewidth = 1) +
    ggplot2::geom_line(
      data = ar_plot,
      mapping = ggplot2::aes(x = time, y = ar2, colour = "AR(2)"),
      linewidth = 1,
      linetype = "dashed"
    ) +
    ggplot2::scale_x_date(labels = function(x) format(zoo::as.yearqtr(x), "%Y Q%q")) +
    ggplot2::scale_colour_manual(name = NULL, values = c("MF-VAR" = "#1b9e77", "AR(2)" = "#d95f02")) +
    ggplot2::scale_fill_manual(name = NULL, values = c("MF-VAR" = "#1b9e77")) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Quarter",
      y = y_label
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "top")

  out_path <- file.path(out_dir, file_name)
  ggplot2::ggsave(out_path, p, width = 8, height = 4.5, dpi = 120)
  out_path
}

plot_target_forecasts_with_history <- function(fc_df, ar_df, history_df, out_dir, title, subtitle, y_label, file_name) {
  stopifnot(nrow(fc_df) > 0, nrow(history_df) > 0)

  hist_plot <- history_df |>
    dplyr::mutate(time = as.Date(time)) |>
    dplyr::filter(time >= as.Date("2023-01-01"))

  forecast_df <- fc_df |>
    dplyr::mutate(time = as.Date(time))

  ar_plot <- ar_df |>
    dplyr::mutate(time = as.Date(time)) |>
    dplyr::filter(time <= max(forecast_df$time))

  last_actual <- max(hist_plot$time)
  last_value <- hist_plot$value[match(last_actual, hist_plot$time)]
  first_forecast <- min(forecast_df$time)

  # Extend forecasts back to the last observed point so the lines join
  # smoothly with history, but only if there's a gap between the last
  # observation and the first forecast period.
  if (first_forecast > last_actual) {
    anchor_row <- tibble::tibble(
      time = last_actual,
      lower = last_value,
      median = last_value,
      upper = last_value
    )

    forecast_df <- dplyr::bind_rows(anchor_row, forecast_df) |>
      dplyr::arrange(time)

    ar_plot <- dplyr::bind_rows(
      tibble::tibble(time = last_actual, ar2 = last_value),
      ar_plot
    ) |>
      dplyr::arrange(time)
  }

  p <- ggplot2::ggplot(hist_plot, ggplot2::aes(x = time, y = value)) +
    ggplot2::geom_line(colour = "#4c4c4c") +
    ggplot2::geom_vline(xintercept = last_actual, linetype = "dotted", colour = "#4c4c4c") +
    ggplot2::geom_ribbon(
      data = forecast_df,
  mapping = ggplot2::aes(x = time, ymin = lower, ymax = upper, fill = "MF-VAR"),
      alpha = 0.2,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_line(
      data = forecast_df,
  mapping = ggplot2::aes(x = time, y = median, colour = "MF-VAR"),
      linewidth = 1,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_line(
      data = ar_plot,
  mapping = ggplot2::aes(x = time, y = ar2, colour = "AR(2)"),
      linewidth = 1,
      linetype = "dashed",
      inherit.aes = FALSE
    ) +
    ggplot2::scale_x_date(labels = function(x) format(zoo::as.yearqtr(x), "%Y Q%q")) +
    ggplot2::scale_colour_manual(name = NULL, values = c("MF-VAR" = "#1b9e77", "AR(2)" = "#d95f02")) +
    ggplot2::scale_fill_manual(name = NULL, values = c("MF-VAR" = "#1b9e77")) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Quarter",
      y = y_label
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "top")

  out_path <- file.path(out_dir, file_name)
  ggplot2::ggsave(out_path, p, width = 8, height = 4.5, dpi = 120)
  out_path
}

plot_gdp_forecasts_with_history <- function(fc_gdp, ar_gdp, qdat, out_dir) {
  # For GDP growth, history_df already contains growth rates (not levels),
  # so the anchor row will correctly use the last observed growth rate.
  history_df <- tibble::tibble(
    time = zoo::as.Date(qdat$qtr, frac = 1),
    value = qdat$gdp_growth
  )

  plot_target_forecasts_with_history(
    fc_df = fc_gdp,
    ar_df = ar_gdp,
    history_df = history_df,
    out_dir = out_dir,
    title = "GDP growth: history and forecasts",
    subtitle = "Shaded area shows MF-VAR 80% interval; dashed line is AR(2)",
    y_label = "Annualised percentage",
    file_name = "forecast_gdp_growth_context.png"
  )
}

plot_inflation_forecasts_with_history <- function(fc_infl, ar_infl, qdat, out_dir) {
  history_df <- tibble::tibble(
    time = zoo::as.Date(qdat$qtr, frac = 1),
    value = qdat$inflation
  )

  plot_target_forecasts_with_history(
    fc_df = fc_infl,
    ar_df = ar_infl,
    history_df = history_df,
    out_dir = out_dir,
    title = "Inflation: history and forecasts",
    subtitle = "Shaded area shows MF-VAR 80% interval; dashed line is AR(2)",
    y_label = "Annualised percentage",
    file_name = "forecast_inflation_context.png"
  )
}

plot_exch_rate_forecasts_with_history <- function(fc_exch, ar_exch, qdat, out_dir) {
  history_df <- tibble::tibble(
    time = zoo::as.Date(qdat$qtr, frac = 1),
    value = exp(qdat$exch_rate)
  )

  plot_target_forecasts_with_history(
    fc_df = fc_exch,
    ar_df = ar_exch,
    history_df = history_df,
    out_dir = out_dir,
    title = "Exchange rate: history and forecasts",
    subtitle = "Shaded area shows MF-VAR 80% interval; dashed line is AR(2)",
    y_label = "CHF per EUR",
    file_name = "forecast_exchange_rate_context.png"
  )
}
