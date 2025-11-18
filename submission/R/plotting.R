# Plotting utilities

utils::globalVariables(c(
  "time", "median", "ar2", "value", "midas_trend", "midas_simple",
  "midas_latent_trend", "midas_latent_simple"
))
time <- median <- ar2 <- value <- midas_trend <- midas_simple <- NULL
midas_latent_trend <- midas_latent_simple <- NULL

plot_context_quarters <- getOption("mfvar.context_quarters", 4L)
combined_context_quarters <- getOption("mfvar.combined_context_quarters", 4L)

plot_target_forecasts <- function(fc_df, ar_df, out_dir, title, subtitle, y_label, file_name) {
  stopifnot(nrow(fc_df) > 0)

  plot_df <- fc_df |>
    dplyr::mutate(time = as.Date(time))

  ar_plot <- ar_df |>
    dplyr::mutate(time = as.Date(time)) |>
    dplyr::filter(time <= max(plot_df$time))

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = time)) +
    ggplot2::geom_line(ggplot2::aes(y = median, colour = "MF-VAR"), linewidth = 1.1) +
    ggplot2::geom_point(ggplot2::aes(y = median, colour = "MF-VAR"), size = 2) +
    ggplot2::geom_line(
      data = ar_plot,
      mapping = ggplot2::aes(x = time, y = ar2, colour = "AR(2)"),
      linewidth = 1,
      linetype = "dashed"
    ) +
    ggplot2::geom_point(
      data = ar_plot,
      mapping = ggplot2::aes(x = time, y = ar2, colour = "AR(2)"),
      size = 2,
      inherit.aes = FALSE
    ) +
    ggplot2::scale_x_date(date_breaks = "1 quarter", labels = function(x) format(zoo::as.yearqtr(x), "%Y Q%q")) +
    ggplot2::scale_colour_manual(name = NULL, values = c("MF-VAR" = "#1b9e77", "AR(2)" = "#d95f02")) +
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

  history_df <- history_df |>
    dplyr::filter(is.finite(value))

  if (!nrow(history_df)) {
    stop("History data contains no finite values for plotting.")
  }

  hist_plot <- history_df |>
    dplyr::filter(is.finite(value)) |>
    dplyr::mutate(time = as.Date(time)) |>
    dplyr::arrange(time)

  if (!nrow(hist_plot)) {
    stop("History data contains no finite values for plotting.")
  }

  if (nrow(hist_plot) > plot_context_quarters) {
    hist_plot <- hist_plot |> dplyr::slice_tail(n = plot_context_quarters)
  }

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
      median = last_value
    )

    forecast_df <- dplyr::bind_rows(anchor_row, forecast_df) |>
      dplyr::arrange(time)

    ar_plot <- dplyr::bind_rows(
      tibble::tibble(time = last_actual, ar2 = last_value),
      ar_plot
    ) |>
      dplyr::arrange(time)
  }

  x_values <- c(hist_plot$time, forecast_df$time, ar_plot$time)
  x_values <- x_values[is.finite(x_values)]
  if (!length(x_values)) {
    stop("No valid time values available for plotting.")
  }
  x_min <- min(x_values)
  x_max <- max(x_values)
  q_breaks <- seq(zoo::as.yearqtr(x_min), zoo::as.yearqtr(x_max), by = 0.25)
  if (!length(q_breaks)) {
    q_breaks <- c(zoo::as.yearqtr(x_min), zoo::as.yearqtr(x_max))
  }
  x_breaks <- zoo::as.Date(q_breaks, frac = 1)

  p <- ggplot2::ggplot(hist_plot, ggplot2::aes(x = time, y = value)) +
    ggplot2::geom_line(colour = "#4c4c4c") +
    ggplot2::geom_vline(xintercept = last_actual, linetype = "dotted", colour = "#4c4c4c") +
    ggplot2::geom_line(
      data = forecast_df,
      mapping = ggplot2::aes(x = time, y = median, colour = "MF-VAR"),
      linewidth = 1,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_point(
      data = forecast_df,
      mapping = ggplot2::aes(x = time, y = median, colour = "MF-VAR"),
      size = 2,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_line(
      data = ar_plot,
      mapping = ggplot2::aes(x = time, y = ar2, colour = "AR(2)"),
      linewidth = 1,
      linetype = "dashed",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_point(
      data = ar_plot,
      mapping = ggplot2::aes(x = time, y = ar2, colour = "AR(2)"),
      size = 2,
      inherit.aes = FALSE
    ) +
    ggplot2::scale_x_date(
      breaks = x_breaks,
      labels = function(x) format(zoo::as.yearqtr(x), "%Y Q%q"),
      limits = c(x_min, x_max)
    ) +
    ggplot2::scale_colour_manual(name = NULL, values = c("MF-VAR" = "#1b9e77", "AR(2)" = "#d95f02")) +
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
    subtitle = "Solid line is MF-VAR; dashed line is AR(2)",
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
    subtitle = "Solid line is MF-VAR; dashed line is AR(2)",
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
    subtitle = "Solid line is MF-VAR; dashed line is AR(2)",
    y_label = "CHF per EUR",
    file_name = "forecast_exchange_rate_context.png"
  )
}

plot_combined_forecasts <- function(
    mfvar_df,
    midas_df,
    ar_df,
    history_df,
    out_dir,
    title,
    y_label,
    file_name,
    latent_df = NULL,
    context_quarters = combined_context_quarters) {
  # Plot all models together: MF-VAR, MIDAS (trend & simple), and AR(2)
  stopifnot(nrow(mfvar_df) > 0, nrow(history_df) > 0)

  hist_plot <- history_df |>
    dplyr::filter(is.finite(.data$value)) |>
    dplyr::mutate(time = as.Date(time)) |>
    dplyr::arrange(time)

  if (!is.null(context_quarters) && nrow(hist_plot) > context_quarters) {
    hist_plot <- hist_plot |> dplyr::slice_tail(n = context_quarters)
  }

  mfvar_plot <- mfvar_df |>
    dplyr::mutate(time = as.Date(time))

  midas_plot <- midas_df |>
    dplyr::mutate(time = as.Date(time))

  latent_plot <- NULL
  if (!is.null(latent_df) && nrow(latent_df)) {
    latent_plot <- latent_df |>
      dplyr::mutate(time = as.Date(time))
  }

  ar_plot <- ar_df |>
    dplyr::mutate(time = as.Date(time)) |>
    dplyr::filter(time <= max(mfvar_plot$time, midas_plot$time))

  if (!nrow(hist_plot)) {
    stop("History data contains no finite values for combined plot")
  }
  if (!nrow(mfvar_plot)) {
    stop("MF-VAR forecast frame is empty for combined plot")
  }
  if (!nrow(midas_plot)) {
    stop("MIDAS forecast frame is empty for combined plot")
  }
  if (!nrow(ar_plot)) {
    stop("AR(2) forecast frame is empty for combined plot")
  }

  last_actual <- max(hist_plot$time)
  last_value <- hist_plot$value[match(last_actual, hist_plot$time)]

  # Anchor all forecast series to last observation
  first_mfvar <- min(mfvar_plot$time)
  if (first_mfvar > last_actual) {
    anchor_mfvar <- tibble::tibble(
      time = last_actual,
      median = last_value
    )
    mfvar_plot <- dplyr::bind_rows(anchor_mfvar, mfvar_plot) |>
      dplyr::arrange(time)
  }

  first_midas <- min(midas_plot$time)
  if (first_midas > last_actual) {
    anchor_midas <- tibble::tibble(
      time = last_actual,
      midas_trend = last_value,
      midas_simple = last_value
    )
    midas_plot <- dplyr::bind_rows(anchor_midas, midas_plot) |>
      dplyr::arrange(time)
  }

  if (!is.null(latent_plot) && nrow(latent_plot)) {
    first_latent <- min(latent_plot$time)
    if (first_latent > last_actual) {
      anchor_latent <- tibble::tibble(
        time = last_actual,
        midas_latent_trend = last_value,
        midas_latent_simple = last_value
      )
      latent_plot <- dplyr::bind_rows(anchor_latent, latent_plot) |>
        dplyr::arrange(time)
    }
  }

  if (nrow(ar_plot) > 0 && min(ar_plot$time) > last_actual) {
    ar_plot <- dplyr::bind_rows(
      tibble::tibble(time = last_actual, ar2 = last_value),
      ar_plot
    ) |>
      dplyr::arrange(time)
  }

  x_values <- c(hist_plot$time, mfvar_plot$time, midas_plot$time, ar_plot$time)
  if (!is.null(latent_plot) && nrow(latent_plot)) {
    x_values <- c(x_values, latent_plot$time)
  }
  x_values <- x_values[is.finite(x_values)]
  if (!length(x_values)) {
    stop("No valid time values available for plotting.")
  }
  x_min <- min(x_values)
  x_max <- max(x_values)
  
  q_breaks <- seq(zoo::as.yearqtr(x_min), zoo::as.yearqtr(x_max), by = 0.25)
  if (!length(q_breaks)) {
    q_breaks <- c(zoo::as.yearqtr(x_min), zoo::as.yearqtr(x_max))
  }
  x_breaks <- zoo::as.Date(q_breaks, frac = 1)
  
  colour_values <- c(
    "MF-VAR" = "#1b9e77",
    "MIDAS (trend)" = "#7570b3",
    "MIDAS (simple)" = "#e7298a",
    "MIDAS-Latent (trend)" = "#66a61e",
    "MIDAS-Latent (simple)" = "#a6761d",
    "AR(2)" = "#d95f02"
  )

  p <- ggplot2::ggplot(hist_plot, ggplot2::aes(x = time, y = value)) +
    ggplot2::geom_line(colour = "#4c4c4c") +
    ggplot2::geom_vline(xintercept = last_actual, linetype = "dotted", colour = "#4c4c4c") +
    ggplot2::geom_line(
      data = mfvar_plot,
      mapping = ggplot2::aes(x = time, y = median, colour = "MF-VAR"),
      linewidth = 1,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_point(
      data = mfvar_plot,
      mapping = ggplot2::aes(x = time, y = median, colour = "MF-VAR"),
      size = 2,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_line(
      data = midas_plot,
      mapping = ggplot2::aes(x = time, y = midas_trend, colour = "MIDAS (trend)"),
      linewidth = 1,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_point(
      data = midas_plot,
      mapping = ggplot2::aes(x = time, y = midas_trend, colour = "MIDAS (trend)"),
      size = 1.8,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_line(
      data = midas_plot,
      mapping = ggplot2::aes(x = time, y = midas_simple, colour = "MIDAS (simple)"),
      linewidth = 1,
      linetype = "dashed",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_point(
      data = midas_plot,
      mapping = ggplot2::aes(x = time, y = midas_simple, colour = "MIDAS (simple)"),
      size = 1.8,
      inherit.aes = FALSE
    ) +
    {
      if (!is.null(latent_plot) && nrow(latent_plot)) {
        list(
          ggplot2::geom_line(
            data = latent_plot,
            mapping = ggplot2::aes(x = time, y = midas_latent_trend, colour = "MIDAS-Latent (trend)"),
            linewidth = 1,
            inherit.aes = FALSE
          ),
          ggplot2::geom_point(
            data = latent_plot,
            mapping = ggplot2::aes(x = time, y = midas_latent_trend, colour = "MIDAS-Latent (trend)"),
            size = 1.8,
            inherit.aes = FALSE
          ),
          ggplot2::geom_line(
            data = latent_plot,
            mapping = ggplot2::aes(x = time, y = midas_latent_simple, colour = "MIDAS-Latent (simple)"),
            linewidth = 1,
            linetype = "dashed",
            inherit.aes = FALSE
          ),
          ggplot2::geom_point(
            data = latent_plot,
            mapping = ggplot2::aes(x = time, y = midas_latent_simple, colour = "MIDAS-Latent (simple)"),
            size = 1.8,
            inherit.aes = FALSE
          )
        )
      }
    } +
    ggplot2::geom_line(
      data = ar_plot,
      mapping = ggplot2::aes(x = time, y = ar2, colour = "AR(2)"),
      linewidth = 0.8,
      linetype = "dotted",
      inherit.aes = FALSE
    ) +
    ggplot2::geom_point(
      data = ar_plot,
      mapping = ggplot2::aes(x = time, y = ar2, colour = "AR(2)"),
      size = 1.8,
      inherit.aes = FALSE
    ) +
    ggplot2::scale_x_date(
      limits = c(x_min, x_max),
      breaks = x_breaks,
      labels = function(x) format(zoo::as.yearqtr(x), "%Y Q%q")
    ) +
    ggplot2::scale_colour_manual(name = NULL, values = colour_values) +
    ggplot2::labs(
      title = title,
      subtitle = "All model forecasts with historical data",
      x = "Quarter",
      y = y_label
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "top")

  out_path <- file.path(out_dir, file_name)
  ggplot2::ggsave(out_path, p, width = 10, height = 5, dpi = 120)
  out_path
}
