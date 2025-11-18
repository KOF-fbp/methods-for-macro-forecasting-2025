# Latent state extraction utilities for MF-VAR models

extract_latent_states <- function(model, summary = c("mean", "median")) {
  if (missing(model) || is.null(model)) {
    stop("'model' must be a fitted mfbvar object returned by estimate_mfvar_model().")
  }

  summary <- match.arg(summary)
  Z <- model[["Z"]]
  if (is.null(Z)) {
    stop("Supplied model does not contain state draws (slot 'Z').")
  }

  time_names <- dimnames(Z)[[1]]
  var_names <- dimnames(Z)[[2]]
  if (is.null(time_names) || is.null(var_names)) {
    stop("State draws are missing dimension names; cannot construct time series output.")
  }

  collapse_fun <- switch(
    summary,
    mean = function(x) mean(x, na.rm = TRUE),
    median = function(x) stats::median(x, na.rm = TRUE)
  )

  collapsed <- apply(Z, c(1, 2), collapse_fun)
  collapsed <- as.matrix(collapsed)

  states_df <- tibble::tibble(date = as.Date(time_names))
  for (var in var_names) {
    states_df[[var]] <- collapsed[, var]
  }

  states_df
}

save_latent_states_csv <- function(states_df, out_dir, filename = "mfvar_latent_states.csv") {
  if (!"date" %in% names(states_df)) {
    stop("states_df must contain a 'date' column.")
  }
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  path <- file.path(out_dir, filename)
  readr::write_csv(states_df, path)
  path
}

plot_latent_states <- function(states_df, out_dir, states = NULL, mode = c("facet", "heatmap"), filename = NULL) {
  if (!"date" %in% names(states_df)) {
    stop("states_df must contain a 'date' column for plotting.")
  }

  mode <- match.arg(mode)
  state_cols <- setdiff(names(states_df), "date")
  if (is.null(states)) {
    states <- state_cols
  } else {
    missing_states <- setdiff(states, state_cols)
    if (length(missing_states)) {
      stop("Requested states not present: ", paste(missing_states, collapse = ", "))
    }
  }

  states_long <- states_df |>
    dplyr::select(date, tidyselect::all_of(states)) |>
    tidyr::pivot_longer(-date, names_to = "state", values_to = "value")

  if (identical(mode, "facet")) {
    p <- ggplot2::ggplot(states_long, ggplot2::aes(x = date, y = value)) +
      ggplot2::geom_line(color = "#1b9e77") +
      ggplot2::facet_wrap(~ state, scales = "free_y", ncol = 2) +
      ggplot2::labs(
        title = "MF-VAR latent states (posterior mean)",
        x = "Date",
        y = "State value"
      ) +
      ggplot2::theme_minimal(base_size = 12)

    default_name <- "mfvar_latent_states_timeseries.png"
  } else {
    states_heatmap <- states_long |>
      dplyr::group_by(state) |>
      dplyr::mutate(
        value_mean = mean(value, na.rm = TRUE),
        value_sd = stats::sd(value, na.rm = TRUE),
        value = dplyr::if_else(value_sd > 0, (value - value_mean) / value_sd, value - value_mean)
      ) |>
      dplyr::ungroup() |>
      dplyr::select(-value_mean, -value_sd)

    p <- ggplot2::ggplot(states_heatmap, ggplot2::aes(x = date, y = state, fill = value)) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_viridis_c(option = "C") +
      ggplot2::labs(
        title = "MF-VAR latent states (heatmap)",
        x = "Date",
        y = "State",
        fill = "Z-score"
      ) +
      ggplot2::theme_minimal(base_size = 12)

    default_name <- "mfvar_latent_states_heatmap.png"
  }

  if (is.null(filename)) {
    filename <- default_name
  }

  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  plot_path <- file.path(out_dir, filename)
  ggplot2::ggsave(plot_path, plot = p, width = 8, height = if (identical(mode, "facet")) 6 else 4.5, dpi = 150)
  plot_path
}

plot_latent_states_with_actuals <- function(states_df, qdat_orig, out_dir, 
                                             target_variables = c("gdp_growth", "inflation", "exch_rate"),
                                             filename = NULL) {
  if (!"date" %in% names(states_df)) {
    stop("states_df must contain a 'date' column for plotting.")
  }
  
  required_cols <- c("qtr", target_variables)
  missing_cols <- setdiff(required_cols, names(qdat_orig))
  if (length(missing_cols)) {
    stop("qdat_orig missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Convert quarterly data to dates (use quarter end dates for alignment)
  qdat_for_plot <- qdat_orig |>
    dplyr::mutate(date = as.Date(zoo::as.yearqtr(.data$qtr))) |>
    dplyr::select(date, tidyselect::all_of(target_variables))
  
  # Prepare latent states (select only quarterly target variables)
  available_states <- intersect(target_variables, setdiff(names(states_df), "date"))
  if (length(available_states) == 0) {
    stop("No target variables found in latent states. Available states: ", 
         paste(setdiff(names(states_df), "date"), collapse = ", "))
  }
  
  states_for_plot <- states_df |>
    dplyr::mutate(date = as.Date(.data$date)) |>
    dplyr::select(date, tidyselect::all_of(available_states))
  
  # Create long-format data for plotting
  actuals_long <- qdat_for_plot |>
    dplyr::select(date, tidyselect::all_of(available_states)) |>
    tidyr::pivot_longer(-date, names_to = "variable", values_to = "value") |>
    dplyr::mutate(type = "Actual (Quarterly)")
  
  latent_long <- states_for_plot |>
    tidyr::pivot_longer(-date, names_to = "variable", values_to = "value") |>
    dplyr::mutate(type = "Latent State (MF-VAR)")
  
  combined <- dplyr::bind_rows(actuals_long, latent_long)
  
  # Create readable variable labels
  var_labels <- c(
    gdp_growth = "GDP Growth (%)",
    inflation = "Inflation (%)",
    exch_rate = "Exchange Rate (log CHF/EUR)"
  )
  
  combined <- combined |>
    dplyr::mutate(
      variable_label = dplyr::recode(.data$variable, !!!var_labels)
    )
  
  # Create the plot
  p <- ggplot2::ggplot(combined, ggplot2::aes(x = date, y = value, color = type, linetype = type)) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::facet_wrap(~ variable_label, scales = "free_y", ncol = 1) +
    ggplot2::scale_color_manual(
      values = c("Actual (Quarterly)" = "#d95f02", "Latent State (MF-VAR)" = "#1b9e77")
    ) +
    ggplot2::scale_linetype_manual(
      values = c("Actual (Quarterly)" = "solid", "Latent State (MF-VAR)" = "dashed")
    ) +
    ggplot2::labs(
      title = "MF-VAR Latent States vs. Actual Quarterly Variables",
      subtitle = "Comparison of posterior mean latent states with observed data",
      x = "Date",
      y = "Value",
      color = NULL,
      linetype = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = 10),
      panel.grid.minor = ggplot2::element_blank()
    )
  
  if (is.null(filename)) {
    filename <- "mfvar_latent_vs_actual.png"
  }
  
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  
  plot_path <- file.path(out_dir, filename)
  ggplot2::ggsave(plot_path, plot = p, width = 10, height = 8, dpi = 150)
  
  message("Saved latent states vs. actuals plot: ", plot_path)
  plot_path
}
