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
