#' Build report with forecasts and evaluation
#'
#' @param posterior List from gibbs_mfvar
#' @param pred_obj List from predict_mfvar
#' @param targets_table data.table with target forecasts
#' @param eval_table data.table with evaluation metrics
#' @param out_dir Output directory path
#' @return invisible(TRUE)
#' @export
build_report <- function(posterior, pred_obj, targets_table, eval_table, out_dir) {
  
  # Create output directory if needed
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  
  cli::cli_alert_info("Building report in {out_dir}")
  
  # Save tables
  if (!is.null(targets_table)) {
    targets_file <- file.path(out_dir, "forecasts_targets.csv")
    data.table::fwrite(targets_table, targets_file)
    cli::cli_alert_success("Saved target forecasts to {basename(targets_file)}")
  }
  
  if (!is.null(eval_table)) {
    eval_file <- file.path(out_dir, "evaluation_metrics.csv")
    data.table::fwrite(eval_table, eval_file)
    cli::cli_alert_success("Saved evaluation metrics to {basename(eval_file)}")
  }
  
  # Print summary to console
  cat("\n=== MFVAR Point Forecasts ===\n")
  if (!is.null(targets_table)) {
    print(targets_table)
  }
  
  cat("\n=== Evaluation: RMSE and MAE ===\n")
  if (!is.null(eval_table)) {
    print(eval_table)
  }
  
  # Plot 1: Latent monthly GDP with observed quarterly
  # (simplified version - assumes GDP is first variable)
  
  tryCatch({
    # Extract latent GDP (posterior mean)
    y_latent_mean <- apply(posterior$y_latent_draws, c(1, 2), mean)
    
    # Create plot for first variable (assumed to be GDP)
    gdp_latent <- y_latent_mean[, 1]
    
    plot_data <- data.frame(
      time = 1:length(gdp_latent),
      latent = gdp_latent
    )
    
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = time, y = latent)) +
      ggplot2::geom_line(color = "blue") +
      ggplot2::labs(title = "Latent Monthly GDP",
                    x = "Time", y = "Log Level") +
      ggplot2::theme_minimal()
    
    plot_file <- file.path(out_dir, "latent_gdp.png")
    ggplot2::ggsave(plot_file, p, width = 8, height = 5)
    cli::cli_alert_success("Saved latent GDP plot to {basename(plot_file)}")
    
  }, error = function(e) {
    cli::cli_alert_warning("Could not generate latent GDP plot: {e$message}")
  })
  
  # Plot 2: Fan chart for inflation (optional)
  # Disabled by default as per instructions
  
  invisible(TRUE)
}
