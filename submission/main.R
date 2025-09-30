renv::status()
# Example R script to test the Docker setup
library(ggplot2)
library(dplyr)

df <- utils::read.csv("data/kof_data_quarterly.csv")
head(df)

# Create sample data
data <- data.frame(
  x = rnorm(100),
  y = rnorm(100),
  group = sample(c("A", "B", "C"), 100, replace = TRUE)
)

# Basic analysis
cat("Data summary:\n")
print(summary(data))

cat("\nGroup counts:\n")
print(table(data$group))

# Create a plot
p <- ggplot(data, aes(x = x, y = y, color = group)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Sample Scatter Plot", 
       x = "X values", 
       y = "Y values")

# Save plot if output directory exists
if (dir.exists("/app/output")) {
  ggsave("/app/output/sample_plot.png", p, width = 8, height = 6)
  cat("Plot saved to output/sample_plot.png\n")
} else {
  # Just display plot info
  print(p)
  cat("Plot created (output directory not mounted)\n")
}

cat("Script completed successfully!\n")
