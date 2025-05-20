# Load required libraries
library(tidyverse)
library(ggplot2)
library(viridis)
library(ggpubr)
library(lme4)

setwd("/Users/sruthikp/GlobalLianaAnalysis")
# Set up directories
dir.create("plots", showWarnings = FALSE)

# Function to load data
load_data <- function(file_path) {
  data <- readRDS(file_path)
  return(data)
}

#Function to create visualizations for your specific variables
create_visualizations <- function(data) {
  # 1. DBH vs Height by Liana Category
  p1 <- ggplot(data, aes(x = dbh, y = h, color = liana.cat)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = TRUE) +
    theme_minimal() +
    labs(title = "DBH vs Height by Liana Category",
         x = "DBH",
         y = "Height",
         color = "Liana Category")
  
  # 2. Height Distribution by Site
  p2 <- ggplot(data, aes(x = site, y = h)) +
    geom_boxplot(fill = "steelblue", alpha = 0.7) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Height Distribution by Site",
         x = "Site",
         y = "Height")
  
  # 3. DBH Distribution by Site
  p3 <- ggplot(data, aes(x = site, y = dbh)) +
    geom_boxplot(fill = "forestgreen", alpha = 0.7) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "DBH Distribution by Site",
         x = "Site",
         y = "DBH")
  
  # 4. Liana Category Distribution by Site
  p4 <- ggplot(data, aes(x = site, fill = liana.cat)) +
    geom_bar(position = "fill") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Proportion of Liana Categories by Site",
         x = "Site",
         y = "Proportion",
         fill = "Liana Category")
  
  # 5. Liana Category Distribution by DBH
  p5 <- ggplot(data, aes(x = liana.cat, y = dbh)) +
    geom_boxplot(fill = "forestgreen", alpha = 0.7) +
    theme_minimal() +
    labs(title = "DBH Distribution by Liana Category",
         x = "Liana Category",
         y = "DBH")
  
  # 6. Height Distribution by Liana Category
  p6 <- ggplot(data, aes(x = liana.cat, y = h)) +
    geom_boxplot(fill = "steelblue", alpha = 0.7) +
    theme_minimal() +
    labs(title = "Height Distribution by Liana Category",
         x = "Liana Category",
         y = "Height")
  
  # 7. Scatter plot of DBH vs Height with Liana Category as color
  p7 <- ggplot(data, aes(x = dbh, y = h, color = liana.cat)) +
    geom_point(alpha = 0.5) +
    facet_wrap(~site) +
    theme_minimal() +
    labs(title = "DBH vs Height by Liana Category and Site",
         x = "DBH",
         y = "Height",
         color = "Liana Category")
  
  # 8. Heatmap of mean DBH by site and liana category
  dbh_summary <- data %>%
    group_by(site, liana.cat) %>%
    summarise(mean_dbh = mean(dbh, na.rm = TRUE), .groups = 'drop')
  
  p8 <- ggplot(dbh_summary, aes(x = site, y = liana.cat, fill = mean_dbh)) +
    geom_tile() +
    scale_fill_viridis(name = "Mean DBH") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Mean DBH by Site and Liana Category",
         x = "Site",
         y = "Liana Category")
  
  # 9. Heatmap of mean height by site and liana category
  height_summary <- data %>%
    group_by(site, liana.cat) %>%
    summarise(mean_height = mean(h, na.rm = TRUE), .groups = 'drop')
  
  p9 <- ggplot(height_summary, aes(x = site, y = liana.cat, fill = mean_height)) +
    geom_tile() +
    scale_fill_viridis(name = "Mean Height") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Mean Height by Site and Liana Category",
         x = "Site",
         y = "Liana Category")
  
  return(list(p1, p2, p3, p4, p5, p6, p7, p8, p9))
}

# Update the save_plots function
save_plots <- function(plots) {
  plot_names <- c(
    "dbh_height_by_liana.png",
    "height_by_site.png",
    "dbh_by_site.png",
    "liana_proportion_by_site.png",
    "dbh_by_liana.png",
    "height_by_liana.png",
    "dbh_height_by_liana_site.png",
    "dbh_heatmap.png",
    "height_heatmap.png"
  )
  
  for (i in seq_along(plots)) {
    ggsave(
      file.path("plots", plot_names[i]),
      plots[[i]],
      width = 12,
      height = 8
    )
  }
}

# Update the main function
main <- function() {
  # Load data
  data <- load_data("data/All.COI.data.RDS")
  
  # Create visualizations
  plots <- create_visualizations(data)
  
  # Save plots
  save_plots(plots)
  
  # Print summary statistics
  print("Summary statistics by liana category:")
  print(summary(data$dbh ~ data$liana.cat))
  print(summary(data$h ~ data$liana.cat))
  
  # Print summary statistics by site
  print("\nSummary statistics by site:")
  print(summary(data$dbh ~ data$site))
  print(summary(data$h ~ data$site))
}

main()
