library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(readr)
library(tidyverse)
library(hrbrthemes)
library(viridis)

draw_graph <- function(data_frame, title) {
  p <- ggplot(data_frame, aes(x = Date, y = Percentage, color = Country)) +
    geom_line(size = 1.2) +
    xlab("Date") +
    ylab("Unemployment Rate") +
    ggtitle(title) +
    theme(
      plot.title = element_text(size = 30, hjust = 0.5, face = 'bold', margin = margin(20, 0, 20, 0)),
      axis.text = element_text(size = 16),
      axis.title.x = element_text(size = 20, margin = margin(t = 10, b = 10)), # Increase space around x-axis label
      axis.title.y = element_text(size = 20, margin = margin(r = 10, l = 10)), # Increase space around y-axis label
      legend.position = "right",
      legend.title = element_blank(),
      legend.text = element_text(size = 18),
      legend.key.height = unit(3, "lines"),
    )
  return(p)
}

draw_graph_difference <- function(data_frame, title) {
  p <- ggplot(data_frame, aes(x = Date, y = Difference, color = Country)) +
    geom_line(size = 1.2) +
    xlab("Date") +
    ylab("Difference of unemployment rate (Country_X - Poland)") +
    scale_y_continuous(breaks = seq(-1,1,0.2),limits = c(-1,1),
                       labels = scales::percent)+
    ggtitle(title) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "blue", size = 1.2) +
    theme(
      plot.title = element_text(size = 30, hjust = 0.5, face = 'bold', margin = margin(20, 0, 20, 0)),
      axis.text = element_text(size = 16),
      axis.title.x = element_text(size = 20, margin = margin(t = 10, b = 10)), # Increase space around x-axis label
      axis.title.y = element_text(size = 20, margin = margin(r = 10, l = 10)), # Increase space around y-axis label
      legend.position = "right",
      legend.title = element_blank(),
      legend.text = element_text(size = 18),
      legend.key.height = unit(3, "lines"),
    )
  return(p)
}


draw_box_plot <- function(data_frame, title) {
  p <- ggplot(data_frame, aes(x = Date, y = Percentage, fill = Country)) +
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE, option = "D") +
    ylab("Unemployment rate") +
    xlab("Date") +
    ggtitle(title) +
    theme_minimal() +
    custom_plot_theme()
  return(p)
}

draw_box_plot_differences <- function(data_frame, title) {
  p <- ggplot(data_frame, aes(x = Date, y = Difference, fill = Country)) +
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE, option = "D") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "blue", size = 1.2) +
    ylab("Difference") +
    xlab("Date") +
    ggtitle(title) +
    theme_minimal() +
    custom_plot_theme()
  return(p)
}


draw_histogram <- function(df, title) {
  ggplot(df, aes(x = Year, y = Difference, fill = Difference > 0)) +
    geom_col() +
    scale_fill_manual(values = c("coral1", "cyan3"),
                      labels = c("Higher without adjustment", "Higher with adjustment")) +
    labs(x = "Year", y = "Difference in Annual Average Unemployment Rate",
         title = title,
         fill = "Adjustment Comparison") +
    theme(
      plot.title = element_text(size = 25, hjust = 0.5, vjust = 0.5, face = 'bold', margin = margin(20, 0, 20, 0)),
      axis.title = element_text(size = 20),
      axis.title.x = element_text(margin = margin(20, 0, 20, 0)),
      axis.title.y = element_text(margin = margin(0, 20, 0, 20)),
      axis.text = element_text(
        size = 17,
        face = 3
      ),
      axis.text.x = element_text(angle = 60, hjust = 1),
      legend.position = "right",
      legend.title = element_blank(),
      legend.text = element_text(size = 18),
      legend.key.height = unit(3, "lines"),
    )
}

# Draw histogram
draw_histogram <- function(df, title, y = "AverageUnemployment") {
  ggplot(df, aes(x = Country, y = get(y), fill = Gender)) +
    geom_col(position = position_dodge()) +
    labs(title = title, x = "Country", y = "Average Unemployment Rate") +
    theme(
      plot.title = element_text(size = 25, hjust = 0.5, vjust = 0.5, face = 'bold', margin = margin(20, 0, 20, 0)),
      axis.title = element_text(size = 20),
      axis.title.x = element_text(margin = margin(20, 0, 20, 0)),
      axis.title.y = element_text(margin = margin(0, 20, 0, 20)),
      axis.text = element_text(
        size = 20,
        face = 3
      ),
      axis.text.x = element_text(angle = 60, hjust = 1),
      legend.position = "right",
      legend.title = element_blank(),
      legend.text = element_text(size = 18),
      legend.key.height = unit(3, "lines"),
    )
}

draw_histogram_diff <- function(df, title) {
  ggplot(df, aes(x = Country, y = Difference, fill = Gender)) +
    geom_col(position = position_dodge()) +
    labs(title = title, x = "Country", y = "Average Unemployment Rate") +
    theme(
      plot.title = element_text(size = 25, hjust = 0.5, vjust = 0.5, face = 'bold', margin = margin(20, 0, 20, 0)),
      axis.title = element_text(size = 20),
      axis.title.x = element_text(margin = margin(20, 0, 20, 0)),
      axis.title.y = element_text(margin = margin(0, 20, 0, 20)),
      axis.text = element_text(
        size = 20,
        face = 3
      ),
      axis.text.x = element_text(angle = 60, hjust = 1),
      legend.position = "right",
      legend.title = element_blank(),
      legend.text = element_text(size = 18),
      legend.key.height = unit(3, "lines"),
    )
}

filter_by_time <- function(data_frame, time_period) {
  filter(data_frame, between(Date, as.Date(time_period[1]), as.Date(time_period[2])))
}


custom_plot_theme <- function() {
  return(theme(
    plot.title = element_text(size = 30, hjust = 0.5, face = 'bold', margin = margin(20, 0, 20, 0)),
    axis.title = element_text(size = 20, face = "bold"),
    axis.title.y = element_text(size = 20, face = "bold", margin = margin(0, 20, 0, 20)),
    axis.text = element_text(
      size = 15,
      face = 3
    ),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    legend.title = element_text(size = 25),
    legend.text = element_text(size = 25),
  ))
}

# Filter by time and calculate average unemployment rates
calculate_averages <- function(data, start_date, end_date, months) {
  # Ensure that start_date and end_date are in proper format
  start_date_converted <- as.Date(start_date, format = "%Y-%m-%d")
  end_date_converted <- as.Date(end_date, format = "%Y-%m-%d")

  # Check for successful conversion
  if (is.na(start_date_converted) || is.na(end_date_converted)) {
    stop("start_date or end_date could not be converted to Date format.")
  }

  data %>%
    filter(Date >= start_date_converted & Date <= end_date_converted) %>%
    filter(month(Date) %in% months) %>%
    group_by(Country) %>%
    dplyr::summarise(AverageUnemployment = mean(Percentage, na.rm = TRUE))
}

extract_data_with_period <- function(male_df, female_df, start, end, months) {
  male_averages <- calculate_averages(male_df, start, end, months)
  female_averages <- calculate_averages(female_df, start, end, months)
  # # Add a gender column
  male_averages$Gender <- "Male"
  female_averages$Gender <- "Female"
  # Combine and reshape data
  combined_data <- rbind(male_averages, female_averages)
  return(combined_data)
}

normal_distribution <- function(data, mean_val, sd_val) {
  p <- ggplot(data, aes(x = poland)) +
    geom_histogram(aes(y = ..density..), binwidth = 1, fill = "blue", color = "black") +
    stat_function(fun = dnorm, args = list(mean = mean_val, sd = sd_val), color = "red", size = 1) +
    labs(title = "Normal Distribution of Unemployment Rates in Poland", x = "Unemployment Rate", y = "Density") +
    custom_plot_theme()
}

