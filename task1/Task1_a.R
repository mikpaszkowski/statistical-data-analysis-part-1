# Load necessary packages
library(ggplot2)
library(dplyr)
library(tidyr)
source("./data_reader.R")
source("./utils.R")
# Read unemployment rate seasonally adjusted
data <- read_unempl_rate("../data/unemployment_seasonally_adjusted_15-74_total.csv", "poland")
colnames(data)[2] <- "Seasonally adjusted"
adjusted_long_format <- data %>% pivot_longer(-"Date", names_to = "Country", values_to="Percentage")

# Read unemployment rate not seasonally adjusted
data2 <- read_unempl_rate("../data/unemployment_not_seasonally_adjusted_15-74_total.csv", "poland")
colnames(data2)[2] <- "Not seasonally adjusted"
no_adjusted_long_format <- pivot_longer(data2, -"Date", names_to = "Country", values_to="Percentage")

plot_data <- rbind(adjusted_long_format, no_adjusted_long_format)
# Plot the data
draw_graph(plot_data, "Change of unemployment rate - Poland") + scale_x_date(date_breaks = "2 year", date_labels = "%Y") + scale_y_continuous(breaks = seq(0, 100, by = 2))
draw_graph(filter_by_time(plot_data, c("2000-01-01", "2006-01-01")), "High fluctuations of unemployment rate - Poland") + scale_y_continuous(breaks = seq(0, 100, by = 1)) + scale_x_date(date_breaks = "1 year", date_labels = "%Y")
draw_graph(filter_by_time(plot_data, c("2005-01-01", "2010-01-01")), "Drop of unemployment rate - Poland") + scale_y_continuous(breaks = seq(0, 100, by = 1)) + scale_x_date(date_breaks = "1 year", date_labels = "%Y")
draw_graph(filter_by_time(plot_data, c("2013-01-01", "2023-01-01")), "Drop of unemployment rate with low fluctuations - Poland") + scale_y_continuous(breaks = seq(0, 100, by = 1)) + scale_x_date(date_breaks = "1 year", date_labels = "%Y")

# Obliczanie średniej rocznej stopy bezrobocia
annual_averages <- plot_data %>%
  mutate(Year = format(Date, "%Y")) %>%
  group_by(Year, Country) %>%
  dplyr::summarise(AnnualAverage = mean(Percentage, na.rm = TRUE)) %>%
  ungroup()
annual_averages
# Obliczanie różnic między średnimi rocznymi
differences <- annual_averages %>%
  pivot_wider(names_from = Country, values_from = AnnualAverage) %>%
  mutate(Difference = `Seasonally adjusted` - `Not seasonally adjusted`) %>%
  select(Year, Difference)
differences
# Rysowanie wykresu słupkowego
draw_histogram(differences, "Annual Difference in Unemployment Rate with and without Seasonal Adjustment")