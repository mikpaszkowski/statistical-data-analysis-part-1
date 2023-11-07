# Load necessary packages
library(ggplot2)
library(dplyr)
library(tidyr)
source("./utils/data_reader.R")

# Read the CSV file
data <- read_unempl_rate("./data/unemployment_seasonally_adjusted_15-74_total.csv", "POLAND")

data_long <- data %>% pivot_longer(-c("DATE"), names_to = "Country", values_to="Percentage")

# # Plot the data
p <- ggplot(data_long, aes(x = DATE, y = Percentage, color = Country)) +
  geom_line(size = 0.8) +
  labs(x = "DATE", y = "Unemployment [%]", title = "Change of seasonally adjusted unemployment rate over time") +
  theme_minimal()

p + scale_x_date(date_breaks = "2 year", date_labels = "%Y")