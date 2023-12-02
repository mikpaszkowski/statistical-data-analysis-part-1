library(ggplot2)
library(dplyr)
library(tidyr)
source("./utils/data_reader.R")
source("./utils/utils.R")

alld <- read_unempl_rate("./data/unemployment_seasonally_adjusted_15-74_total.csv", c("poland", "czech_republic", "germany", "sweden", "italy", "spain"))
alld_long_format <- pivot_longer(alld, -"Date", names_to = "Country", values_to="Percentage")
draw_graph(filter_by_time(alld_long_format, c("2010-01-01", "2023-01-01")), "Change of unemployment rate - choosen european countries") + scale_x_date(date_breaks = "1 year", date_labels = "%Y") + scale_y_continuous(breaks = seq(0, 100, by = 2))

data_several_countries <- read_unempl_rate("./data/unemployment_seasonally_adjusted_15-74_total.csv", c("poland", "spain", "france"))
data_several_countries <- data_several_countries %>% pivot_longer(-Date, names_to = "Country", values_to="Percentage")
draw_graph(filter_by_time(data_several_countries, c("2010-01-01", "2023-01-01")), "Unemployment EU countries 1998 - 2023")
draw_graph(filter_by_time(alld_long_format, c("2019-01-01", "2023-01-01")), "Unemployment EU countries in COVID-19 - 2019-2023") + scale_x_date(date_breaks = "1 year", date_labels = "%Y") + scale_y_continuous(breaks = seq(0, 100, by = 2))
draw_graph(filter_by_time(alld_long_format, c("2007-01-01", "2012-01-01")), "Unemployment EU countries during 2008 financial crisis - 2019-2023") + scale_x_date(date_breaks = "1 year", date_labels = "%Y") + scale_y_continuous(breaks = seq(0, 100, by = 2))

# Read and prepare Poland's data
poland_data <- read_unempl_rate("./data/unemployment_seasonally_adjusted_15-74_total.csv", "poland") %>%
  pivot_longer(-Date, names_to = "Country", values_to = "Percentage")

# differential plot of unemployment rate in each country as difference country - Poland
calculate_difference <- function(country_data, poland_data) {
  country_data %>%
    left_join(poland_data, by = "Date", suffix = c("_country", "_poland")) %>%
    mutate(Difference = Percentage_country - Percentage_poland) %>%
    select(Date, Country = Country_country, Difference)
}

# Read and prepare other countries' data
other_countries <- c("czech_republic", "france", "germany", "italy", "sweden", "spain")
other_countries_data <- read_unempl_rate("./data/unemployment_seasonally_adjusted_15-74_total.csv", other_countries) %>%
  pivot_longer(-Date, names_to = "Country", values_to = "Percentage")
split_data <- split(other_countries_data, other_countries_data$Country)

# Calculate differences for each country
differences_data <- lapply(split_data, calculate_difference, poland_data = poland_data) %>%
  bind_rows()
draw_graph_difference(filter_by_time(differences_data, c("2010-01-01", "2023-01-01")), "Difference in Unemployment Rates Compared to Poland 2010-2023") + scale_x_date(date_breaks = "1 year", date_labels = "%Y") + scale_y_continuous(breaks = seq(-10, 100, by = 2))
draw_box_plot_differences(filter_by_time(differences_data, c("2010-01-01", "2023-01-01")), "Difference in Unemployment Rates Compared to Poland 2010-2023") + scale_x_date(date_breaks = "1 year", date_labels = "%Y") + scale_y_continuous(breaks = seq(-10, 100, by = 2))
draw_box_plot(filter_by_time(alld_long_format, c("2007-01-01", "2010-01-01")), "Unemployment EU countries during 2008 financial crisis - 2007-2010") + scale_x_date(date_breaks = "1 year", date_labels = "%Y") + scale_y_continuous(breaks = seq(0, 100, by = 2))
draw_box_plot(filter_by_time(alld_long_format, c("2010-01-01", "2023-01-01")), "Unemployment rate - choosen european countries") + scale_y_continuous(breaks = seq(0, 100, by = 5)) + scale_x_date(date_breaks = "1 year", date_labels = "%Y")

past_decade_range <- read_unempl_rate("./data/unemployment_seasonally_adjusted_15-74_total.csv", c("poland", "czech_republic", "germany", "sweden", "italy", "spain"))
past_decade_range <- past_decade_range %>% pivot_longer(-Date, names_to = "Country", values_to="Percentage")
draw_graph(filter_by_time(past_decade_range, c("1980-01-01", "2009-01-01")), "Unemployment EU countries 1998 - 2010")

draw_box_plot(filter_by_time(alld_long_format,  c("2017-01-01", "2023-01-01")), "Unemployment EU countries 2017 - 2023") + scale_y_continuous(breaks = seq(0, 100, by = 5)) + scale_x_date(date_breaks = "1 year", date_labels = "%Y")
