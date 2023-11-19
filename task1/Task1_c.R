library(ggplot2)
library(dplyr)
library(tidyr)
source("./utils.R")
source("./data_reader.R")

male_data_poland <- read_unempl_rate("../data/unemployment_not_seasonally_adjusted_15-74_male.csv", "poland")

colnames(male_data_poland)[2] <- "Male"
male_data_long_format <- pivot_longer(male_data_poland, -"Date", names_to = "Country", values_to = "Percentage")

female_data_poland <- read_unempl_rate("../data/unemployment_not_seasonally_adjusted_15-74_female.csv", "poland")
colnames(female_data_poland)[2] <- "Female"
female_data_several_countries <- female_data_poland %>% pivot_longer(-Date, names_to = "Country", values_to = "Percentage")

compare_plot <- rbind(male_data_long_format, female_data_several_countries)
draw_graph(filter_by_time(compare_plot, c("1997-09-01", "2023-04-01")), "Unemployment Poland - female and male - all months") +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(0, 100, by = 2))

summer_male_data_long <- male_data_long_format %>%
  filter(month(Date) %in% c(6, 7, 8))
summer_female <- female_data_several_countries %>%
  filter(month(Date) %in% c(6, 7, 8))
compare_plot <- rbind(summer_male_data_long, summer_female)
draw_graph(filter_by_time(compare_plot, c("2000-09-01", "2023-04-01")), "Unemployment Poland - female and male - winter months")


# histograms to compare female/male uneployment rate
male_data <- read_unempl_rate("../data/unemployment_not_seasonally_adjusted_15-74_male.csv", c("poland", "czech_republic", "france", "denmark", "italy", "sweden", "spain"))
male_data_formatted <- pivot_longer(male_data, -"Date", names_to = "Country", values_to = "Percentage")
female_data <- read_unempl_rate("../data/unemployment_not_seasonally_adjusted_15-74_female.csv", c("poland", "czech_republic", "france", "denmark", "italy", "sweden", "spain"))
female_data_formatted <- pivot_longer(female_data, -"Date", names_to = "Country", values_to = "Percentage")

summer_months <- c(6, 7, 8)
winter_months <- c(12, 1, 2)
draw_histogram(extract_data_with_period(male_data_formatted, female_data_formatted, "2001-01-01", "2004-12-31", summer_months), "Average Unemployment Rate by Gender in 2001-2004 - summer months")
draw_histogram(extract_data_with_period(male_data_formatted, female_data_formatted, "2001-01-01", "2004-12-31", winter_months), "Average Unemployment Rate by Gender in 2001-2004 - winter months")
draw_histogram(extract_data_with_period(male_data_formatted, female_data_formatted, "2010-01-01", "2014-12-31", summer_months), "Average Unemployment Rate by Gender in 2010-2014 - summer months")
draw_histogram(extract_data_with_period(male_data_formatted, female_data_formatted, "2010-01-01", "2014-12-31", winter_months), "Average Unemployment Rate by Gender in 2010-2014 - winter months")

# Assuming data_frame is your dataset and 'unemployment_rate' is the column of interest
data_frame <- read_unempl_rate("../data/unemployment_not_seasonally_adjusted_15-74_male.csv", "poland")
poland_data <- data_frame %>%
  filter(Date >= as.Date("2010-01-01"), Date <= as.Date("2020-01-01"))

mean_val <- mean(poland_data$poland, na.rm = TRUE)
sd_val <- sd(poland_data$poland, na.rm = TRUE)

ggplot(poland_data, aes(x = poland)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "cyan3", color = "black") +
  stat_function(fun = dnorm, args = list(mean = mean_val, sd = sd_val), color = "red", size = 2) +
  labs(title = "Normal Distribution of Unemployment Rates in Poland", x = "Unemployment Rate", y = "Density")