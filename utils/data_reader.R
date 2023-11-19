library(anytime)
library(tidyverse)
library(ggplot2)

NEW_COLUMN_NAMES <- c("Date",
                      "austria", "belgium",
                      "bulgaria", "cyprus",
                      "czech_republic", "germany",
                      "denmark", "estonia",
                      "spain", "finland",
                      "france", "united_kingdom",
                      "greece", "croatia", "hungary",
                      "ireland", "italy",
                      "lithuania", "luxemburg",
                      "latvia", "malta",
                      "netherlands", "poland",
                      "portugal", "romania",
                      "sweden", "slovenia",
                      "slovakia", "euro_area")


read_unempl_rate <- function(file, countries = NULL) {
  unemployment_data <- read.csv(file)
  unemployment_data <- unemployment_data[, -2]
  colnames(unemployment_data) <- NEW_COLUMN_NAMES

  unemployment_data$Date <- as.Date(unemployment_data$Date, format = "%Y-%m-%d")
  unemployment_data[, -1] <- sapply(unemployment_data[, -1], as.numeric)
  if (!is.null(countries)) {
    unemployment_data <- unemployment_data[, c("Date", countries), drop = FALSE]
  }
  unemployment_data <- na.omit(unemployment_data)
  return(unemployment_data)
}