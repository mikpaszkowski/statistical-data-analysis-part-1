library(anytime)
library(tidyverse)

NEW_COLUMN_NAMES <- c("DATE",
                      "AUSTRIA", "BELGIUM",
                      "BULGARIA", "CYPRUS",
                      "CZECH_REPUBLIC", "GERMANY",
                      "DENMARK", "ESTONIA",
                      "SPAIN", "FINLAND",
                      "FRANCE", "UNITED_KINGDOM",
                      "GREECE", "CROATIA", "HUNGARY",
                      "IRELAND", "ITALY",
                      "LITHUANIA", "LUXEMBURG",
                      "LATVIA", "MALTA",
                      "NETHERLANDS", "POLAND",
                      "PORTUGAL", "ROMANIA",
                      "SWEDEN", "SLOVENIA",
                      "SLOVAKIA", "EURO_AREA")


read_unempl_rate <- function(file, countries = NULL) {
  unemployment_data <- read.csv(file)
  unemployment_data <- unemployment_data[, -2]

  colnames(unemployment_data) <- NEW_COLUMN_NAMES

  unemployment_data <- na.omit(unemployment_data)
  unemployment_data$DATE <- as.Date(unemployment_data$DATE, format = "%Y-%m-%d")
  unemployment_data[, -1] <- sapply(unemployment_data[, -1], as.numeric)
  head(unemployment_data)
  # If countries is provided, select only those countries
  if (!is.null(countries)) {
    unemployment_data <- unemployment_data[, c("DATE", countries), drop = FALSE]
  }
  return(unemployment_data)
}