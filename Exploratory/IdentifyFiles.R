library(dplyr)
library(tidyr)
library(ggplot2)

# load the preprocess file
ID <- "ID06"
test <- "04"

filepath <- file.path(
  "Data",
  "IntermediateData",
  paste0(ID, "_", test, "_preprocessed.csv")
)

dat <- read.csv(filepath)

dat |>
  ggplot(aes(time, smo2_dom)) +
  geom_point(color = "blue") +
  geom_point(aes(y = smo2_ndom), color = "red")
