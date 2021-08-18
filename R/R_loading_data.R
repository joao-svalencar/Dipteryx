# reading libraries -------------------------------------------------------
library(here)

# loading data ------------------------------------------------------------
fates <- read.csv(here::here("data", "fates.csv"))
head(fates)
str(fates)

dist <- read.csv(here::here("data", "dist.csv"))
head(dist)
str(dist)