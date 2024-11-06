
##############################
### Setup test_bomen_calc
##############################

# In tests/testthat/setup.R
library(testthat)
library(tidyverse)

# First read and process the species info
soort_info <- readRDS(file.path(here::here(), "tests/testthat/soort_info.RDS"))

# Create test data
#---------------------------
set.seed(123)
test_data <- expand.grid(
  Jaar = 2022:2024,
  PlotNr = 1:80,
  BoomNr = 1:20) |>
  mutate(MetingKey = 1:n(),
         BladverliesNetto = rnorm(n(), mean = 25, sd = 10),
         Leeftijdsklasse = as.character(sample(2:7, n(), replace = TRUE)),
         SPEC_DES = NA_character_,
         Soort = NA_character_,
         SoortType = NA_character_,
         SoortIndeling = NA_character_,
         SoortVolgorde = NA_integer_)

# Select some random species from soort_info (excluding summary rows)
valid_species <- soort_info[!is.na(soort_info$SPEC_DES), ]
n_samples <- nrow(test_data)
selected_species <- valid_species[sample(nrow(valid_species),
                                         n_samples,
                                         replace = TRUE), ]

# Fill in species information
test_data$SPEC_DES <- selected_species$SPEC_DES
test_data$Soort <- selected_species$Soort
test_data$SoortType <- selected_species$SoortType
test_data$SoortIndeling <- selected_species$SoortIndeling
test_data$SoortVolgorde <- selected_species$SoortVolgorde

# Calculate Beschadigd based on BladverliesNetto
test_data$Beschadigd <- factor(ifelse(test_data$BladverliesNetto > 25,
                                      "beschadigd",
                                      "onbeschadigd"),
                               levels = c("onbeschadigd", "beschadigd"))

# Save this as your test data
saveRDS(test_data, file.path(here::here(), "tests/testthat/test_data.rds"))

# create reference results
#---------------------------

# Create reference outputs with original function
reference_outputs <- list()

# Case 1: Simple vector group, no group2, no response
reference_outputs$case1 <- bomen_calc_orig(
  test_data |> filter(Jaar == 2024),
  group = c("Jaar"),
  group2 = NULL,
  respons = NULL
)

# Case 2: List group with multiple groupings, with group2 and response
normal_groups <- list(
  c("Jaar"),
  c("Jaar", "SoortType"),
  c("Jaar", "SoortIndeling")
)

reference_outputs$case2 <- bomen_calc_orig(
  test_data,
  group = normal_groups,
  group2 = "SPEC_DES",
  respons = "BladverliesNetto"
)

saveRDS(reference_outputs, "tests/testthat/reference_outputs.RDS")




########################################################################
