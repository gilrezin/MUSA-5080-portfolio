library(tidyverse)
library(tidycensus)

pa_income <- get_acs(
  geography = "county",
  variables = "B19013_001",
  state = "PA",
  year = 2023,
  survey = "acs5"
)

pa_income <- mutate(pa_income, moe_pct = moe / estimate * 100)

worst <- pa_income %>%
  filter(moe_pct > 8) %>%
  arrange(estimate) %>%
  select(NAME, estimate, moe, moe_pct)

pa_income <- mutate(pa_income, reliable = moe_pct < 5)

# group-by + summarize - sort rows by variable
# in this example, one row for the number of TRUE and the number of FALSE in var 'reliable'
pa_income %>%
  group_by(reliable) %>%
  summarize(n = n(),
      avg_income = mean(estimate))

pa_income <- pa_income %>%
  mutate(reliability = case_when(
    moe_pct < 3 ~ "High Confidence",
    moe_pct < 6 ~ "Moderate",
    TRUE ~ "Low confidence"
  ))

# =================================================
# HW1

state_wide <- get_acs(
  geography = "county",
  variables = c(income = "B19013_001",
                pop = "B01003_001"),
  state = "WA", year = 2023, survey = "acs5", output = "wide"
)

state_wide <- get_acs(
  geography = "tract",
  variables = c(income = "B19013_001",
                pop = "B01003_001"),
  state = "PA", county = "Philadelphia", year = 2023, survey = "acs5", output = "wide"
)

state_wide %>%
  mutate(moe_pct = incomeM / incomeE * 100) %>%
  arrange(desc(moe_pct)) %>%
  select(NAME, popE, incomeE, moe_pct) %>%
  head(10)

