library(ggplot2)
library(arrow)
library(tidyr)
library(dplyr)

services_annual_dataset = arrow::read_parquet("wto_services_values/services_annual_dataset.parquet")
dplyr::glimpse(services_annual_dataset)
merchandise_values_annual_dataset = arrow::read_parquet("wto_merchandise_values/merchandise_values_annual_dataset.parquet")
dplyr::glimpse(merchandise_values_annual_dataset)

# concat these tables
dataset = bind_rows(services_annual_dataset, merchandise_values_annual_dataset)

# drop useless columns (having only a single value)
dataset %>% select(-one_of(c("Period", "PeriodCode", "FrequencyCode", "Frequency", "Unit", "UnitCode"))) -> dataset

# drop non-country Reporters/Partners:
# - 900 are various aggregates like "Europe" and we can filter them out
# - 000 is "world" and we can drop it
# - letter codes are to drop too.
# - The rest are countries and we keep them.
is_country = function(col) {
  is_000 = col == "000"
  is_9xx = as.numeric(col) >= 900
  is_9xx[is.na(is_9xx)] = TRUE
  is_9xx
  return(!(is_000 | is_9xx))
}

dataset[is_country(dataset$ReporterCode) & is_country(dataset$PartnerCode),] -> dataset_only_countries

CODE_SERVICES_EXPORT = "ITS_CS_AX5"
CODE_SERVICES_IMPORT = "ITS_CS_AM5"
CODE_MERCHANDISE_EXPORT = "ITS_MTV_AX"
CODE_MERCHANDISE_IMPORT = "ITS_MTV_AM"

dataset$IndicatorCode %>% unique

# some basic info about data
dataset_only_countries %>%
  sapply(function(x) n_distinct(x))

glimpse(dataset_only_countries)

#########

# TOP 5 COUNTRIES BY INDICATOR

# dataset where one side is World, and the other is country
should_keep_code = function(df) {
  indicator = df$IndicatorCode %in% c(CODE_SERVICES_EXPORT, CODE_SERVICES_IMPORT, CODE_MERCHANDISE_EXPORT, CODE_MERCHANDISE_IMPORT)
  reporter = df$ReporterCode
  partner = df$PartnerCode
  reporter_is_country = is_country(reporter)
  partner_is_country = is_country(partner)
  return(indicator & ((reporter == "000" & partner_is_country) | (partner == "000" & reporter_is_country)))
}

dataset_world_to_country <- dataset[should_keep_code(dataset), ]

top_reporters_total = dataset_world_to_country %>%
  group_by(ReporterCode,Reporter,IndicatorCode,Indicator) %>%
  summarise(sum_value = sum(Value)) %>%
  group_by(IndicatorCode,Indicator) %>%
  slice_max(order_by = sum_value, n = 5)

reporters_by_year = dataset_world_to_country %>%
  group_by(ReporterCode,Reporter,IndicatorCode,Year) %>%
  summarise(sum_value = sum(Value)) %>%
  inner_join(top_reporters_total, c("ReporterCode", "Reporter", "IndicatorCode"))

ggplot(reporters_by_year) +
  geom_line(aes(color=Reporter, x=Year, y=sum_value.x)) +
  facet_wrap(vars(Indicator)) +
  coord_trans(y="log")


