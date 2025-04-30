library(ggplot2)
library(arrow)
library(tidyr)
library(dplyr)
library(stringr)

## IMPORTANT !! BE CAREFUL WITH AGGREGATION:
## - ReporterCode/PartnerCode values overlap (some are already aggregated like "World")
## - ProductCode values overlap (some are already aggregated like "Total services")
## - IndicatorCode values overlap (e.g ITS_CS_AM6 probably overlaps with ITS_CS_AM5)
## Typically you want to use these aggregates instead of doing this manually!!!

## Variables
#  services_annual_dataset & merchandise_values_annual_dataset
#    - raw data from CSV
#  dataset
#    - the concated services_annual_dataset
#      and merchandise_values_annual_dataset + some useless columns dropped
#  dataset_only_countries
#    - dataset filtered so that only countries remain (no aggregated
#      Reporters/Partners)
#  max_year - maximum year in the dataset (2023)
# 

## Functions
#  is_country(code: str[..]) -> bool[..]
#    - returns whether a given Reporter/PartnerCode is a country
#      (not aggregation like World)
#  

## Constants
RP_CODE_WORLD = "000"

CODE_SERVICES_EXPORT = "ITS_CS_AX5"
CODE_SERVICES_IMPORT = "ITS_CS_AM5"
CODE_SERVICES_EXPORT_FULL = "ITS_CS_AX6"
CODE_SERVICES_IMPORT_FULL = "ITS_CS_AM6"
CODE_MERCHANDISE_EXPORT = "ITS_MTV_AX"
CODE_MERCHANDISE_IMPORT = "ITS_MTV_AM"

CODES_SERVICES = c(CODE_SERVICES_EXPORT_FULL, CODE_SERVICES_IMPORT_FULL)
CODES_MERCHANDISE = c(CODE_MERCHANDISE_EXPORT, CODE_MERCHANDISE_IMPORT)

#### PREPROCESSING

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
  is_000 = col == RP_CODE_WORLD
  is_9xx = as.numeric(col) >= 900
  is_9xx[is.na(is_9xx)] = TRUE
  is_9xx
  return(!(is_000 | is_9xx))
}

dataset[is_country(dataset$ReporterCode) & is_country(dataset$PartnerCode),] -> dataset_only_countries

# List of product codes
product_codes = dataset %>% select(ProductCode) %>% unique
print(product_codes, n=1000)

# List of top-level product codes
product_codes %>% filter((nchar(ProductCode) == 2) & (ProductCode != "TO")) -> top_level_product_codes

max_year = max(dataset$Year)

#### USEFUL STUFF

glimpse(dataset_only_countries)

# All Indicator Codes
dataset$IndicatorCode %>% unique

# some basic info about data
dataset_only_countries %>% sapply(function(x) n_distinct(x))

# Service Product codes (letter-only) are hierarchical,
# starting from just "S" (all services), then e.g "SK" is "Personal, cultural, and recreational services"
# which has subcategories (SK1, SK11, ...)
# Let's care only about a single level, let's say the top level (2 letters) is manageable.

# Service Product codes like S123 - IDK, duplicated with the above.
service_codes = dataset %>% select(Product,ProductCode) %>% unique %>% arrange(., Product)
print(service_codes, n=300)

# Merchandise Product codes are encoded like some prefix code
# e.g AG-Agricultural Products -> AGFO-Food
# 2 letter is top level (except "TO" which is total.)
merchandise_codes = dataset %>% filter(IndicatorCode %in% CODES_MERCHANDISE) %>% 
  select(Product,ProductCode) %>% unique %>%
  arrange(., ProductCode)
print(merchandise_codes, n=300)

# Countries & their codes
print(dataset %>% select(ReporterCode,Reporter) %>% arrange(., Reporter) %>% unique, n=1000)

########
########
########

# TOP 5 COUNTRIES BY INDICATOR by time

# dataset where one side is World, and the other is country
should_keep_code = function(df) {
  indicator = df$IndicatorCode %in% c(CODE_SERVICES_EXPORT, CODE_SERVICES_IMPORT, CODE_MERCHANDISE_EXPORT, CODE_MERCHANDISE_IMPORT)
  reporter = df$ReporterCode
  partner = df$PartnerCode
  reporter_is_country = is_country(reporter)
  partner_is_country = is_country(partner)
  return(indicator & ((reporter == RP_CODE_WORLD & partner_is_country) | (partner == RP_CODE_WORLD & reporter_is_country)))
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

#######

# Values by section

countries = c("156", "840", "616") # China, USA, Poland

dataset %>%
  filter((ReporterCode %in% countries) &
           (ProductCode %in% top_level_product_codes$ProductCode) &
           (Year == max_year) & (PartnerCode == RP_CODE_WORLD)) ->
  export_by_sector

export_by_sector

EXPORT=c(CODE_MERCHANDISE_EXPORT,CODE_SERVICES_EXPORT_FULL)
IMPORT=c(CODE_MERCHANDISE_IMPORT,CODE_SERVICES_IMPORT_FULL)
export_by_sector <- export_by_sector %>%
  mutate(ImportExport=ifelse(IndicatorCode %in% EXPORT, "Export",
                      ifelse(IndicatorCode %in% IMPORT, "Import", NA)))

ggplot(export_by_sector) +
  geom_bar(stat="identity", aes(x=str_c(ProductCode," ",Product), y=Value)) +
  facet_grid(ImportExport~Reporter) +
  coord_flip()

#######

# Advertising exports (only USA, only to some countries)
# (just for fun)

PRODUCT_ADVERTISING = "SJ221"

dataset_only_countries %>%
  filter((ProductCode == PRODUCT_ADVERTISING) & (Year == max_year)) %>%
  # THIS lapply FIXES AN ERROR:
  #Error in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y,  : 
  #                     invalid string in PangoCairo_Text
  lapply(., iconv, to = "UTF-8") %>% tibble::as_tibble() %>%
  mutate(Value=as.numeric(Value)) %>%
  ggplot() +
  geom_bar(stat="identity", aes(x=str_c(PartnerCode, " ", Partner),y=Value)) +
  coord_flip()
