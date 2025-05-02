library(ggplot2)
library(arrow)
library(tidyr)
library(dplyr)
library(stringr)
library(plotly)
library(readr)

GG_THEME = theme_bw()

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
CONTINENT_CODES = c("970", "950", "983", "905", "931", "932")

P_CODE_SERVICES_TOTAL = "S"

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
product_codes %>% filter((nchar(ProductCode) == 3)) -> second_level_product_codes

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
print(service_codes %>% arrange(., ProductCode), n=300)

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

SHORT_INDICATOR_NAMES = data.frame(IndicatorCode=c(
  CODE_SERVICES_EXPORT,
  CODE_SERVICES_IMPORT,
  CODE_SERVICES_EXPORT_FULL,
  CODE_SERVICES_IMPORT_FULL,
  CODE_MERCHANDISE_EXPORT,
  CODE_MERCHANDISE_IMPORT
), IndicatorShortName=c(
  "Export (services)",
  "Import (services)",
  "Export (services)",
  "Import (services)",
  "Export (merchandise)",
  "Import (merchandise)"
))

# dataset where one side is World, and the other is country
INDICATORS = c(CODE_SERVICES_EXPORT, CODE_SERVICES_IMPORT, CODE_MERCHANDISE_EXPORT, CODE_MERCHANDISE_IMPORT)
should_keep_code = function(df) {
  indicator = df$IndicatorCode %in% INDICATORS
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
  inner_join(top_reporters_total, c("ReporterCode", "Reporter", "IndicatorCode")) %>%
  inner_join(SHORT_INDICATOR_NAMES, by="IndicatorCode")

show_over_time = function() {
  # dataset where one side is World, and the other is country
  INDICATORS = c(CODE_SERVICES_EXPORT, CODE_SERVICES_IMPORT, CODE_MERCHANDISE_EXPORT, CODE_MERCHANDISE_IMPORT)
  should_keep_code = function(df) {
    indicator = df$IndicatorCode %in% INDICATORS
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
    inner_join(top_reporters_total, c("ReporterCode", "Reporter", "IndicatorCode")) %>%
    inner_join(SHORT_INDICATOR_NAMES, by="IndicatorCode")
  
  # thx https://stackoverflow.com/a/41637932
  Reces_table <- read.table(text = "  Start        End
                           1 1980-01-01 1980-07-01
                           2 1981-07-01 1982-11-01
                           3 1990-07-01 1991-03-01
                           4 2001-03-01 2001-11-01
                           5 2007-12-01 2009-06-01
                           6 2020-03-01 2021-12-01", header = TRUE,
                            stringsAsFactors = FALSE)
  Reces_table$Start <- parse_date(Reces_table$Start, format="%Y-%m-%d")
  Reces_table$End <- parse_date(Reces_table$End, format="%Y-%m-%d")
  
  grid_vector = c()
  indicator_codes = 
    for (i in 1:length(INDICATORS)) {
      indicator <- INDICATORS[i]
      reporters_by_year_for_indicator <- reporters_by_year %>% filter(IndicatorCode == indicator)
      reporters_by_year_for_indicator$Year = parse_date(as.character(reporters_by_year_for_indicator$Year), format="%Y") 
      print(reporters_by_year_for_indicator)
      # Create the bar plot

      print(min(reporters_by_year_for_indicator$Year))
      print(max(reporters_by_year_for_indicator$Year))
      print(Reces_table)
      
      p <- ggplot(reporters_by_year_for_indicator) + GG_THEME +
              geom_line(aes(color=Reporter, x=Year, y=sum_value.x)) +
              scale_y_log10() +
              geom_rect(data = Reces_table, inherit.aes = FALSE,
                        aes(xmin=Start, xmax=End, ymin=0, ymax=+Inf), 
                        fill='pink', alpha=0.5) +
              labs(title=reporters_by_year_for_indicator$IndicatorShortName, y="Value (mln $)")
              
      # append plot
      grid_vector <- c(grid_vector, list(p))
    }

  # Create a grid layout
  do.call(gridExtra::grid.arrange, c(grid_vector, ncol = 2))
}
show_over_time()

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

#######

SHORT_PRODUCT_NAMES = data.frame(ProductCode=c(
  "SA",
  "SB",
  "SC",
  "SD",
  "SE",
  "SF",
  "SG",
  "SH",
  "SI",
  "SJ",
  "SK",
  "SL",
  "SN",
  "AG",
  "MA",
  "MI"
), ProductShortName=c(
  "Contract Mfg.",
  "Maintenance & Repair",
  "Transport",
  "Travel",
  "Construction",
  "Insurance & Pension",
  "Financial",
  "IP Use Charges",
  "Telecom & Info",
  "Other Business",
  "Personal & Cultural",
  "Gov. Goods",
  "Unallocated",
  "Agricultural",
  "Manufactured Goods",
  "Fuels & Mining"
))

# top 5 sectors per continent
dataset_continents <- dataset %>% filter(
  (ReporterCode %in% CONTINENT_CODES) & (PartnerCode == RP_CODE_WORLD) &
    (Year == max_year) &
    (ProductCode %in% top_level_product_codes$ProductCode) &
    (IndicatorCode %in% EXPORT)
) %>% group_by(ReporterCode) %>% slice_max(order_by=Value, n = 4) %>% ungroup()

dataset %>% filter(
  (PartnerCode == RP_CODE_WORLD) &
    (Year == max_year) &
    (ProductCode %in% top_level_product_codes$ProductCode) &
    (IndicatorCode == "ITS_MTV_AX")
) %>% select(Reporter) %>% unique

grid_vector = c()
for (i in 1:length(CONTINENT_CODES)) {
  continent_code <- CONTINENT_CODES[i]
  continent_data <- dataset_continents %>%
    filter(ReporterCode == continent_code) %>%
    inner_join(SHORT_PRODUCT_NAMES, by="ProductCode")
  continent_name = continent_data$Reporter
  
  # Create the bar plot
  p <- ggplot(continent_data, aes(x = ProductShortName, y = Value)) +
    geom_bar(stat = "identity") +
    labs(title = continent_name,
         y = "Value (mln $)",x = "") +
    theme(axis.text.x = element_text(angle = 30, hjust=1))
    
  
  # append plot
  grid_vector <- c(grid_vector, list(p))
}

# Create a grid layout
grid_layout <- do.call(gridExtra::grid.arrange, c(grid_vector, ncol = 3))
grid_layout

#######

IP_USE_CHARGES = "SH"
dataset %>% filter(
  str_detect(ProductCode, IP_USE_CHARGES) &
    is_country(dataset$ReporterCode) &
    is_country(dataset$PartnerCode) &
    (nchar(ProductCode) == 3) &
    (Year == max_year) &
    (IndicatorCode %in% EXPORT)
) %>%
  arrange(-Value) %>%
  select(IndicatorCode,Reporter,Partner,ProductCode,Product,Value) %>% print(n=800)

#######

dataset %>% filter(
  (ProductCode == "S") &
  (dataset$Reporter %in% c("Ireland", "Germany")) &
  (dataset$PartnerCode == "840") &
  (IndicatorCode %in% c(CODE_SERVICES_EXPORT_FULL, CODE_SERVICES_IMPORT_FULL))
) -> double_irish

double_irish <- double_irish %>%
  mutate(ImportExport=ifelse(IndicatorCode %in% EXPORT, "Export",
                             ifelse(IndicatorCode %in% IMPORT, "Import", NA)))

double_irish %>% select(ProductCode)

double_irish %>% ggplot() + geom_line(aes(x=Year, y=Value, color=Reporter, linetype=ImportExport)) + labs(title="Ireland Import/Export of Services from/to USA")

######

dataset %>% filter(
  (ProductCode %in% top_level_product_codes$ProductCode) &
    (dataset$Reporter == "Ireland") &
    (dataset$PartnerCode == "840") &
    (IndicatorCode %in% c(CODE_SERVICES_IMPORT_FULL))
) -> double_irish

double_irish <- double_irish %>%
  mutate(ImportExport=ifelse(IndicatorCode %in% EXPORT, "Export",
                             ifelse(IndicatorCode %in% IMPORT, "Import", NA)))

double_irish %>% select(ProductCode)

double_irish %>% ggplot(aes(x=Year, y=Value, fill=Product)) +
  geom_area(position="stack") +
  labs(title="Ireland Import/Export of Services from/to USA")

#######

# Top 10 exporters and their top 5 importers (chord diagram)

# install.packages("circlize")
library(circlize)

top_10_exporters = dataset %>% filter(
  (IndicatorCode == CODE_SERVICES_EXPORT_FULL) &
  (is_country(ReporterCode)) &
  (PartnerCode == RP_CODE_WORLD) &
  (ProductCode == P_CODE_SERVICES_TOTAL) &
  (Year == max_year)
) %>% arrange(., -Value) %>% head(10)

top_10_exporters %>% select(Reporter,Value)

for_chord_diagram <- dataset_only_countries %>%
  filter(
    (IndicatorCode == CODE_SERVICES_EXPORT_FULL) &
    (ReporterCode %in% top_10_exporters$ReporterCode) &
    (ProductCode == P_CODE_SERVICES_TOTAL) &
    (Year == max_year)
  ) %>%
  group_by(Reporter) %>%
  slice_max(order_by = Value, n = 5) %>%
  ungroup %>%
  transmute(from=Reporter, to=Partner, value=Value)

print(for_chord_diagram,n=60)
print(df)

chordDiagram(for_chord_diagram)

