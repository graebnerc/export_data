rm(list = ls())
library(haven)
library(data.table)
library(Hmisc)
library(here)

# The Growth Lab at Harvard University, 2019, 
# "country_sitcproduct4digit_year.dta", 
# International Trade Data (SITC, Rev. 2), 
# https://doi.org/10.7910/DVN/H8SFD2/M5I8JR, 
# Harvard Dataverse, V4 

sitc4 <- read_dta(here::here("data/raw/pure_exports/country_sitcproduct4digit_year.dta"))
setDT(sitc4)
head(sitc4)

# For SITC codes see: 
# https://unstats.un.org/unsd/tradekb/Knowledgebase/50262/Search-SITC-code-description

# Oil shares of total exports----------------------------------
oil_codes <- c("33", "34") 
# 33	Petroleum, petroleum products and related materials 
# 34	Gas, natural and manufactured

# Coal and metal share of total exports------------------------
coal_and_metal_codes_2 <- c("32", "35", "28", "68", "97")
# 32	Coal, coke and briquettes
# 35	Electric current
# 28	Metalliferous ores and metal scrap
# 68	Non-ferrous metals
# 97	Gold, non-monetary (excluding gold ores and concentrates)

coal_and_metal_codes_4 <- c("5224", "5231", "5232", "5233")
# 5224	Metallic oxides of zinc, iron, lead, chromium etc
# 5231	Metallic salts and peroxysalts of inorganic acids
# 5232	Metallic salts and peroxysalts of inorganic acids
# 5233	Salts of metallic acids; compounds of precious metals

# Primary goods------------------------------------------------
primary_goods_codes_1 <- c("0", "1", "2", "4")
primary_goods_codes_2 <- c(primary_goods_codes_1, "3")
# In jedem Fall:
# 0	Food and live animals chiefly for food
# 2	Crude materials, inedible, except fuels
# 1	Beverages and tobacco
# 4	Animal and vegetable oils, fats and waxes
# Unklar:
# 3	Mineral fuels, lubricants and related materials

# Standard data----------------------------------------------------------------
sitc4_processed <- copy(sitc4)
sitc4_processed <- sitc4_processed[
  , .(location_code, sitc_product_code, year, export_value)]

sitc4_processed[, world_trade:=sum(export_value), year]
sitc4_processed[, country_trade:=sum(export_value), .(year, location_code)]
sitc4_processed[, country_share:=country_trade/world_trade]
sitc4_processed[, sitc_product_code2:=substr(sitc_product_code, 1, 2) ]
sitc4_processed[, sitc_product_code1:=substr(sitc_product_code, 1, 1) ]
head(sitc4_processed)


sitc4_processed[sitc_product_code2 %in% oil_codes,
                oil_exports:=sum(export_value), .(year, location_code)]

sitc4_processed[
  sitc_product_code2 %in% coal_and_metal_codes_2 | sitc_product_code %in% coal_and_metal_codes_4,
  coal_and_metal_exports:=sum(export_value), .(year, location_code)]

sitc4_processed[sitc_product_code1 %in% primary_goods_codes_1,
                primary_exports_1:=sum(export_value), .(year, location_code)]

sitc4_processed[sitc_product_code1 %in% primary_goods_codes_2,
                primary_exports_2:=sum(export_value), .(year, location_code)]

sitc4_processed[, primary_exports_1_share_world:=primary_exports_1/world_trade]
sitc4_processed[, primary_exports_2_share_world:=primary_exports_2/world_trade]

sitc4_processed[, primary_exports_1_share_country:=primary_exports_1/country_trade]
sitc4_processed[, primary_exports_2_share_country:=primary_exports_2/country_trade]


sitc4_processed[, oil_exports_share_world:=oil_exports/world_trade]
sitc4_processed[, coal_and_metal_exports_share_world:=coal_and_metal_exports/world_trade]

sitc4_processed[, oil_exports_share_country:=oil_exports/country_trade]
sitc4_processed[, coal_and_metal_exports_share_country:=coal_and_metal_exports/country_trade]

set(sitc4_processed, , c("sitc_product_code", "sitc_product_code1", 
                         "sitc_product_code2", "export_value"), NULL)
sitc4_processed <- unique(sitc4_processed)
sitc4_processed

sitc4_processed <- melt(
  sitc4_processed, id.vars = c("year", "location_code",  "world_trade",
                               "country_trade", "country_share")
)
sitc4_processed <- unique(sitc4_processed[!is.na(value)])


sitc4_processed <- dcast(sitc4_processed, year + location_code + world_trade +
                           country_trade + country_share ~ variable,
                         value.var = "value")
var.labels <- c(
  year="Year of obs",
  location_code="Exporting country (iso3c)",
  world_trade="Total world exports in current USD",
  country_trade="Total exports of country in current USD",
  country_share="Share of country exports in total world exports",
  oil_exports="Total oil-related goods by country in current USD",
  coal_and_metal_exports="Total coal-and-metal-related goods by country in current USD",
  primary_exports_1="Total exports of primary goods (excluding Mineral fuels, lubricants and related materials) by country in current USD",
  primary_exports_2="Total exports of primary goods (including Mineral fuels, lubricants and related materials) by country in current USD",
  primary_exports_1_share_world="Export share of country w.r.t. primary_exports_1",
  primary_exports_2_share_world="Export share of country w.r.t. primary_exports_2",
  primary_exports_1_share_country="Share of primary goods (excluding Mineral fuels, lubricants and related materials) in total exports of country",
  primary_exports_2_share_country="Share of primary goods (including Mineral fuels, lubricants and related materials) in total exports of country",
  oil_exports_share_world="Export share of country w.r.t. oil_exports",
  coal_and_metal_exports_share_world="Export share of country w.r.t. coal_and_metal_exports",
  oil_exports_share_country="Share of oil-related goods in total exports of country",
  coal_and_metal_exports_share_country="Share of coal-and-metal-related goods in total exports of country"
)

label(sitc4_processed) = as.list(
  var.labels[match(names(sitc4_processed), names(var.labels))])
saveRDS(object = sitc4_processed,
        file = here("data/tidy/sitc_primary_exports.rds"))

# Alternative data-------------------------------------------------------------

# Alternative computation without the following HS product classes:
removed_codes <- c(
  "unspecified", "travel", "transport", "ict", "financial"
)

sitc4_processed_red <- copy(sitc4)
sitc4_processed_red <- sitc4_processed_red[
  , .(location_code, sitc_product_code, year, export_value)]
sitc4_processed_red <- sitc4_processed_red[!sitc_product_code %in% removed_codes]

sitc4_processed_red[, world_trade:=sum(export_value), year]
sitc4_processed_red[, country_trade:=sum(export_value), .(year, location_code)]
sitc4_processed_red[, country_share:=country_trade/world_trade]
sitc4_processed_red[, sitc_product_code2:=substr(sitc_product_code, 1, 2) ]
sitc4_processed_red[, sitc_product_code1:=substr(sitc_product_code, 1, 1) ]
head(sitc4_processed_red)


sitc4_processed_red[sitc_product_code2 %in% oil_codes,
                oil_exports:=sum(export_value), .(year, location_code)]

sitc4_processed_red[
  sitc_product_code2 %in% coal_and_metal_codes_2 | sitc_product_code %in% coal_and_metal_codes_4,
  coal_and_metal_exports:=sum(export_value), .(year, location_code)]

sitc4_processed_red[sitc_product_code1 %in% primary_goods_codes_1,
                primary_exports_1:=sum(export_value), .(year, location_code)]

sitc4_processed_red[sitc_product_code1 %in% primary_goods_codes_2,
                primary_exports_2:=sum(export_value), .(year, location_code)]

sitc4_processed_red[, primary_exports_1_share_world:=primary_exports_1/world_trade]
sitc4_processed_red[, primary_exports_2_share_world:=primary_exports_2/world_trade]

sitc4_processed_red[, primary_exports_1_share_country:=primary_exports_1/country_trade]
sitc4_processed_red[, primary_exports_2_share_country:=primary_exports_2/country_trade]


sitc4_processed_red[, oil_exports_share_world:=oil_exports/world_trade]
sitc4_processed_red[, coal_and_metal_exports_share_world:=coal_and_metal_exports/world_trade]

sitc4_processed_red[, oil_exports_share_country:=oil_exports/country_trade]
sitc4_processed_red[, coal_and_metal_exports_share_country:=coal_and_metal_exports/country_trade]

set(sitc4_processed_red, , c("sitc_product_code", "sitc_product_code1", 
                         "sitc_product_code2", "export_value"), NULL)
sitc4_processed_red <- unique(sitc4_processed_red)
sitc4_processed_red

sitc4_processed_red <- melt(
  sitc4_processed_red, id.vars = c("year", "location_code",  "world_trade",
                               "country_trade", "country_share")
)
sitc4_processed_red <- unique(sitc4_processed_red[!is.na(value)])


sitc4_processed_red <- dcast(sitc4_processed_red, year + location_code + world_trade +
                           country_trade + country_share ~ variable,
                         value.var = "value")
var.labels <- c(
  year="Year of obs",
  location_code="Exporting country (iso3c)",
  world_trade="Total world exports in current USD",
  country_trade="Total exports of country in current USD",
  country_share="Share of country exports in total world exports",
  oil_exports="Total oil-related goods by country in current USD",
  coal_and_metal_exports="Total coal-and-metal-related goods by country in current USD",
  primary_exports_1="Total exports of primary goods (excluding Mineral fuels, lubricants and related materials) by country in current USD",
  primary_exports_2="Total exports of primary goods (including Mineral fuels, lubricants and related materials) by country in current USD",
  primary_exports_1_share_world="Export share of country w.r.t. primary_exports_1",
  primary_exports_2_share_world="Export share of country w.r.t. primary_exports_2",
  primary_exports_1_share_country="Share of primary goods (excluding Mineral fuels, lubricants and related materials) in total exports of country",
  primary_exports_2_share_country="Share of primary goods (including Mineral fuels, lubricants and related materials) in total exports of country",
  oil_exports_share_world="Export share of country w.r.t. oil_exports",
  coal_and_metal_exports_share_world="Export share of country w.r.t. coal_and_metal_exports",
  oil_exports_share_country="Share of oil-related goods in total exports of country",
  coal_and_metal_exports_share_country="Share of coal-and-metal-related goods in total exports of country"
)

label(sitc4_processed_red) = as.list(
  var.labels[match(names(sitc4_processed_red), names(var.labels))])
saveRDS(object = sitc4_processed_red,
        file = here("data/tidy/sitc_primary_exports_alt.rds"))

