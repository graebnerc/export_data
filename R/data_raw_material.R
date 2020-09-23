rm(list = ls())
library(haven)
library(here)
library(Hmisc)

# Raw file used (from Harvard Dataverse):
# The Growth Lab at Harvard University, 2019,
# "country_hsproduct2digit_year.tab", International Trade Data (HS, 92),
# https://doi.org/10.7910/DVN/T4CHWJ/8ATVXR, Harvard Dataverse, V3,
# UNF:6:87fqn4vGhKTiNJERT2q7CA== [fileUNF]

hs2 <- read_dta(here("data/raw/pure_exports/country_hsproduct2digit_year.dta"))
setDT(hs2)

# Product codes:
primary_goods <- c(
  paste0("0", 1:5), # Animals
  paste0("0", 6:9), paste0("1", 0:5), # Vegetable
  paste0("1", 6:9), paste0("2", 0:4) # Food products
)

natural_ressources <- c(
  paste0("2", 5:6),  # Minerals
  "27", # Fuels
  "41", # Raw hides and skins (other than  furskins)
  "71" # Natural/cultured pearls, prec stones &  metals
)

# Unclear:
unclear_goods <- c(
  "44", "45", # Wood and articles of wood; wood  charcoal; Cork and articles of cork.
  "51", # Wool, fine/coarse animal hair, horsehair  yarn;
  "51", # Cotton
  paste0("7", 2:9), paste0("8",0:3) # Metals
)

# Standard data----------------------------------------------------------------
hs2_processed <- hs2[, .(year, export_value, location_code, hs_product_code)]
hs2_processed[, world_trade:=sum(export_value), year]
hs2_processed[, country_trade:=sum(export_value), .(year, location_code)]
hs2_processed[, country_share:=country_trade/world_trade]

hs2_processed[hs_product_code %in% primary_goods,
              primary_exports:=sum(export_value), .(year, location_code)]
hs2_processed[hs_product_code %in% natural_ressources,
              natural_exports:=sum(export_value), .(year, location_code)]

hs2_processed[, primary_exports_share_world:=primary_exports/world_trade]
hs2_processed[, primary_exports_share_country:=primary_exports/country_trade]
hs2_processed[, natural_exports_share_world:=natural_exports/world_trade]
hs2_processed[, natural_exports_share_country:=natural_exports/country_trade]
hs2_processed[, hs_product_code:=NULL]
hs2_processed[, export_value:=NULL]
hs2_processed <- unique(hs2_processed)
hs2_processed

hs2_processed <- melt(
  hs2_processed, id.vars = c("year", "location_code",  "world_trade",
                             "country_trade", "country_share")
  )
hs2_processed <- hs2_processed[!is.na(value)]
hs2_processed <- dcast(hs2_processed, year + location_code + world_trade +
                         country_trade + country_share ~ variable,
                       value.var = "value")

var.labels <- c(
  year="Year of obs",
  location_code="Exporting country (iso3c)",
  world_trade="Total world exports in current USD",
  country_trade="Total exports of country in current USD",
  country_share="Share of country exports in total world exports",
  primary_exports="Total exports of primary goods by country in current USD",
  natural_exports="Total exports of natural ressources by country in current USD",
  primary_exports_share_world="Export share of country primary goods exports",
  primary_exports_share_country="Share of primary goods in total exports of country",
  natural_exports_share_world="Export share of country natural ressources exports",
  natural_exports_share_country="Share of natural ressources in total exports of country"
)
label(hs2_processed) = as.list(
  var.labels[match(names(hs2_processed), names(var.labels))])
saveRDS(object = hs2_processed,
        file = here("data/tidy/hs2_primary_exports.rds"))

# Alternative data-------------------------------------------------------------

# Alternative computation without the following HS product classes:
removed_codes <- c(
  "unspecified", "travel", "transport", "ict", "financial"
)

hs2_processed_red <- hs2[, .(year, export_value, location_code, hs_product_code)]
hs2_processed_red <- hs2_processed_red[!hs_product_code %in% removed_codes]
hs2_processed_red[, world_trade:=sum(export_value), year]
hs2_processed_red[, country_trade:=sum(export_value), .(year, location_code)]
hs2_processed_red[, country_share:=country_trade/world_trade]

hs2_processed_red[hs_product_code %in% primary_goods,
              primary_exports:=sum(export_value), .(year, location_code)]
hs2_processed_red[hs_product_code %in% natural_ressources,
              natural_exports:=sum(export_value), .(year, location_code)]

hs2_processed_red[, primary_exports_share_world:=primary_exports/world_trade]
hs2_processed_red[, primary_exports_share_country:=primary_exports/country_trade]
hs2_processed_red[, natural_exports_share_world:=natural_exports/world_trade]
hs2_processed_red[, natural_exports_share_country:=natural_exports/country_trade]
hs2_processed_red[, hs_product_code:=NULL]
hs2_processed_red[, export_value:=NULL]
hs2_processed_red <- unique(hs2_processed_red)
hs2_processed_red

hs2_processed_red <- melt(
  hs2_processed_red, id.vars = c("year", "location_code",  "world_trade",
                             "country_trade", "country_share")
)
hs2_processed_red <- hs2_processed_red[!is.na(value)]
hs2_processed_red <- dcast(hs2_processed_red, year + location_code + world_trade +
                         country_trade + country_share ~ variable,
                       value.var = "value")

var.labels <- c(
  year="Year of obs",
  location_code="Exporting country (iso3c)",
  world_trade="Total world exports in current USD",
  country_trade="Total exports of country in current USD",
  country_share="Share of country exports in total world exports",
  primary_exports="Total exports of primary goods by country in current USD",
  natural_exports="Total exports of natural ressources by country in current USD",
  primary_exports_share_world="Export share of country primary goods exports",
  primary_exports_share_country="Share of primary goods in total exports of country",
  natural_exports_share_world="Export share of country natural ressources exports",
  natural_exports_share_country="Share of natural ressources in total exports of country"
)
label(hs2_processed_red) = as.list(
  var.labels[match(names(hs2_processed_red), names(var.labels))])
saveRDS(object = hs2_processed_red,
        file = here("data/tidy/hs2_primary_exports_alt.rds"))
