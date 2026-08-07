# remember to load postgres first

pkgs <- c(
  "RPostgres",
  "dplyr",
  "dbplyr",
  "tidyr",
  "purrr",
  "stringr",
  "readr",
  "readxl",
  "janitor"
)

for (pkg in pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

library(RPostgres)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(readr)
library(readxl)
library(janitor)

# sudo -iu postgres psql -c "CREATE DATABASE comtrade;"
# sudo -iu postgres psql -c "CREATE ROLE pacha WITH LOGIN PASSWORD 'your_password';"
# sudo -iu postgres psql -c "GRANT ALL PRIVILEGES ON DATABASE comtrade TO pacha;"
# sudo -iu postgres psql -d comtrade -c "GRANT USAGE ON SCHEMA public TO pacha;"
# sudo -iu postgres psql -d comtrade -c "GRANT CREATE ON SCHEMA public TO pacha;"
# sudo -iu postgres psql -c "SHOW data_directory;"

# -- DB Version: 18
# -- OS Type: linux
# -- DB Type: desktop
# -- Total Memory (RAM): 16 GB
# -- CPUs num: 8
# -- Connections num: 20
# -- Data Storage: ssd

# ALTER SYSTEM SET
#  max_connections = '20';
# ALTER SYSTEM SET
#  shared_buffers = '1GB';
# ALTER SYSTEM SET
#  effective_cache_size = '4GB';
# ALTER SYSTEM SET
#  maintenance_work_mem = '1GB';
# ALTER SYSTEM SET
#  checkpoint_completion_target = '0.9';
# ALTER SYSTEM SET
#  wal_buffers = '16MB';
# ALTER SYSTEM SET
#  default_statistics_target = '100';
# ALTER SYSTEM SET
#  random_page_cost = '1.1';
# ALTER SYSTEM SET
#  effective_io_concurrency = '200';
# ALTER SYSTEM SET
#  work_mem = '31207kB';
# ALTER SYSTEM SET
#  huge_pages = 'off';
# ALTER SYSTEM SET
#  min_wal_size = '100MB';
# ALTER SYSTEM SET
#  max_wal_size = '2GB';
# ALTER SYSTEM SET
#  max_worker_processes = '8';
# ALTER SYSTEM SET
#  max_parallel_workers_per_gather = '4';
# ALTER SYSTEM SET
#  max_parallel_workers = '8';
# ALTER SYSTEM SET
#  max_parallel_maintenance_workers = '4';
# ALTER SYSTEM SET
#  wal_level = 'minimal';
# ALTER SYSTEM SET
#  max_wal_senders = '0';

con <- dbConnect(
  RPostgres::Postgres(),
  dbname = Sys.getenv("COMTRADE_NAME"),
  host = Sys.getenv("COMTRADE_HOST"),
  user = Sys.getenv("COMTRADE_USER"),
  password = Sys.getenv("COMTRADE_PASSWORD"),
  port = Sys.getenv("COMTRADE_PORT")
)

# Drop existing tables

# dbExecute(con, "DROP TABLE IF EXISTS public.classification_codes CASCADE;")
# dbExecute(con, "DROP TABLE IF EXISTS public.commodity_codes CASCADE;")
# dbExecute(con, "DROP TABLE IF EXISTS public.commodity_correlations CASCADE;")
# dbExecute(con, "DROP TABLE IF EXISTS public.country_codes CASCADE;")
# dbExecute(con, "DROP TABLE IF EXISTS public.exports CASCADE;")
# dbExecute(con, "DROP TABLE IF EXISTS public.imports CASCADE;")
# dbExecute(con, "DROP TABLE IF EXISTS public.unit_codes CASCADE;")
# dbExecute(con, "DROP TABLE IF EXISTS public.mot_codes CASCADE;")
# dbExecute(con, "DROP TABLE IF EXISTS public.customs_codes CASCADE;")
# dbExecute(con, "DROP TABLE IF EXISTS public.flow_codes CASCADE;")
# dbExecute(con, "DROP TABLE IF EXISTS public.mos_codes CASCADE;")

dinp <- "input/data"
dinp2 <- "input/meta"

# classification codes ----

d_classification_codes <- tibble(
  classification_code = c(
    "HS", "H0", "H1", "H2", "H3", "H4", "H5", "H6",
    "S1", "S2", "S3", "S4", "SS", "B4", "B5"
  ),
  classification_id = 1L:15L
)

if (!dbExistsTable(con, "classification_codes")) {
  dbWriteTable(
    con,
    "classification_codes",
    d_classification_codes,
    row.names = FALSE,
    field.types = c(
      classification_code = "text",
      classification_id = "integer"
    )
  )
}

map_hs <- paste0("HS", c(22, 17, 12, "07", "02", 96, "92"))

d_commodity_hs <- map_df(
  map_hs,
  function(x) {
    read_excel(paste0(dinp2, "/HSCodeandDescription.xlsx"),
      sheet = x
    ) %>%
      clean_names() %>%
      rename(
        classification_code = classification,
        commodity_code = code
      )
  }
)

d_commodity_hs$is_basic_level <- as.integer(d_commodity_hs$is_basic_level)

map_sitc <- paste0("SITC", 4:1)

d_commodity_sitc <- map_df(
  map_sitc,
  function(x) {
    read_excel(paste0(dinp2, "/SITCCodeandDescription.xlsx"),
      sheet = x
    ) %>%
      clean_names() %>%
      rename(
        classification_code = classification,
        commodity_code = code
      )
  }
)

d_commodity_sitc$is_basic_level <- as.integer(d_commodity_sitc$is_basic_level)

map_bec <- paste0("BEC", 5:4)

d_commodity_bec <- map_df(
  map_bec,
  function(x) {
    read_excel(paste0(dinp2, "/BECCodeandDescription.xlsx"),
      sheet = x
    ) %>%
      clean_names() %>%
      rename(
        classification_code = classification,
        commodity_code = code
      )
  }
)

d_commodity_bec$is_basic_level <- as.integer(d_commodity_bec$is_basic_level)

d_commodities <- bind_rows(
  d_commodity_hs,
  d_commodity_sitc,
  d_commodity_bec
) %>%
  mutate(
    classification_code = str_trim(classification_code),
    commodity_code = str_trim(commodity_code),
    description = str_trim(description),
    parent_code = str_trim(parent_code)
  )

d_commodities$level <- as.integer(d_commodities$level)

rm(
  d_commodity_hs,
  d_commodity_sitc,
  d_commodity_bec
)

d_commodities <- d_commodities %>%
  filter(classification_code != "SIT") %>%
  mutate(
    classification_code = case_when(
      classification_code == "BE5" ~ "B5",
      classification_code == "BE4" ~ "B4",
      TRUE ~ classification_code
    )
  )

d_commodities <- d_commodities %>%
  left_join(d_classification_codes) %>%
  select(
    classification_code,
    classification_id,
    commodity_code,
    description,
    parent_code,
    level,
    is_basic_level
  )

# Check for missing commodity codes and report count only
missing_codes_count <- d_commodities %>%
  filter(is.na(commodity_code)) %>%
  nrow()

if (missing_codes_count > 0) {
  warning("Found ", missing_codes_count, " records with missing commodity codes")
}

if (!dbExistsTable(con, "commodity_codes")) {
  dbWriteTable(
    con,
    "commodity_codes",
    d_commodities,
    row.names = FALSE,
    field.types = c(
      classification_code = "text",
      classification_id = "integer",
      commodity_code = "text",
      description = "text",
      parent_code = "text",
      level = "integer",
      is_basic_level = "integer"
    )
  )
}

# add primary key by classification id
try(dbExecute(con, "ALTER TABLE classification_codes ADD PRIMARY KEY (classification_id);"), silent = TRUE)

# add primary key by classification_code and commodity_code
try(dbExecute(con, "ALTER TABLE commodity_codes ADD PRIMARY KEY (classification_id, commodity_code);"), silent = TRUE)

# correlation table ----

d_correlation <- read_excel(paste0(dinp2, "/HS-SITC-BEC_Correlations_2022.xlsx")) %>%
  clean_names() %>%
  rename(
    h0 = hs92, h1 = hs96, h2 = hs02, h3 = hs07, h4 = hs12, h5 = hs17, h6 = hs22,
    s1 = sitc1, s2 = sitc2, s3 = sitc3, s4 = sitc4,
    b4 = bec4, b5 = bec5
  ) %>%
  select(
    h0, h1, h2, h3, h4, h5, h6,
    s1, s2, s3, s4,
    b4, b5
  ) %>%
  mutate(across(everything(), str_trim))

if (!dbExistsTable(con, "commodity_correlations")) {
  dbWriteTable(
    con,
    "commodity_correlations",
    d_correlation,
    row.names = FALSE,
    field.types = c(
      h0 = "text",
      h1 = "text",
      h2 = "text",
      h3 = "text",
      h4 = "text",
      h5 = "text",
      h6 = "text",
      s1 = "text",
      s2 = "text",
      s3 = "text",
      s4 = "text",
      b4 = "text",
      b5 = "text"
    )
  )
}

# country codes ----

# d_country_codes <- comtradr::country_codes %>%
#   as_tibble() %>%
#   select(country_code = id, country_iso3 = iso_3, country_name = country) %>%
#   distinct()

# saveRDS(d_country_codes, paste0(dinp2, "/country_codes.rds"))

d_country_codes <- readRDS(paste0(dinp2, "/country_codes.rds")) %>%
  mutate(
    country_iso3 = str_trim(country_iso3),
    country_name = str_trim(country_name)
  ) %>%
  arrange(country_code)

if (!dbExistsTable(con, "country_codes")) {
  dbWriteTable(
    con,
    "country_codes",
    d_country_codes,
    row.names = FALSE,
    field.types = c(
      country_code = "integer",
      country_iso3 = "text",
      country_name = "text"
    )
  )
}

# add primary key by country id
try(dbExecute(con, "ALTER TABLE country_codes ADD PRIMARY KEY (country_code);"), silent = TRUE)

# units ----

d_units <- tibble(
  qty_unit_code = c(-1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 25L, 26L, 27L, 28L, 29L, 30L, 31L, 32L, 33L, 34L, 35L, 36L, 37L, 38L, 39L, 40L, 41L),
  qty_abbr = c("N/A", "m2", "1000 kWh", "m", "u", "2u", "l", "kg", "1000u", "U (jeu/pack)", "12u", "m3", "carat", "km", "g", "hive", "1000 m3", "TJ", "BBL", "1000 L", "1000 KG", "kWH", "l alc 100%", "head", "kg/net eda", "kg C5H14ClNO", "kg P2O5", "kg H2O2", "kg met.am.", "kg N", "kg KOH", "kg K2O", "kg NaOH", "kg 90% sdt", "kg U", "ct/l", "Bq", "gi F/S", "GRT", "GT", "ce/el"),
  qty_description = c(
    "Not available or not specified or no quantity.",
    "Area in square meters",
    "Electrical energy in thousands of kilowatt-hours",
    "Length in meters",
    "Number of items",
    "Number of pairs",
    "Volume in liters",
    "Weight in kilograms",
    "Thousand of items",
    "Number of packages",
    "Dozen of items",
    "Volume in cubic meters",
    "Weight in carats",
    "Length in Kilometers",
    "Weight in grams",
    "Beehive",
    "Volume in thousand cubic meters",
    "Terajoule (gross calorific value)",
    "Barrels",
    "Volume in thousands of liters",
    "Weight in thousand of kilograms",
    "Electrical energy in kilowatt-hours",
    "Litre pure (100 %) alcohol - l alc. 100%",
    "Head",
    "Kilogram drained net weight",
    "Kilogram of choline chloride",
    "Kilogram of diphosphorus pentaoxide",
    "Kilogram of hydrogen peroxide",
    "Kilogram of methylamines",
    "Kilogram of nitrogen",
    "Kilogram of potassium hydroxide (caustic potash)",
    "Kilogram of potassium oxide",
    "Kilogram of sodium hydroxide (caustic soda)",
    "Kilogram of substance 90 % dry",
    "Kilogram of uranium",
    "Carrying capacity in tonnes",
    "Becquerels",
    "Gram of fissile isotopes",
    "Gross register ton",
    "Gross tonnage",
    "Number of cells/elements"
  )
) %>%
  mutate(
    qty_abbr = str_trim(qty_abbr),
    qty_description = str_trim(qty_description)
  )

if (!dbExistsTable(con, "unit_codes")) {
  dbWriteTable(
    con,
    "unit_codes",
    d_units,
    row.names = FALSE,
    field.types = c(
      qty_unit_code = "integer",
      qty_abbr = "text",
      qty_description = "text"
    )
  )
}

# add primary key by qty_unit_code
try(dbExecute(con, "ALTER TABLE unit_codes ADD PRIMARY KEY (qty_unit_code);"), silent = TRUE)

# mode of transport codes ----

d_mot_codes <- tibble(
  mot_code = c(0L, 1000L, 2000L, 2100L, 2200L, 2900L, 3000L, 3100L, 3200L, 3900L, 9000L, 9100L, 9110L, 9120L, 9190L, 9200L, 9300L, 9900L),
  mot_name = c(
    "Total Modes of Transport",
    "Air",
    "Water",
    "Sea",
    "Inland waterway",
    "Water, not else classified",
    "Land",
    "Railway",
    "Road",
    "Land, not else classified",
    "Not elsewhere classified",
    "Pipelines and cables",
    "Pipelines",
    "Cables",
    "Pipelines and cables, not else classified",
    "Postal consignments, mail or courier shipment",
    "Self propelled goods",
    "Other"
  )
) %>%
  mutate(mot_name = str_trim(mot_name))

if (!dbExistsTable(con, "mot_codes")) {
  dbWriteTable(
    con,
    "mot_codes",
    d_mot_codes,
    row.names = FALSE,
    field.types = c(
      mot_code = "integer",
      mot_name = "text"
    )
  )
}

# add primary key by mot_code
try(dbExecute(con, "ALTER TABLE mot_codes ADD PRIMARY KEY (mot_code);"), silent = TRUE)

# customs procedure codes ----

d_customs_codes <- tibble(
  customs_code = c("C00", "C01", "C02", "C03", "C04", "C05", "C06", "C07", "C08", "C09", "C10", "C11", "C12", "C13", "C14", "C15", "C20"),
  customs_description = c(
    "TOTAL CPC",
    "Clearance for home use",
    "Reimportation in the same state",
    "Outright exportation",
    "Customs warehouses",
    "Free zone",
    "Inward processing",
    "Outward processing",
    "Drawback",
    "Processing of goods for home use",
    "Carriage of goods coastwise",
    "Customs offences",
    "Travellers",
    "Postal traffic",
    "Stores",
    "Relief consignments",
    "CPC N.E.S."
  )
) %>%
  mutate(
    customs_code = str_trim(customs_code),
    customs_description = str_trim(customs_description)
  )

if (!dbExistsTable(con, "customs_codes")) {
  dbWriteTable(
    con,
    "customs_codes",
    d_customs_codes,
    row.names = FALSE,
    field.types = c(
      customs_code = "text",
      customs_description = "text"
    )
  )
}

# add primary key by customs_code
try(dbExecute(con, "ALTER TABLE customs_codes ADD PRIMARY KEY (customs_code);"), silent = TRUE)

# flow codes ----

d_flow_codes <- tibble(
  flow_code = c("FM", "M", "MIP", "MOP", "RM", "DX", "RX", "X", "XIP", "XOP"),
  flow_description = c(
    "Foreign Import",
    "Import",
    "Import of goods for inward processing",
    "Import of goods after outward processing",
    "Re-import",
    "Domestic Export",
    "Re-export",
    "Export",
    "Export of goods after inward processing",
    "Export of goods for outward processing"
  ),
  flow_category = c("M", "M", "M", "M", "M", "X", "X", "X", "X", "X")
) %>%
  mutate(
    flow_code = str_trim(flow_code),
    flow_description = str_trim(flow_description),
    flow_category = str_trim(flow_category)
  )

if (!dbExistsTable(con, "flow_codes")) {
  dbWriteTable(
    con,
    "flow_codes",
    d_flow_codes,
    row.names = FALSE,
    field.types = c(
      flow_code = "text",
      flow_description = "text",
      flow_category = "text"
    )
  )
}

# add primary key by flow_code
try(dbExecute(con, "ALTER TABLE flow_codes ADD PRIMARY KEY (flow_code);"), silent = TRUE)

# mode of supply codes ----

d_mos_codes <- tibble(
  mos_code = c(0L, 1L, 2L, 3L, 4L),
  mos_description = c(
    "All Modes of Supply",
    "Cross-border",
    "Consumption abroad",
    "Commercial presence",
    "Presence of natural persons"
  )
) %>%
  mutate(mos_description = str_trim(mos_description))

if (!dbExistsTable(con, "mos_codes")) {
  dbWriteTable(
    con,
    "mos_codes",
    d_mos_codes,
    row.names = FALSE,
    field.types = c(
      mos_code = "integer",
      mos_description = "text"
    )
  )
}

# add primary key by mos_code
try(dbExecute(con, "ALTER TABLE mos_codes ADD PRIMARY KEY (mos_code);"), silent = TRUE)

# trade ----

# Create table if it doesn't exist
create_trade_table <- function(con, table_name, value_col_name) {
  if (!dbExistsTable(con, table_name)) {
    message("Creating ", table_name, " table...")

    empty_df <- data.frame(
      year = integer(),
      reporter_code = integer(),
      partner_code = integer(),
      partner2code = integer(),
      classification_search_code = character(),
      classification_id = integer(),
      is_original_classification = integer(),
      commodity_code = character(),
      customs_code = character(),
      mos_code = integer(),
      mot_code = integer(),
      qty_unit_code = integer(),
      qty = double(),
      is_qty_estimated = integer(),
      alt_qty_unit_code = integer(),
      alt_qty = double(),
      is_alt_qty_estimated = integer(),
      net_wgt = double(),
      is_net_wgt_estimated = integer(),
      gross_wgt = double(),
      is_gross_wgt_estimated = integer(),
      value = double(),
      primary_value = double(),
      legacy_estimation_flag = integer(),
      is_reported = integer(),
      is_aggregate = integer(),
      added_zero = integer()
    )

    colnames(empty_df)[colnames(empty_df) == "value"] <- value_col_name

    field_types <- c(
      year = "integer",
      reporter_code = "integer",
      partner_code = "integer",
      partner2code = "integer",
      classification_search_code = "text",
      classification_id = "integer",
      is_original_classification = "integer",
      commodity_code = "text",
      customs_code = "text",
      mos_code = "integer",
      mot_code = "integer",
      qty_unit_code = "integer",
      qty = "double precision",
      is_qty_estimated = "integer",
      alt_qty_unit_code = "integer",
      alt_qty = "double precision",
      is_alt_qty_estimated = "integer",
      net_wgt = "double precision",
      is_net_wgt_estimated = "integer",
      gross_wgt = "double precision",
      is_gross_wgt_estimated = "integer",
      value = "double precision",
      primary_value = "double precision",
      legacy_estimation_flag = "integer",
      is_reported = "integer",
      is_aggregate = "integer",
      added_zero = "integer"
    )

    names(field_types)[names(field_types) == "value"] <- value_col_name

    dbWriteTable(con, table_name, empty_df, row.names = FALSE, field.types = field_types)
  }
}

# Process a single file for exports or imports
process_file <- function(x, flow_type, d_country_codes, d_classification_codes) {
  message(rep("-", 40))
  message("Processing ", basename(x))
  message(rep("-", 40))

  iso3_code <- substr(basename(x), 1, 3)
  year_from_filename <- as.integer(substr(basename(x), 5, 8))

  # Get country code for this file
  matching_entries <- d_country_codes %>% filter(country_iso3 == iso3_code)
  if (nrow(matching_entries) == 0) {
    warning("Country ISO3 code '", iso3_code, "' not found in country_codes table for file ", basename(x))
    return(data.frame())
  }

  # Determine which value column to keep based on flow type
  # TODO: add re-imports / re-exports ?
  value_col <- if (flow_type == "X") "fobvalue" else "cifvalue"
  drop_col <- if (flow_type == "X") "cifvalue" else "fobvalue"

  # Read and preprocess file, filter by reporter and flow
  d_trade <- readRDS(x) %>%
    mutate(across(where(is.character), str_trim)) %>%
    filter(flow_code == flow_type) %>%
    select(-flow_code, -all_of(drop_col))

  # Product-level filter
  d_trade <- d_trade %>%
    filter(
      case_when(
        classification_code %in% c("S1", "S2", "S3", "S4") ~ str_length(cmd_code) == 5,
        TRUE ~ str_length(cmd_code) == 6
      ),
      cmd_code != "TOTAL",
      str_length(partner_code) %in% 2:3
    )

  # Type conversions
  d_trade <- d_trade %>%
    mutate(
      dataset_code = as.character(dataset_code),
      year = as.integer(period),
      partner2code = as.integer(partner2code),
      is_original_classification = as.integer(is_original_classification),
      reporter_code = as.integer(reporter_code),
      partner_code = as.integer(partner_code),
      mos_code = as.integer(mos_code),
      mot_code = as.integer(mot_code),
      qty_unit_code = as.integer(qty_unit_code),
      is_qty_estimated = as.integer(is_qty_estimated),
      alt_qty_unit_code = as.integer(alt_qty_unit_code),
      is_alt_qty_estimated = as.integer(is_alt_qty_estimated),
      qty = as.double(qty),
      alt_qty = as.double(alt_qty),
      net_wgt = as.double(net_wgt),
      is_net_wgt_estimated = as.integer(is_net_wgt_estimated),
      gross_wgt = as.double(gross_wgt),
      is_gross_wgt_estimated = as.integer(is_gross_wgt_estimated),
      !!sym(value_col) := as.double(!!sym(value_col)),
      primary_value = as.double(primary_value),
      legacy_estimation_flag = as.integer(legacy_estimation_flag),
      is_reported = as.integer(is_reported),
      is_aggregate = as.integer(is_aggregate)
    ) %>%
    select(-period)

  # Code validations
  d_trade <- d_trade %>%
    mutate(
      mos_code = case_when(mos_code %in% c(0L, 1L, 2L, 3L, 4L) ~ mos_code, TRUE ~ NA_integer_),
      mot_code = case_when(mot_code %in% c(
        0L, 1000L, 2000L, 2100L, 2200L, 2900L, 3000L, 3100L, 3200L, 3900L, 9000L,
        9100L, 9110L, 9120L, 9190L, 9200L, 9300L, 9900L
      ) ~ mot_code, TRUE ~ NA_integer_),
      qty_unit_code = case_when(qty_unit_code %in% c(
        -1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L,
        15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 25L, 26L, 27L, 28L, 29L, 30L, 31L, 32L, 33L, 34L, 35L,
        36L, 37L, 38L, 39L, 40L, 41L
      ) ~ qty_unit_code, TRUE ~ NA_integer_),
      customs_code = case_when(customs_code %in% c(
        "C00", "C01", "C02", "C03", "C04", "C05", "C06", "C07", "C08",
        "C09", "C10", "C11", "C12", "C13", "C14", "C15", "C20"
      ) ~ customs_code, TRUE ~ NA_character_)
    )

  # Join classification codes
  d_trade <- d_trade %>%
    rename(commodity_code = cmd_code) %>%
    left_join(d_classification_codes, by = "classification_code") %>%
    select(-classification_code)

  # Reorder columns
  d_trade <- d_trade %>%
    select(
      dataset_code, year, reporter_code,
      partner_code, partner2code, classification_search_code, classification_id, is_original_classification,
      commodity_code, customs_code, mos_code, mot_code, qty_unit_code, qty, is_qty_estimated, alt_qty_unit_code,
      alt_qty, is_alt_qty_estimated, net_wgt, is_net_wgt_estimated, gross_wgt, is_gross_wgt_estimated,
      all_of(value_col), primary_value, legacy_estimation_flag, is_reported, is_aggregate
    )

  # Handle missing values
  d_trade <- d_trade %>%
    mutate(
      added_zero = case_when(is.na(!!sym(value_col)) ~ 1L, TRUE ~ 0L),
      !!sym(value_col) := case_when(is.na(!!sym(value_col)) ~ 0, TRUE ~ !!sym(value_col))
    )

  if (!nrow(d_trade) > 0) d_trade <- data.frame()

  d_trade
}

# create "dataset_codes" table if it doesn't exist
if (!dbExistsTable(con, "dataset_codes")) {
  dbWriteTable(
    con,
    "dataset_codes",
    data.frame(
      year = integer(),
      reporter_code = integer(),
      classification_id = integer(),
      dataset_code = character()
    ),
    row.names = FALSE,
    field.types = c(
      year = "integer",
      reporter_code = "integer",
      classification_id = "integer",
      dataset_code = "text"
    )
  )
}

# Process all files for a given year and flow type
process_year <- function(y, flow_type, table_name, value_col_name, con, dinp, d_country_codes, d_classification_codes) {
  message("========================================")
  message("Copying data for year ", y)
  message("========================================")

  finp <- list.files(dinp, pattern = paste0(y, "\\.rds$"), full.names = TRUE)

  if (length(finp) == 0) {
    message("No files found for year ", y, ". Skipping.")
    return(TRUE)
  }

  message("Found ", length(finp), " files for year ", y)

  # Create table if needed
  create_trade_table(con, table_name, value_col_name)

  # Process all files
  d_trade <- map(finp, function(x) {
    d_aux <- process_file(x, flow_type, d_country_codes, d_classification_codes)

    if (nrow(d_aux) == 0) {
      return(TRUE)
    }

    d_meta <- d_aux %>%
      select(year, reporter_code, classification_id, dataset_code) %>%
      distinct()

    d_aux <- d_aux %>%
      select(-dataset_code)

    # if d_meta$dataset_code not present in "dataset_codes" table, write to it
    existing_dataset_codes <- tbl(con, "dataset_codes") %>%
      filter(year == d_meta$year, reporter_code == d_meta$reporter_code, classification_id == d_meta$classification_id) %>%
      pull(dataset_code) %>%
      unique()

    if (length(existing_dataset_codes) == 0L) {
      dbWriteTable(
        con,
        "dataset_codes",
        d_meta,
        append = TRUE,
        row.names = FALSE
      )
    }

    # delete the old data that matches year + reporter_code
    dbExecute(
      con,
      paste0(
        "DELETE FROM ", table_name, " WHERE year = ", d_meta$year,
        " AND reporter_code = ", d_meta$reporter_code
      )
    )

    # delete old meta data that matches year + reporter_code
    dbExecute(
      con,
      paste0(
        "DELETE FROM dataset_codes WHERE year = ", d_meta$year,
        " AND reporter_code = ", d_meta$reporter_code,
        " AND dataset_code != '", d_meta$dataset_code, "'"
      )
    )

    # purrr's nest to split data into chunks for dbWriteTable
    # chunk = 500,000 rows at a time
    d_aux <- d_aux %>%
      mutate(chunk_id = (row_number() - 1) %/% 500000) %>%
      group_by(chunk_id) %>%
      nest() %>%
      ungroup() %>%
      select(data) %>%
      pull()

    j <- length(d_aux)

    map(
      seq_along(d_aux),
      function(i) {
        message("Writing chunk ", i, " of ", j, " to ", table_name, "...")
        dbWriteTable(con, table_name, d_aux[[i]], append = TRUE, row.names = FALSE)
      }
    )

    return(TRUE)
  })

  return(TRUE)
}

map(2016:2025, function(y) {
  process_year(y, "X", "exports", "fobvalue", con, dinp, d_country_codes, d_classification_codes)
})

# map(1962:2025, function(y) {
#   process_year(y, "M", "imports", "cifvalue", con, dinp, d_country_codes, d_classification_codes)
# })

# indexes and foreign keys (besides year) ----

message("Creating indexes and foreign keys...")

## exports ----

try(dbExecute(con, "CREATE INDEX idx_exports_year ON exports (year);"), silent = TRUE)
try(dbExecute(con, "CREATE INDEX idx_exports_reporter ON exports (reporter_code);"), silent = TRUE)
try(dbExecute(con, "CREATE INDEX idx_exports_partner ON exports (partner_code);"), silent = TRUE)
try(dbExecute(con, "CREATE INDEX idx_exports_commodity ON exports (commodity_code);"), silent = TRUE)

try(dbExecute(con, "ALTER TABLE exports ADD CONSTRAINT fk_exports_classification_codes
  FOREIGN KEY (classification_id) REFERENCES classification_codes (classification_id);"), silent = TRUE)

try(dbExecute(con, "ALTER TABLE exports ADD CONSTRAINT fk_exports_country_codes
  FOREIGN KEY (reporter_code) REFERENCES country_codes (country_code);"), silent = TRUE)

try(dbExecute(con, "ALTER TABLE exports ADD CONSTRAINT fk_exports_country_codes_2
  FOREIGN KEY (partner_code) REFERENCES country_codes (country_code);"), silent = TRUE)

try(dbExecute(con, "ALTER TABLE exports ADD CONSTRAINT fk_exports_commodity_codes
  FOREIGN KEY (classification_id, commodity_code) REFERENCES commodity_codes (classification_id, commodity_code);"), silent = TRUE)

try(dbExecute(con, "ALTER TABLE exports ADD CONSTRAINT fk_exports_units
  FOREIGN KEY (qty_unit_code) REFERENCES unit_codes (qty_unit_code);"), silent = TRUE)

try(dbExecute(con, "ALTER TABLE exports ADD CONSTRAINT fk_exports_mot_codes
  FOREIGN KEY (mot_code) REFERENCES mot_codes (mot_code);"), silent = TRUE)

try(dbExecute(con, "ALTER TABLE exports ADD CONSTRAINT fk_exports_customs_codes
  FOREIGN KEY (customs_code) REFERENCES customs_codes (customs_code);"), silent = TRUE)

try(dbExecute(con, "ALTER TABLE exports ADD CONSTRAINT fk_exports_mos_codes
  FOREIGN KEY (mos_code) REFERENCES mos_codes (mos_code);"), silent = TRUE)

## imports ----

try(dbExecute(con, "CREATE INDEX idx_imports_year ON imports (year);"), silent = TRUE)
try(dbExecute(con, "CREATE INDEX idx_imports_reporter ON imports (reporter_code);"), silent = TRUE)
try(dbExecute(con, "CREATE INDEX idx_imports_partner ON imports (partner_code);"), silent = TRUE)
try(dbExecute(con, "CREATE INDEX idx_imports_commodity ON imports (commodity_code);"), silent = TRUE)

try(dbExecute(con, "ALTER TABLE imports ADD CONSTRAINT fk_imports_classification_codes
  FOREIGN KEY (classification_id) REFERENCES classification_codes (classification_id);"), silent = TRUE)

try(dbExecute(con, "ALTER TABLE imports ADD CONSTRAINT fk_imports_country_codes
  FOREIGN KEY (reporter_code) REFERENCES country_codes (country_code);"), silent = TRUE)

try(dbExecute(con, "ALTER TABLE imports ADD CONSTRAINT fk_imports_country_codes_2
  FOREIGN KEY (partner_code) REFERENCES country_codes (country_code);"), silent = TRUE)

try(dbExecute(con, "ALTER TABLE imports ADD CONSTRAINT fk_imports_commodity_codes
  FOREIGN KEY (classification_id, commodity_code) REFERENCES commodity_codes (classification_id, commodity_code);"), silent = TRUE)

try(dbExecute(con, "ALTER TABLE imports ADD CONSTRAINT fk_imports_units
  FOREIGN KEY (qty_unit_code) REFERENCES unit_codes (qty_unit_code);"), silent = TRUE)

try(dbExecute(con, "ALTER TABLE imports ADD CONSTRAINT fk_imports_mot_codes
  FOREIGN KEY (mot_code) REFERENCES mot_codes (mot_code);"), silent = TRUE)

try(dbExecute(con, "ALTER TABLE imports ADD CONSTRAINT fk_imports_customs_codes
  FOREIGN KEY (customs_code) REFERENCES customs_codes (customs_code);"), silent = TRUE)

try(dbExecute(con, "ALTER TABLE imports ADD CONSTRAINT fk_imports_mos_codes
  FOREIGN KEY (mos_code) REFERENCES mos_codes (mos_code);"), silent = TRUE)

dbDisconnect(con)
