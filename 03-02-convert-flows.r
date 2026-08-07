# Apply LT conversion weights to bilateral exports/imports flows,
# converting every year's data to a single target classification vintage.

source("03-00-lt-helpers.r")

on.exit(try(dbDisconnect(con), silent = TRUE), add = TRUE)

# Configuration ----
# h0 = HS1992; change to any node in adj_graph
# h1 = HS1996, h2 = HS2002, h3 = HS2007, h4 = HS2012, h5 = HS2017, h6 = HS2022

# target_class <- "h3"
target_class <- commandArgs(trailingOnly = TRUE)
digits       <- Sys.getenv("COMTRADE_HS_DIGITS")
y            <- Sys.getenv("COMTRADE_YEARS")
y            <- as.integer(unlist(strsplit(y, ":")))
y            <- seq(min(y), max(y))

if (!(nchar(target_class) == 2L)) stop("Missing target class")
if (!any(digits %in% c(4L,6L))) stop("Digits undefined (use 4 or 6)")
if (!min(y) >= 1962L) stop("Years undefined (>= 1962)")

classification_id_numeric <- setDT(dbGetQuery(con, "select distinct classification_code, classification_id from commodity_codes"))
classification_id_numeric <- classification_id_numeric[classification_code == toupper(target_class)]
classification_id_numeric <- classification_id_numeric$classification_id

# Load cached pair weights ----

pair_weights <- load_all_pair_weights(digits)
have_pairs <- names(pair_weights)
message("Loaded ", length(have_pairs), " pair weight tables: ",
        paste(have_pairs, collapse = ", "))

# Sanity-check that the path from every relevant source class to the target
# is fully covered by cached pairs.
cls <- load_classification_codes(con)
d_codes <- cls$d_codes

classification_codes_known <- d_codes[
  !classification_code %in% c("hs", "ss", "b4", "b5"),
  classification_code
]

d_conversion_weights <- list()
for (c_code in classification_codes_known) {
  if (c_code == target_class) {
    d_conversion_weights[[c_code]] <- data.table(
      from = character(), to = character(), weight = double()
    )
  } else if (c_code %in% igraph::V(adj_graph)$name) {
    d_conversion_weights[[c_code]] <- build_chain_weights(
      c_code, target_class, pair_weights
    )
  }
}

# Per-year conversion ----

exports_tbl <- paste0("exports_", target_class)
imports_tbl <- paste0("imports_", target_class)

for (y2 in y) {
  message(y2)

  exports_done <- FALSE
  imports_done <- FALSE

  if (dbExistsTable(con, exports_tbl)) {
    n <- dbGetQuery(con, paste0("SELECT COUNT(*) AS n FROM ", exports_tbl, " WHERE year = ", y2))$n
    if (n > 0) exports_done <- TRUE
  }

  if (dbExistsTable(con, imports_tbl)) {
    n <- dbGetQuery(con, paste0("SELECT COUNT(*) AS n FROM ", imports_tbl, " WHERE year = ", y2))$n
    if (n > 0) imports_done <- TRUE
  }

  if (exports_done && imports_done) {
    message("  already exists in database, skipping")
    next
  }

  dc <- as.data.table(dbGetQuery(con, paste0(
    "SELECT DISTINCT classification_id FROM dataset_codes WHERE year = ", y2
  )))
  dc <- merge(dc, d_codes, by = "classification_id", all.x = TRUE)
  classification_codes <- dc[order(classification_code), classification_code]

  ## exports ----
  if (!exports_done) {
    dbBegin(con)
    tryCatch({
      if (dbExistsTable(con, exports_tbl)) {
        dbExecute(con, paste0("DELETE FROM ", exports_tbl, " WHERE year = ", y2))
      }
      for (c_code in classification_codes) {
        message("  exports ", c_code)
        class_id <- cls$lookup[[c_code]]
        if (is.null(class_id)) next

        d_trade <- setDT(dbGetQuery(con, paste0(
          "SELECT year, reporter_code, partner_code, commodity_code, fobvalue, qty, qty_unit_code",
          " FROM exports WHERE year = ", y2, " AND classification_id = ", class_id
        )))

        d_trade <- d_trade[nchar(commodity_code) >= 4L]
        d_trade[, commodity_code_parent := substr(commodity_code, 1L, 4L)]
        d_trade[, commodity_code_top := fifelse(commodity_code == commodity_code_parent, TRUE, FALSE)]
        d_trade <- d_trade[commodity_code_top == FALSE]

        d_trade[, commodity_code_parent := NULL]
        d_trade[, commodity_code_top := NULL]
        d_trade[, commodity_code := substr(commodity_code, 1L, digits)]

        d_trade <- d_trade[, .(fobvalue = sum(fobvalue, na.rm = TRUE), qty = sum(qty, na.rm = TRUE)),
          by = .(year, reporter_code, partner_code, commodity_code, qty_unit_code)]

        if (nrow(d_trade) == 0) {
          rm(d_trade)
          next
        }

        weights <- d_conversion_weights[[c_code]]
        if (is.null(weights)) {
          rm(d_trade)
          next
        }

        d_chunk <- apply_weighted_conversion(d_trade, weights, c("fobvalue", "qty"))
        rm(d_trade)

        if (nrow(d_chunk) > 0) {
          d_chunk[, year := as.integer(y2)]
          d_chunk[, classification_id := as.integer(classification_id_numeric)]
          d_chunk <- d_chunk[, .(year, reporter_code, partner_code, classification_id, commodity_code, qty_unit_code, fobvalue, qty)]
          d_chunk[, fobvalue := round(fobvalue / 1000000, 3)]
          setorder(d_chunk, reporter_code, partner_code, commodity_code)
          dbWriteTable(con, exports_tbl, d_chunk, append = TRUE, row.names = FALSE)
        }
        rm(d_chunk)
        gc(verbose = FALSE)
      }
      dbCommit(con)
    }, error = function(e) {
      dbRollback(con)
      stop(e)
    })
  }

  ## imports ----
  if (!imports_done) {
    dbBegin(con)
    tryCatch({
      if (dbExistsTable(con, imports_tbl)) {
        dbExecute(con, paste0("DELETE FROM ", imports_tbl, " WHERE year = ", y2))
      }
      for (c_code in classification_codes) {
        message("  imports ", c_code)
        class_id <- cls$lookup[[c_code]]
        if (is.null(class_id)) next

        d_trade <- setDT(dbGetQuery(con, paste0(
          "SELECT year, reporter_code, partner_code, commodity_code, cifvalue, qty, qty_unit_code",
          " FROM imports WHERE year = ", y2, " AND classification_id = ", class_id
        )))

        d_trade <- d_trade[nchar(commodity_code) >= 4L]
        d_trade[, commodity_code_parent := substr(commodity_code, 1L, 4L)]
        d_trade[, commodity_code_top := fifelse(commodity_code == commodity_code_parent, TRUE, FALSE)]
        d_trade <- d_trade[commodity_code_top == FALSE]

        d_trade[, commodity_code_parent := NULL]
        d_trade[, commodity_code_top := NULL]
        d_trade[, commodity_code := substr(commodity_code, 1L, digits)]

        d_trade <- d_trade[, .(cifvalue = sum(cifvalue, na.rm = TRUE), qty = sum(qty, na.rm = TRUE)),
          by = .(year, reporter_code, partner_code, commodity_code, qty_unit_code)]

        if (nrow(d_trade) == 0) {
          rm(d_trade)
          next
        }

        weights <- d_conversion_weights[[c_code]]
        if (is.null(weights)) {
          rm(d_trade)
          next
        }

        d_chunk <- apply_weighted_conversion(d_trade, weights, c("cifvalue", "qty"))
        rm(d_trade)

        if (nrow(d_chunk) > 0) {
          d_chunk[, year := as.integer(y2)]
          d_chunk[, classification_id := as.integer(classification_id_numeric)]
          d_chunk <- d_chunk[, .(year, reporter_code, partner_code, classification_id, commodity_code, qty_unit_code, cifvalue, qty)]
          d_chunk[, cifvalue := round(cifvalue / 1000000, 3)]
          setorder(d_chunk, reporter_code, partner_code, commodity_code)
          dbWriteTable(con, imports_tbl, d_chunk, append = TRUE, row.names = FALSE)
        }
        rm(d_chunk)
        gc(verbose = FALSE)
      }
      dbCommit(con)
    }, error = function(e) {
      dbRollback(con)
      stop(e)
    })
  }

  gc(verbose = FALSE)
}

# Indexes ----

## exports ----

try(dbExecute(con, sprintf("CREATE INDEX idx_exports_%s_year ON exports_%s (year);", target_class, target_class)), silent = TRUE)
try(dbExecute(con, sprintf("CREATE INDEX idx_exports_%s_reporter ON exports_%s (reporter_code);", target_class, target_class)), silent = TRUE)
try(dbExecute(con, sprintf("CREATE INDEX idx_exports_%s_partner ON exports_%s (partner_code);", target_class, target_class)), silent = TRUE)
try(dbExecute(con, sprintf("CREATE INDEX idx_exports_%s_commodity ON exports_%s (commodity_code);", target_class, target_class)), silent = TRUE)

try(dbExecute(con, sprintf("ALTER TABLE exports_%s ADD CONSTRAINT fk_exports_%s_country_codes
  FOREIGN KEY (reporter_code) REFERENCES country_codes (country_code);", target_class, target_class)), silent = TRUE)

try(dbExecute(con, sprintf("ALTER TABLE exports_%s ADD CONSTRAINT fk_exports_%s_country_codes_2
  FOREIGN KEY (partner_code) REFERENCES country_codes (country_code);", target_class, target_class)), silent = TRUE)

try(dbExecute(con, sprintf("ALTER TABLE exports_%s ADD CONSTRAINT fk_exports_%s_commodity_codes
  FOREIGN KEY (classification_id, commodity_code) REFERENCES commodity_codes (classification_id, commodity_code);", target_class, target_class)), silent = TRUE)

try(dbExecute(con, sprintf("ALTER TABLE exports_%s ADD CONSTRAINT fk_exports_%s_units
  FOREIGN KEY (qty_unit_code) REFERENCES unit_codes (qty_unit_code);", target_class, target_class)), silent = TRUE)

## imports ----

try(dbExecute(con, sprintf("CREATE INDEX idx_imports_%s_year ON imports_%s (year);", target_class, target_class)), silent = TRUE)
try(dbExecute(con, sprintf("CREATE INDEX idx_imports_%s_reporter ON imports_%s (reporter_code);", target_class, target_class)), silent = TRUE)
try(dbExecute(con, sprintf("CREATE INDEX idx_imports_%s_partner ON imports_%s (partner_code);", target_class, target_class)), silent = TRUE)
try(dbExecute(con, sprintf("CREATE INDEX idx_imports_%s_commodity ON imports_%s (commodity_code);", target_class, target_class)), silent = TRUE)

try(dbExecute(con, sprintf("ALTER TABLE imports_%s ADD CONSTRAINT fk_imports_%s_country_codes
  FOREIGN KEY (reporter_code) REFERENCES country_codes (country_code);", target_class, target_class)), silent = TRUE)

try(dbExecute(con, sprintf("ALTER TABLE imports_%s ADD CONSTRAINT fk_imports_%s_country_codes_2
  FOREIGN KEY (partner_code) REFERENCES country_codes (country_code);", target_class, target_class)), silent = TRUE)

try(dbExecute(con, sprintf("ALTER TABLE imports_%s ADD CONSTRAINT fk_imports_%s_commodity_codes
  FOREIGN KEY (classification_id, commodity_code) REFERENCES commodity_codes (classification_id, commodity_code);", target_class, target_class)), silent = TRUE)

try(dbExecute(con, sprintf("ALTER TABLE imports_%s ADD CONSTRAINT fk_imports_%s_units
  FOREIGN KEY (qty_unit_code) REFERENCES unit_codes (qty_unit_code);", target_class, target_class)), silent = TRUE)

message("Done.")
