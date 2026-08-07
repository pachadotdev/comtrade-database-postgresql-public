# Compute and cache LT conversion weights for adjacent classification pairs.
#
# Each pair is saved to its own RDS file under output/lt_weights/, so this
# script can be stopped/resumed and pairs can be (re)computed individually.
#
# Usage:
#   Rscript 03a-compute-lt-weights.r            # all missing pairs
#   Rscript 03a-compute-lt-weights.r h5 h6      # only the h5<->h6 pair
#   PAIRS="h5_h6,h4_h5" Rscript 03a-compute-lt-weights.r

source("03-00-lt-helpers.r")

on.exit(try(dbDisconnect(con), silent = TRUE), add = TRUE)

digits <- Sys.getenv("COMTRADE_HS_DIGITS")
if (!any(digits %in% c(4L,6L))) stop("Digits undefined (use 4 or 6)")

# dir.create(LT_WEIGHTS_DIR, showWarnings = FALSE, recursive = TRUE)

# Decide which pairs to compute.
args <- commandArgs(trailingOnly = TRUE)
env_pairs <- Sys.getenv("PAIRS", unset = "")

selected <- NULL
if (length(args) == 2L) {
  selected <- list(c(args[1], args[2]))
} else if (nzchar(env_pairs)) {
  toks <- strsplit(env_pairs, "[, ]+")[[1]]
  toks <- toks[nzchar(toks)]
  selected <- lapply(toks, function(t) {
    parts <- strsplit(t, "_")[[1]]
    if (length(parts) != 2L) stop("Bad pair spec: ", t)
    parts
  })
} else {
  selected <- adj_edges
}

cls <- load_classification_codes(con)

for (e in selected) {
  A <- e[1]; B <- e[2]
  if (release_year[[A]] > release_year[[B]]) { tmp <- A; A <- B; B <- tmp }

  # out_path <- pair_path(A, B, digits)
  tbl_name <- pair_key(A, B)
  tbl_name_fwd <- paste0(tbl_name, "_fwd")
  tbl_name_bwd <- paste0(tbl_name, "_bwd")
  
  # if (file.exists(out_path)) {
  #   message("[skip] ", out_path, " already exists")
  #   next
  # }

  if (dbExistsTable(con, tbl_name_fwd) && dbExistsTable(con, tbl_name_bwd)) {
    message("[skip] ", tbl_name, " already exists in database")
    next
  }

  message("==> Computing pair ", A, " <-> ", B, " (digits=", digits, ")")
  t0 <- Sys.time()
  pw <- compute_pair_weights(con, A, B, digits, cls$lookup)
  message(sprintf("    elapsed: %s",
                  format(round(Sys.time() - t0, 1))))

  # saveRDS(pw, out_path, compress = "xz")
  
  # dbWriteTable(con, out_path, pw, append = TRUE, row.names = FALSE)
  dbWriteTable(con, tbl_name_fwd, pw$forward, overwrite = TRUE, row.names = FALSE)
  dbWriteTable(con, tbl_name_bwd, pw$backward, overwrite = TRUE, row.names = FALSE)

  message("    saved -> ", tbl_name)

  # Aggressive cleanup so each pair runs in roughly the same memory.
  rm(pw); gc(verbose = FALSE)
}

message("Done.")
