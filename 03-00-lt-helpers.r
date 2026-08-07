# Shared helpers for the data-driven Lukaszuk-Torun (LT) classification
# harmonization pipeline.
#
# Sourced by:
#   03a-compute-lt-weights.r  (compute / cache per-pair weight tables)
#   03b-convert-flows.r       (apply chained weights to bilateral flows)
#
# Reference: Bustos et al. (2026, Sci. Data); reference implementation at
# https://github.com/harvard-growth-lab/comtrade-conversion-weights.

suppressPackageStartupMessages({
  library(RPostgres)
  library(data.table)
  library(igraph)
  library(Matrix)
  library(quadprog)
})

con <- dbConnect(
  RPostgres::Postgres(),
  dbname = Sys.getenv("COMTRADE_NAME"),
  host = Sys.getenv("COMTRADE_HOST"),
  user = Sys.getenv("COMTRADE_USER"),
  password = Sys.getenv("COMTRADE_PASSWORD"),
  port = Sys.getenv("COMTRADE_PORT")
)

# Adjacency graph between classification vintages. Each edge is c(older, newer).
adj_edges <- list(
  c("h0", "h1"), c("h1", "h2"), c("h2", "h3"),
  c("h3", "h4"), c("h4", "h5"), c("h5", "h6"),
  c("s1", "s2"), c("s2", "s3"), c("s3", "s4"),
  c("s3", "h0")
)

# Release year of each vintage (anchor t1 in the LT optimization).
release_year <- c(
  h0 = 1992, h1 = 1996, h2 = 2002, h3 = 2007,
  h4 = 2012, h5 = 2017, h6 = 2022,
  s1 = 1962, s2 = 1976, s3 = 1988, s4 = 2007
)

adj_graph <- igraph::graph_from_edgelist(
  do.call(rbind, adj_edges),
  directed = FALSE
)

# Load classification_codes once and cache.
load_classification_codes <- function(con) {
  d_codes <- as.data.table(dbGetQuery(con, "SELECT * FROM classification_codes"))
  d_codes[, classification_code := tolower(classification_code)]
  list(
    d_codes = d_codes,
    lookup  = setNames(d_codes$classification_id, d_codes$classification_code)
  )
}

# Pull import totals (V_{i,k}) for a given classification at a given year,
# aggregated to `digits`, grouped by reporter and code.
get_class_year_totals <- function(con, cls, yr, digits, class_id_lookup) {
  cid <- class_id_lookup[[cls]]
  if (is.null(cid)) return(data.table())
  d <- as.data.table(dbGetQuery(con, paste0(
    "SELECT reporter_code, commodity_code, cifvalue FROM imports",
    " WHERE year = ", yr, " AND classification_id = ", cid
  )))
  d[, commodity_code := substr(commodity_code, 1L, digits)]
  d <- d[!is.na(commodity_code) & commodity_code != ""]
  d <- d[, .(V = sum(cifvalue, na.rm = TRUE)), by = .(reporter_code, commodity_code)]
  d[V > 0]
}

# Solve a single LT-group QP.
#
# Input:
#   X       : I x K reporter-by-source trade matrix.
#   Y       : I x S reporter-by-target trade matrix.
#   allowed : K x S logical mask of WCO-permitted cells.
#
# Decision variables are the *allowed* cells only (vector beta of length
# P = sum(allowed)) — this avoids the KS x KS dense Hessian and the
# O(KS - P) equality constraints used to zero out disallowed cells, which
# makes the problem tractable for large groups (e.g. h5<->h6 at 4 digits).
#
# Per-target-column decoupling: the prediction is hat{Y}[, s] = X[, k in
# allowed_s] %*% beta_s, so the Hessian is block-diagonal across s with
# blocks t(X_s) %*% X_s.
solve_group_qp <- function(X, Y, allowed) {
  K <- ncol(X); S <- ncol(Y); I <- nrow(X)

  # Helper: equal-weight fallback over allowed cells.
  fallback <- function() {
    B <- matrix(0, K, S)
    for (k in seq_len(K)) {
      ok <- which(allowed[k, , drop = TRUE])
      if (length(ok) > 0) {
        B[k, ok] <- 1 / length(ok)
      } else {
        B[k, ] <- 1 / S
      }
    }
    B
  }

  if (I == 0L || all(X == 0) || all(Y == 0)) return(fallback())

  # Index allowed cells.
  idx <- which(allowed, arr.ind = TRUE)
  P <- nrow(idx)
  if (P == 0L) return(fallback())

  # Group variable indices by target column s; build per-block Hessians.
  s_of <- idx[, 2]
  k_of <- idx[, 1]
  ord <- order(s_of, k_of)
  idx <- idx[ord, , drop = FALSE]
  s_of <- idx[, 2]
  k_of <- idx[, 1]

  # Variable position p -> (k, s); var_pos_in_var maps cell to its variable
  # index for constraint construction.
  var_pos <- integer(K * S)
  var_pos[(s_of - 1L) * K + k_of] <- seq_len(P)

  # Build block-diagonal Hessian Dmat (P x P).
  blocks <- vector("list", S)
  dvec  <- numeric(P)
  for (s in seq_len(S)) {
    sel <- which(s_of == s)
    if (length(sel) == 0L) {
      blocks[[s]] <- matrix(0, 0, 0)
      next
    }
    Xs <- X[, k_of[sel], drop = FALSE]
    blocks[[s]] <- 2 * crossprod(Xs)
    dvec[sel]   <- 2 * as.vector(crossprod(Xs, Y[, s, drop = FALSE]))
  }
  Dmat <- as.matrix(Matrix::bdiag(blocks))
  # Tikhonov regularization for strict positive-definiteness.
  diag(Dmat) <- diag(Dmat) + 1e-6

  # Equality: row sums = 1 (one per source code k that has at least one
  # allowed target). Sources with no allowed cell get an even split fallback
  # later (they contribute nothing to the QP).
  active_k <- sort(unique(k_of))
  nK <- length(active_k)
  A_sum <- matrix(0, P, nK)
  for (j in seq_along(active_k)) {
    rows <- which(k_of == active_k[j])
    A_sum[rows, j] <- 1
  }
  bvec_sum <- rep(1, nK)

  # Inequality: each beta_p >= 0.
  A_pos <- diag(1, P)
  bvec_pos <- rep(0, P)

  Amat <- cbind(A_sum, A_pos)
  bvec <- c(bvec_sum, bvec_pos)
  meq  <- nK

  sol <- tryCatch(
    quadprog::solve.QP(Dmat, dvec, Amat, bvec, meq = meq),
    error = function(e) {
      message("  QP failed: ", conditionMessage(e), " — using equal weights")
      NULL
    }
  )
  if (is.null(sol)) return(fallback())

  beta <- pmax(sol$solution, 0)
  B <- matrix(0, K, S)
  B[cbind(k_of, s_of)] <- beta

  # Fill rows that had no allowed cell (shouldn't happen given group def).
  rs <- rowSums(B)
  zero_rows <- which(rs == 0)
  for (k in zero_rows) {
    ok <- which(allowed[k, , drop = TRUE])
    if (length(ok) > 0) {
      B[k, ok] <- 1 / length(ok)
    } else {
      B[k, ] <- 1 / S
    }
  }
  rs <- rowSums(B)
  rs[rs == 0] <- 1
  B / rs
}

# Compute LT conversion weights for an adjacent pair (A older, B newer).
# Returns a list with $forward (A -> B) and $backward (B -> A), each a
# tibble(from, to, weight).
compute_pair_weights <- function(con, A, B, digits, class_id_lookup) {
  stopifnot(release_year[[B]] >= release_year[[A]])
  t1 <- release_year[[B]]
  t0 <- t1 - 1L

  message(sprintf("LT weights: %s (year %d) <-> %s (year %d)", A, t0, B, t1))

  # 1. Correlation between the two vintages.
  d_corr <- as.data.table(dbGetQuery(con, paste0(
    "SELECT \"", A, "\" AS a, \"", B, "\" AS b FROM commodity_correlations"
  )))
  d_corr <- d_corr[!is.na(a) & !is.na(b) & a != "" & b != ""]
  d_corr[, `:=`(a = substr(a, 1L, digits), b = substr(b, 1L, digits))]
  d_corr <- unique(d_corr)

  if (nrow(d_corr) == 0) {
    return(list(
      forward  = data.table(from = character(), to = character(), weight = double()),
      backward = data.table(from = character(), to = character(), weight = double())
    ))
  }

  # 2. Connected components ("groups") on the bipartite a-b graph.
  d_corr[, `:=`(a_node = paste0("A:", a), b_node = paste0("B:", b))]
  g <- igraph::graph_from_data_frame(
    d_corr[, c("a_node", "b_node")], directed = FALSE
  )
  comps <- igraph::components(g)
  d_corr[, group := comps$membership[a_node]]

  # 3. Timely-reporter import totals at the two anchor years.
  totals_A <- get_class_year_totals(con, A, t0, digits, class_id_lookup)
  totals_B <- get_class_year_totals(con, B, t1, digits, class_id_lookup)
  timely <- intersect(unique(totals_A$reporter_code), unique(totals_B$reporter_code))
  if (length(timely) > 0) {
    totals_A <- totals_A[reporter_code %in% timely]
    totals_B <- totals_B[reporter_code %in% timely]
  }

  fwd_list <- list()
  bwd_list <- list()
  group_ids <- sort(unique(d_corr$group))
  n_groups <- length(group_ids)

  for (gi in seq_along(group_ids)) {
    gid <- group_ids[gi]
    rows <- d_corr[group == gid]
    a_codes <- sort(unique(rows$a))
    b_codes <- sort(unique(rows$b))
    K <- length(a_codes); S <- length(b_codes)

    if (gi %% 50L == 0L || K * S > 500) {
      message(sprintf("  group %d/%d  (K=%d, S=%d)", gi, n_groups, K, S))
    }

    allowed <- matrix(FALSE, K, S, dimnames = list(a_codes, b_codes))
    allowed[cbind(match(rows$a, a_codes), match(rows$b, b_codes))] <- TRUE

    tA <- totals_A[commodity_code %in% a_codes]
    tB <- totals_B[commodity_code %in% b_codes]
    rep_set <- intersect(unique(tA$reporter_code), unique(tB$reporter_code))

    if (length(rep_set) == 0L) {
      X <- matrix(0, 0, K, dimnames = list(NULL, a_codes))
      Y <- matrix(0, 0, S, dimnames = list(NULL, b_codes))
    } else {
      tA <- tA[reporter_code %in% rep_set]
      tB <- tB[reporter_code %in% rep_set]
      X <- matrix(0, length(rep_set), K, dimnames = list(rep_set, a_codes))
      Y <- matrix(0, length(rep_set), S, dimnames = list(rep_set, b_codes))
      X[cbind(match(tA$reporter_code, rep_set), match(tA$commodity_code, a_codes))] <- tA$V
      Y[cbind(match(tB$reporter_code, rep_set), match(tB$commodity_code, b_codes))] <- tB$V
      sX <- sum(X); sY <- sum(Y)
      if (sX > 0) X <- X / sX
      if (sY > 0) Y <- Y / sY
    }

    Bfwd <- solve_group_qp(X, Y, allowed)
    Bbwd <- solve_group_qp(Y, X, t(allowed))

    fwd_list[[length(fwd_list) + 1]] <- data.table(
      from   = rep(a_codes, times = S),
      to     = rep(b_codes, each  = K),
      weight = as.vector(Bfwd)
    )[weight > 0]

    bwd_list[[length(bwd_list) + 1]] <- data.table(
      from   = rep(b_codes, times = K),
      to     = rep(a_codes, each  = S),
      weight = as.vector(Bbwd)
    )[weight > 0]
  }

  list(
    forward  = rbindlist(fwd_list),
    backward = rbindlist(bwd_list)
  )
}

# Compose two weight tables: (from1->to1) %*% (from2->to2).
compose_weights <- function(w1, w2) {
  if (nrow(w1) == 0 || nrow(w2) == 0) {
    return(data.table(from = character(), to = character(), weight = double()))
  }
  w1 <- as.data.table(w1)
  w2 <- as.data.table(w2)
  merged <- merge(w1, w2, by.x = "to", by.y = "from", allow.cartesian = TRUE)
  merged[, weight := weight.x * weight.y]
  result <- merged[, .(weight = sum(weight)), by = .(from, to = to.y)]
  result[weight > 0]
}

# Build src -> tgt weight table by chaining adjacent-pair weights along the
# shortest path in `adj_graph`.
build_chain_weights <- function(src, tgt, pair_weights) {
  if (src == tgt) {
    return(data.table(from = character(), to = character(), weight = double()))
  }
  path <- igraph::shortest_paths(adj_graph, from = src, to = tgt)$vpath[[1]]
  path <- names(path)
  if (length(path) < 2) {
    stop(sprintf("No conversion path found from '%s' to '%s'.", src, tgt))
  }

  acc <- NULL
  for (i in seq_len(length(path) - 1)) {
    a <- path[i]; b <- path[i + 1]
    if (release_year[[a]] <= release_year[[b]]) {
      key <- paste0(a, "_", b); direction <- "forward"
    } else {
      key <- paste0(b, "_", a); direction <- "backward"
    }
    step <- pair_weights[[key]][[direction]]
    if (is.null(step) || nrow(step) == 0) {
      stop(sprintf("Missing %s weights for step %s -> %s.", direction, a, b))
    }
    acc <- if (is.null(acc)) step else compose_weights(acc, step)
  }
  acc
}

# Apply a (from, to, weight) conversion table to trade data. Trade rows whose
# code is not in the weight table get assigned to "9999" (unmatched) with
# weight 1, preserving the value.
apply_weighted_conversion <- function(d_trade, weights, value_cols) {
  d_trade <- as.data.table(d_trade)
  by_cols <- c("year", "reporter_code", "partner_code", "commodity_code")
  if ("qty_unit_code" %in% names(d_trade)) by_cols <- c(by_cols, "qty_unit_code")

  if (nrow(weights) == 0) {
    d_trade[, commodity_code := fifelse(is.na(commodity_code), "9999", commodity_code)]
    return(d_trade[
      , lapply(.SD, sum, na.rm = TRUE),
      by = by_cols,
      .SDcols = value_cols
    ])
  }
  weights <- as.data.table(weights)
  merged <- merge(d_trade, weights, by.x = "commodity_code", by.y = "from",
                  all.x = TRUE, allow.cartesian = TRUE)
  merged[, to     := fifelse(is.na(to),     "9999", to)]
  merged[, weight := fifelse(is.na(weight), 1,      weight)]
  for (vc in value_cols) {
    merged[, (vc) := get(vc) * weight]
  }
  by_out <- c(setdiff(by_cols, "commodity_code"), "to")
  result <- merged[
    , lapply(.SD, sum, na.rm = TRUE),
    by = by_out,
    .SDcols = value_cols
  ]
  setnames(result, "to", "commodity_code")
  result
}

# Path layout ----
# LT_WEIGHTS_DIR <- "output/lt_weights"

pair_key <- function(A, B) {
  if (release_year[[A]] <= release_year[[B]]) paste0(A, "_", B) else paste0(B, "_", A)
}

# pair_path <- function(A, B, digits) {
#   file.path(LT_WEIGHTS_DIR, sprintf("%s_%dd.rds", pair_key(A, B), digits))
# }

# Load all available cached pair weights into a single named list.
load_all_pair_weights <- function(digits) {
  out <- list()
  for (e in adj_edges) {
    A <- e[1]; B <- e[2]
    
    # p <- pair_path(A, B, digits)

    p <- pair_key(A, B)
    
    if (dbExistsTable(con, paste0(p, "_fwd")) && dbExistsTable(con, paste0(p, "_bwd"))) {
      # out[[pair_key(A, B)]] <- readRDS(p)
      out[[pair_key(A, B)]] <- list(
        forward  = as.data.table(dbReadTable(con, paste0(pair_key(A, B), "_fwd"))),
        backward = as.data.table(dbReadTable(con, paste0(pair_key(A, B), "_bwd")))
      )
    }
  }
  out
}
