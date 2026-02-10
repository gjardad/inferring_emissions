###############################################################################
# number_of_positive_obs_by_fuel_proxy.R
#
# Summarize cached proxies (02_proxies/cache/*.rds) restricted to the LOOCV
# training sample (loocv_training_sample). Produces one row per proxy with
# overall and euets-grouped counts of zeros / positives and other stats.
#
# Usage: source() after setting LOOCV_ROOT or sourcing your paths.R, or run in
# an interactive session where loocv_training_sample is available.
#
###############################################################################

# ====================
# Define paths ------------------
# ====================

if (tolower(Sys.info()[["user"]]) == "jardang") {
  REPO_DIR <- "C:/Users/jardang/Documents/inferring_emissions"
} else if (tolower(Sys.info()[["user"]]) == "jota_"){
  REPO_DIR <- "C:/Users/jota_/Documents/inferring_emissions"
} else {
  stop("Define REPO_DIR for this user.")
}
source(file.path(REPO_DIR, "config", "paths.R"))



library(data.table)
library(jsonlite)   # for serializing mods (optional)

# -----------------------
# CONFIG - adjust if needed
# -----------------------
# If you already source your repo paths earlier, LOOCV_ROOT should exist.
# Otherwise set LOOCV_ROOT manually, or change cache_dir and loocv_path below.
if (!exists("LOOCV_ROOT")) {
  if (file.exists("00_config/paths.R")) {
    source("00_config/paths.R")    # this is how 02_build_proxies.R sets LOOCV_ROOT
  }
}

# default cache dir relative to LOOCV_ROOT
cache_dir <- if (exists("LOOCV_ROOT")) file.path(LOOCV_ROOT, "02_proxies", "cache") else "02_proxies/cache"

# default path to saved loocv training sample (adjust if you save elsewhere)
loocv_rdata_candidates <- c(
  if (exists("PROC_DATA")) file.path(PROC_DATA, "loocv_training_sample.RData") else NULL,
  file.path("01_preprocess", "loocv_training_sample.RData"),
  file.path("data", "processed", "loocv_training_sample.RData")
)
loocv_rdata_candidates <- loocv_rdata_candidates[file.exists(loocv_rdata_candidates)]

if (length(loocv_rdata_candidates) == 0 && !exists("loocv_training_sample")) {
  stop("Could not find loocv_training_sample.RData. Either load loocv_training_sample in the R session or place the file in one of the candidate locations and re-run.\nCandidate locations: ",
       paste(c("PROC_DATA/... (if PROC_DATA exists)", "01_preprocess/loocv_training_sample.RData", "data/processed/loocv_training_sample.RData"), collapse = "; "))
}

# load loocv_training_sample if not in env already
if (!exists("loocv_training_sample")) {
  load(loocv_rdata_candidates[1])   # creates loocv_training_sample
}
if (!exists("loocv_training_sample")) stop("loocv_training_sample not found after loading RData.")

# -----------------------
# Summary statistic functions - modify/add here
# each function receives numeric vector x (fuel_proxy) and returns a single number
# -----------------------
default_stats <- list(
  total_obs = function(x) sum(!is.na(x)),
  n_zero   = function(x) sum(x == 0, na.rm = TRUE),
  n_positive = function(x) sum(x > 0, na.rm = TRUE),
  pct_positive = function(x) {
    tot <- sum(!is.na(x)); if (tot==0) NA_real_ else mean(x > 0, na.rm = TRUE)
  },
  mean_proxy = function(x) mean(x, na.rm = TRUE),
  median_proxy = function(x) stats::median(x, na.rm = TRUE)
)

# -----------------------
# Helper to compute a named vector of stats given numeric vector x
# -----------------------
compute_stats <- function(x, stats_funs = default_stats) {
  sapply(stats_funs, function(f) {
    # protect against errors in user-supplied f
    v <- tryCatch(f(x), error = function(e) NA_real_)
    # coerce logicals/ints to numeric for tidy storage
    if (is.logical(v)) v <- as.numeric(v)
    v
  }, USE.NAMES = TRUE)
}

# -----------------------
# Load list of proxy .rds files
# -----------------------
rds_files <- list.files(cache_dir, pattern = "^proxy_.*\\.rds$", full.names = TRUE)
if (length(rds_files) == 0) stop("No .rds files found in cache_dir: ", cache_dir)

# Prepare loocv data.table
loocv_dt <- as.data.table(loocv_training_sample)
# ensure vat/year/euets are present
if (!("vat" %in% names(loocv_dt))) stop("'vat' column missing from loocv_training_sample")
if (!("year" %in% names(loocv_dt))) stop("'year' column missing from loocv_training_sample")
if (!("euets" %in% names(loocv_dt))) {
  # if no euets, create NA so grouping still works
  loocv_dt[, euets := NA_integer_]
}

# normalize types for joining
loocv_dt[, buyer_id := as.character(vat)]
loocv_dt[, year := as.integer(year)]

# result list
res_list <- vector("list", length(rds_files))

# iterate proxies
for (i in seq_along(rds_files)) {
  f <- rds_files[i]
  x <- readRDS(f)    # x is list(name, mods, proxy)
  pname <- x$name
  mods <- x$mods
  pdt <- as.data.table(x$proxy)   # should include buyer_id, year, fuel_proxy
  
  # normalize proxy table
  if (!("buyer_id" %in% names(pdt))) stop("proxy file ", f, " missing buyer_id column")
  if (!("year" %in% names(pdt))) stop("proxy file ", f, " missing year column")
  if (!("fuel_proxy" %in% names(pdt))) stop("proxy file ", f, " missing fuel_proxy column")
  pdt[, buyer_id := as.character(buyer_id)]
  pdt[, year := as.integer(year)]
  
  # join proxy into LOOCV training sample (left join: keep all loocv rows)
  merged <- merge(loocv_dt, pdt[, .(buyer_id, year, fuel_proxy)], by = c("buyer_id", "year"), all.x = TRUE, sort = FALSE)
  
  # compute overall stats
  overall_stats <- compute_stats(merged$fuel_proxy, default_stats)
  names(overall_stats) <- paste0("overall_", names(overall_stats))
  
  # compute stats by euets groups (we expect 0/1)
  euets_levels <- sort(unique(merged$euets))
  euets_stats <- list()
  for (lev in euets_levels) {
    sub <- merged[euets == lev]
    prefix <- paste0("euets", ifelse(is.na(lev), "NA", lev), "_")
    s <- compute_stats(sub$fuel_proxy, default_stats)
    names(s) <- paste0(prefix, names(s))
    euets_stats <- c(euets_stats, as.list(s))
  }
  # If some expected groups absent, you can still create NA columns for 0/1 groups:
  if (!0 %in% euets_levels) {
    na_cols <- paste0("euets0_", names(default_stats))
    euets_stats[na_cols] <- rep(NA_real_, length(na_cols))
  }
  if (!1 %in% euets_levels) {
    na_cols <- paste0("euets1_", names(default_stats))
    euets_stats[na_cols] <- rep(NA_real_, length(na_cols))
  }
  
  # unpack mods into flat columns (optional; safe-serialize to JSON also included)
  mods_flat <- list()
  if (!is.null(mods) && length(mods) > 0) {
    # try to turn scalar entries into columns
    for (nm in names(mods)) {
      val <- mods[[nm]]
      if (length(val) == 1 && (is.atomic(val) || is.character(val))) {
        mods_flat[[paste0("mod_", nm)]] <- val
      } else {
        # complex entry: serialize to JSON string
        mods_flat[[paste0("mod_", nm)]] <- toJSON(val, auto_unbox = TRUE)
      }
    }
  }
  
  # collect summary row
  row <- c(
    list(proxy_name = pname,
         rds_file = f,
         n_loocv_rows = nrow(merged)),
    as.list(overall_stats),
    euets_stats,
    mods_flat
  )
  res_list[[i]] <- as.data.table(row)
}

# combine
summary_dt <- rbindlist(res_list, fill = TRUE)

# order columns nicely: proxy_name first
setcolorder(summary_dt, c("proxy_name", setdiff(names(summary_dt), "proxy_name")))

# Save outputs
out_dir <- file.path(cache_dir, "..", "summary")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
f_csv <- file.path(out_dir, "proxies_summary_by_loocv.csv")
f_rds <- file.path(out_dir, "proxies_summary_by_loocv.rds")
f_json <- file.path(out_dir, "proxies_summary_by_loocv.json")

fwrite(summary_dt, f_csv)
saveRDS(summary_dt, f_rds)
write_json(as.list(summary_dt), f_json, pretty = TRUE, auto_unbox = TRUE)

message("Wrote summary: ", f_csv, " and ", f_rds)

# -----------------------
# Quick interactive view
# -----------------------
print(head(summary_dt))