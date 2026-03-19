###############################################################################
# Scratch: compute MAPD among emitters for sectors 17/18, 19, 24
# Revenue and Elastic Net only, both CV designs
###############################################################################

REPO_DIR <- tryCatch(
  dirname(normalizePath(sys.frame(1)$ofile, winslash = "/")),
  error = function(e) normalizePath(getwd(), winslash = "/")
)
while (!file.exists(file.path(REPO_DIR, "paths.R"))) REPO_DIR <- dirname(REPO_DIR)
source(file.path(REPO_DIR, "paths.R"))

library(dplyr)
source(file.path(UTILS_DIR, "calibration_helpers.R"))

BASE_SEED <- 2026L
K_sec     <- 5L
K_firm    <- 5L

SECTOR_GROUPS <- list("17/18" = c("17", "18"), "19" = "19", "24" = "24")

# ── Load data ────────────────────────────────────────────────────────────────
e_sec <- new.env()
load(file.path(PROC_DATA, "repeated_cv_proxy_sector_asinh.RData"), envir = e_sec)
proxy_matrix_sec <- e_sec$proxy_matrix
panel <- e_sec$repeated_cv_proxy_panel
M_sec <- ncol(proxy_matrix_sec)
rm(e_sec)

e_firm <- new.env()
load(file.path(PROC_DATA, "repeated_cv_proxy_firm_asinh.RData"), envir = e_firm)
proxy_matrix_firm <- e_firm$proxy_matrix
M_firm <- ncol(proxy_matrix_firm)
rm(e_firm)

load(file.path(PROC_DATA, "firm_year_panel_with_proxies.RData"))
panel <- panel %>% left_join(
  training_sample %>% select(vat, year, revenue),
  by = c("vat", "year")
)
rm(training_sample, syt)

M <- min(M_sec, M_firm)

# ── Helper: MAPD among emitters for one sector group ─────────────────────────
mapd_emitters <- function(y, yhat, nace2d, group_codes) {
  idx <- which(nace2d %in% group_codes & y > 0)
  if (length(idx) < 1) return(NA_real_)
  median(abs(y[idx] - yhat[idx]) / y[idx])
}

# ── Panel A: Sector-level CV ─────────────────────────────────────────────────
cat("Panel A: Sector-level CV\n")

# Revenue (deterministic)
fold_k_A <- assign_folds(panel, "sector", K_sec, BASE_SEED + 1L)
yhat_rev_A <- calibrate_sector(panel, panel$revenue, fold_k_A)

for (g in names(SECTOR_GROUPS)) {
  val <- mapd_emitters(panel$y, yhat_rev_A, panel$nace2d, SECTOR_GROUPS[[g]])
  cat(sprintf("  Revenue  NACE %-5s  MAPD = %.3f\n", g, val))
}

# EN (M repeats)
mapd_A_en <- matrix(NA_real_, nrow = M, ncol = length(SECTOR_GROUPS),
                    dimnames = list(NULL, names(SECTOR_GROUPS)))
for (r in seq_len(M)) {
  fold_k_r <- assign_folds(panel, "sector", K_sec, BASE_SEED + r)
  en_levels_r <- proxy_to_levels(proxy_matrix_sec[, r])
  yhat_r <- calibrate_sector(panel, en_levels_r, fold_k_r)
  for (g in names(SECTOR_GROUPS)) {
    mapd_A_en[r, g] <- mapd_emitters(panel$y, yhat_r, panel$nace2d, SECTOR_GROUPS[[g]])
  }
  if (r %% 50 == 0) cat(sprintf("    %d/%d\n", r, M))
}
cat("  Elastic Net:\n")
for (g in names(SECTOR_GROUPS)) {
  cat(sprintf("    NACE %-5s  MAPD = %.3f (sd %.3f)\n",
              g, mean(mapd_A_en[, g], na.rm = TRUE), sd(mapd_A_en[, g], na.rm = TRUE)))
}

# ── Panel B: Firm-level CV ───────────────────────────────────────────────────
cat("\nPanel B: Firm-level CV\n")

mapd_B_rev <- matrix(NA_real_, nrow = M, ncol = length(SECTOR_GROUPS),
                     dimnames = list(NULL, names(SECTOR_GROUPS)))
mapd_B_en  <- matrix(NA_real_, nrow = M, ncol = length(SECTOR_GROUPS),
                     dimnames = list(NULL, names(SECTOR_GROUPS)))

for (r in seq_len(M)) {
  fold_k_r <- assign_folds(panel, "firm", K_firm, BASE_SEED + r)
  en_levels_r <- proxy_to_levels(proxy_matrix_firm[, r])

  yhat_rev_r <- calibrate_sector(panel, panel$revenue, fold_k_r)
  yhat_en_r  <- calibrate_sector(panel, en_levels_r, fold_k_r)

  for (g in names(SECTOR_GROUPS)) {
    mapd_B_rev[r, g] <- mapd_emitters(panel$y, yhat_rev_r, panel$nace2d, SECTOR_GROUPS[[g]])
    mapd_B_en[r, g]  <- mapd_emitters(panel$y, yhat_en_r,  panel$nace2d, SECTOR_GROUPS[[g]])
  }
  if (r %% 50 == 0) cat(sprintf("    %d/%d\n", r, M))
}

cat("  Revenue:\n")
for (g in names(SECTOR_GROUPS)) {
  cat(sprintf("    NACE %-5s  MAPD = %.3f (sd %.3f)\n",
              g, mean(mapd_B_rev[, g], na.rm = TRUE), sd(mapd_B_rev[, g], na.rm = TRUE)))
}
cat("  Elastic Net:\n")
for (g in names(SECTOR_GROUPS)) {
  cat(sprintf("    NACE %-5s  MAPD = %.3f (sd %.3f)\n",
              g, mean(mapd_B_en[, g], na.rm = TRUE), sd(mapd_B_en[, g], na.rm = TRUE)))
}

cat("\nDone.\n")
