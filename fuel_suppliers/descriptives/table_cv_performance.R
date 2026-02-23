###############################################################################
# fuel_suppliers/descriptives/table_cv_performance.R
#
# PURPOSE
#   Generate paper-ready LaTeX tables from the group k-fold CV results.
#
#   Table 1 — Model selection (cv_performance.tex):
#     Three blocks: PPML calibrated | Hurdle calibrated | Hurdle calibrated+clipped.
#     Columns: N | Extensive margin (FPR, TPR, Mass capt.) |
#              Intensive margin (nRMSE, MAPD, Spearman rho)
#     PPML models show "---" for extensive margin (no explicit classification).
#     "Calibrated + clipped" enforces that no non-emitter is imputed emissions
#     above the maximum observed ETS emitter in that sector-year.
#
#   Table 2 — False-positive severity (fp_severity.tex):
#     For each calibrated and calibrated+clipped model, how large are imputed
#     emissions for confirmed non-emitters (y=0) relative to the sector's
#     emitter distribution?  Panels for NACE 19 (coke/petroleum) and 24 (metals).
#     Entries: percentile rank within the emitter ecdf of the median/p90/p99
#     of predicted emissions among true non-emitters in that sector.
#
# INPUTS
#   {OUTPUT_DIR}/fuel_suppliers_cv_performance.csv
#
# OUTPUTS
#   {OUTPUT_DIR}/enet_cv_performance.tex
#   {OUTPUT_DIR}/enet_fp_severity.tex
###############################################################################

# ── Paths ────────────────────────────────────────────────────────────────────
if (tolower(Sys.info()[["user"]]) == "jardang") {
  REPO_DIR <- "C:/Users/jardang/Documents/inferring_emissions"
} else if (tolower(Sys.info()[["user"]]) == "jota_"){
  REPO_DIR <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile, winslash = "/")), error = function(e) normalizePath(getwd(), winslash = "/"))
  while (!file.exists(file.path(REPO_DIR, "paths.R"))) REPO_DIR <- dirname(REPO_DIR)
} else {
  stop("Define REPO_DIR for this user.")
}
source(file.path(REPO_DIR, "paths.R"))


# ── Load CV results ──────────────────────────────────────────────────────────
cv_path <- file.path(OUTPUT_DIR, "fuel_suppliers_cv_performance.csv")
if (!file.exists(cv_path)) stop("CV results not found: ", cv_path)

cv <- read.csv(cv_path, stringsAsFactors = FALSE)


# ===========================================================================
# Table 1: Model selection
#   Block A: PPML (calibrated)
#   Block B: Hurdle (calibrated)
#   Block C: Hurdle (calibrated + clipped)
# ===========================================================================

# Model keys used in the CSV
ppml_models   <- c("benchmark", "proxy_pooled", "proxy_within_buyer")
hurdle_models <- c("hurdle_benchmark", "hurdle_proxy_pooled", "hurdle_proxy_within_buyer")

ppml_labels   <- c("PPML benchmark", "PPML + pooled proxy", "PPML + within-buyer proxy")
hurdle_labels <- c("Hurdle benchmark", "Hurdle + pooled proxy", "Hurdle + within-buyer proxy")
hurdle_clip_labels <- paste0(hurdle_labels, ", clipped")

# Extract rows
ppml_cal    <- cv[cv$variant == "calibrated" & cv$model %in% ppml_models, ]
hurdle_cal  <- cv[cv$variant == "calibrated" & cv$model %in% hurdle_models, ]
hurdle_clip <- cv[cv$variant == "calibrated_clipped" & cv$model %in% hurdle_models, ]

# Order within each block
ppml_cal$model    <- factor(ppml_cal$model,    levels = ppml_models)
hurdle_cal$model  <- factor(hurdle_cal$model,  levels = hurdle_models)
hurdle_clip$model <- factor(hurdle_clip$model, levels = hurdle_models)
ppml_cal    <- ppml_cal[order(ppml_cal$model), ]
hurdle_cal  <- hurdle_cal[order(hurdle_cal$model), ]
hurdle_clip <- hurdle_clip[order(hurdle_clip$model), ]

# Formatting helpers
fmt3 <- function(x) ifelse(is.na(x), "---", sprintf("%.3f", x))
fmt_n <- function(x) ifelse(is.na(x), "---", format(as.integer(x), big.mark = ","))

# Helper: emit one row of Table 1
emit_row <- function(r, lab, show_ext) {
  if (show_ext) {
    fpr <- fmt3(r$fpr_nonemitters)
    tpr <- fmt3(r$tpr_emitters)
    mc  <- fmt3(r$emitter_mass_captured)
  } else {
    fpr <- "---"; tpr <- "---"; mc <- "---"
  }
  sprintf(
    "%s & %s & %s & %s & %s & %s & %s & %s \\\\",
    lab, fmt_n(r$n), fpr, tpr, mc,
    fmt3(r$nRMSE), fmt3(r$mapd_emitters), fmt3(r$spearman)
  )
}

# Build LaTeX
tex <- c(
  "\\begin{tabular}{l r ccc ccc}",
  "\\toprule",
  " & & \\multicolumn{3}{c}{Extensive margin} & \\multicolumn{3}{c}{Intensive margin} \\\\",
  "\\cmidrule(lr){3-5} \\cmidrule(lr){6-8}",
  "Model & $N$ & FPR & TPR & Mass capt. & nRMSE & MAPD & $\\rho$ \\\\",
  "\\midrule"
)

# Block A: PPML calibrated
for (i in seq_len(nrow(ppml_cal)))
  tex <- c(tex, emit_row(ppml_cal[i, ], ppml_labels[i], show_ext = FALSE))
tex <- c(tex, "\\addlinespace")

# Block B: Hurdle calibrated
for (i in seq_len(nrow(hurdle_cal)))
  tex <- c(tex, emit_row(hurdle_cal[i, ], hurdle_labels[i], show_ext = TRUE))

# Block C: Hurdle calibrated + clipped (only if rows exist)
if (nrow(hurdle_clip) > 0) {
  tex <- c(tex, "\\addlinespace")
  for (i in seq_len(nrow(hurdle_clip)))
    tex <- c(tex, emit_row(hurdle_clip[i, ], hurdle_clip_labels[i], show_ext = TRUE))
}

tex <- c(tex, "\\bottomrule", "\\end{tabular}")

out1 <- file.path(OUTPUT_DIR, "enet_cv_performance.tex")
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)
writeLines(tex, out1)
cat("Saved Table 1 (model selection) to:", out1, "\n")


# ===========================================================================
# Table 2: False-positive severity (NACE 19 and 24)
#   Shows calibrated AND calibrated+clipped hurdle rows.
# ===========================================================================

# Combine all rows that go into Table 2
all_rows   <- rbind(ppml_cal, hurdle_cal, hurdle_clip)
all_labels <- c(ppml_labels,
                hurdle_labels,
                if (nrow(hurdle_clip) > 0) hurdle_clip_labels else character(0))

has_fp <- any(!is.na(all_rows$nonemit_p50_rank_19)) ||
          any(!is.na(all_rows$nonemit_p50_rank_24))

if (has_fp) {

  fmt_pct <- function(x) ifelse(is.na(x), "---", sprintf("%.0f", x * 100))

  tex2 <- c(
    "\\begin{tabular}{l ccc ccc}",
    "\\toprule",
    " & \\multicolumn{3}{c}{NACE 19 (Coke \\& petroleum)} & \\multicolumn{3}{c}{NACE 24 (Basic metals)} \\\\",
    "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7}",
    "Model & $p_{50}$ & $p_{90}$ & $p_{99}$ & $p_{50}$ & $p_{90}$ & $p_{99}$ \\\\",
    "\\midrule"
  )

  n_ppml   <- nrow(ppml_cal)
  n_hurdle <- nrow(hurdle_cal)
  n_clip   <- nrow(hurdle_clip)

  for (i in seq_len(nrow(all_rows))) {
    r <- all_rows[i, ]
    lab <- all_labels[i]

    line <- sprintf(
      "%s & %s & %s & %s & %s & %s & %s \\\\",
      lab,
      fmt_pct(r$nonemit_p50_rank_19),
      fmt_pct(r$nonemit_p90_rank_19),
      fmt_pct(r$nonemit_p99_rank_19),
      fmt_pct(r$nonemit_p50_rank_24),
      fmt_pct(r$nonemit_p90_rank_24),
      fmt_pct(r$nonemit_p99_rank_24)
    )
    tex2 <- c(tex2, line)

    # Visual breaks between blocks
    if (i == n_ppml || i == n_ppml + n_hurdle)
      tex2 <- c(tex2, "\\addlinespace")
  }

  tex2 <- c(tex2, "\\bottomrule", "\\end{tabular}")

  out2 <- file.path(OUTPUT_DIR, "enet_fp_severity.tex")
  writeLines(tex2, out2)
  cat("Saved Table 2 (false-positive severity) to:", out2, "\n")

} else {
  cat("Skipping Table 2: no sector-specific false-positive severity data available.\n")
}
