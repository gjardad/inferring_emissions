res <- readRDS("analysis/active/output/threshold_p_levels_eval_results.rds")
pred <- res$predictions
em <- pred[pred$y > 0, ]
em$apd <- abs(em$y - em$yhat) / em$y
cat("n emitters:", nrow(em), "\n")
cat("APD quantiles:\n")
print(quantile(em$apd, c(0, .1, .25, .4, .45, .5, .55, .6, .75, .9, 1)))
cat("\nemitters with yhat == 0:", sum(em$yhat == 0), "\n")
cat("emitters with apd == 1 exactly (within 1e-9):", sum(abs(em$apd - 1) < 1e-9), "\n")
cat("emitters with apd > 1:", sum(em$apd > 1 + 1e-9), "\n")
cat("emitters with apd < 1:", sum(em$apd < 1 - 1e-9), "\n")
cat("\nBy sector:\n")
for (s in c("paper","refining","metals")) {
  ss <- em[em$sector == s, ]
  cat(sprintf("  %s: n=%d  yhat0=%d  apd=1=%d  apd<1=%d  apd>1=%d  median=%.3f\n",
    s, nrow(ss), sum(ss$yhat == 0),
    sum(abs(ss$apd-1) < 1e-9),
    sum(ss$apd < 1 - 1e-9),
    sum(ss$apd > 1 + 1e-9),
    median(ss$apd)))
}
cat("\nclassified x true emit (full eval set):\n")
print(table(classified = pred$classified_emit, true_emit = pred$emit))
cat("\nAmong true emitters with yhat == 0, are they unclassified?\n")
print(table(em$classified_emit[em$yhat == 0]))
