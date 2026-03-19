load("C:/Users/jota_/Documents/NBB_data/processed/repeated_cv_proxy_sector_asinh.RData")
load("C:/Users/jota_/Documents/NBB_data/processed/firm_year_panel_with_proxies.RData")
panel <- repeated_cv_proxy_panel
panel <- merge(panel, training_sample[, c("vat", "year", "revenue")], by = c("vat", "year"))

ne <- panel[panel$nace2d %in% c("17", "18") & panel$y == 0, ]
cat("Non-emitters in 17/18:", nrow(ne), "\n")
cat("Revenue == 0:", sum(ne$revenue == 0), "\n")
cat("Revenue > 0:", sum(ne$revenue > 0), "\n")
cat("Revenue is NA:", sum(is.na(ne$revenue)), "\n")
cat("Fraction with revenue > 0:", mean(ne$revenue > 0, na.rm = TRUE), "\n")
cat("\nRevenue distribution among non-emitters:\n")
print(summary(ne$revenue))
cat("\nRevenue quantiles:\n")
print(quantile(ne$revenue, probs = c(0, 0.01, 0.05, 0.1, 0.5, 0.9, 1), na.rm = TRUE))
