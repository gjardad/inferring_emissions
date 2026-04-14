DATA_DIR <- "c:/Users/jota_/Documents/NBB_data"
load(file.path(DATA_DIR, "processed", "imjv_eutl_match.RData"))
unmatched <- imjv_eutl[imjv_eutl$matched == FALSE, ]
cat(unmatched$imjv_name, sep = "\n")
