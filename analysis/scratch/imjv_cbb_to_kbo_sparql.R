# =============================================================================
# Query CBB SPARQL endpoint to get KBO numbers for all IMJV CBB numbers
# =============================================================================

library(httr)
library(jsonlite)

DATA_DIR <- "c:/Users/jota_/Documents/NBB_data"

# Load IMJV CBB numbers
imjv <- read.delim(file.path(DATA_DIR, "raw/IMJV/imjv_co2_lucht.tsv"),
                   colClasses = "character")
cbbs <- unique(imjv$cbb_number)
cat("Unique CBB numbers to query:", length(cbbs), "\n\n")

# Build VALUES clause with typed literals
values_str <- paste0('"', cbbs, '"^^<http://www.w3.org/2001/XMLSchema#string>',
                     collapse = " ")

query <- paste0('
SELECT ?cbb ?kbo ?exploitant_label ?exploitatie_label WHERE {
  VALUES ?cbb { ', values_str, ' }
  ?exploitatie <http://purl.org/dc/elements/1.1/identifier> ?cbb .
  ?exploitatie <http://www.w3.org/ns/org#siteOf> ?exploitant .
  ?exploitant <http://www.w3.org/ns/org#identifier> ?kbo .
  OPTIONAL { ?exploitant <http://www.w3.org/2000/01/rdf-schema#label> ?exploitant_label . }
  OPTIONAL { ?exploitatie <http://www.w3.org/2000/01/rdf-schema#label> ?exploitatie_label . }
}
')

cat("Sending SPARQL query...\n")

response <- POST(
  "https://data.cbb.omgeving.vlaanderen.be/sparql",
  body = list(query = query),
  encode = "form",
  add_headers(Accept = "application/sparql-results+json"),
  timeout(120)
)

cat("Status:", status_code(response), "\n")

if (status_code(response) == 200) {
  result <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
  bindings <- result$results$bindings

  if (length(bindings) > 0 && nrow(bindings) > 0) {
    df <- data.frame(
      cbb = bindings$cbb$value,
      kbo = bindings$kbo$value,
      exploitant = if (!is.null(bindings$exploitant_label)) bindings$exploitant_label$value else NA,
      exploitatie = if (!is.null(bindings$exploitatie_label)) bindings$exploitatie_label$value else NA,
      stringsAsFactors = FALSE
    )

    cat("\nResults:", nrow(df), "rows\n")
    cat("Unique CBBs matched:", length(unique(df$cbb)), "/", length(cbbs), "\n\n")

    # Show unique mappings
    unique_df <- df[!duplicated(df$cbb), ]
    cat("=== CBB -> KBO mapping (unique CBBs) ===\n")
    print(unique_df, row.names = FALSE)

    # Save
    write.csv(df, file.path(DATA_DIR, "processed", "imjv_cbb_to_kbo.csv"),
              row.names = FALSE)
    cat("\nSaved to:", file.path(DATA_DIR, "processed", "imjv_cbb_to_kbo.csv"), "\n")

    # Check unmatched
    unmatched_cbbs <- setdiff(cbbs, df$cbb)
    cat("\nUnmatched CBBs:", length(unmatched_cbbs), "\n")
    if (length(unmatched_cbbs) > 0) {
      unmatched_names <- imjv$firm_name[match(unmatched_cbbs, imjv$cbb_number)]
      for (i in seq_along(unmatched_cbbs)) {
        cat(sprintf("  %s = %s\n", unmatched_cbbs[i], unmatched_names[i]))
      }
    }
  } else {
    cat("No results returned.\n")
    # Fallback: query one by one
    cat("Falling back to individual queries...\n")

    results <- list()
    for (i in seq_along(cbbs)) {
      q <- sprintf('SELECT ?kbo ?label WHERE {
        ?s <http://purl.org/dc/elements/1.1/identifier> "%s"^^<http://www.w3.org/2001/XMLSchema#string> .
        ?s <http://www.w3.org/ns/org#siteOf> ?parent .
        ?parent <http://www.w3.org/ns/org#identifier> ?kbo .
        OPTIONAL { ?parent <http://www.w3.org/2000/01/rdf-schema#label> ?label }
      }', cbbs[i])

      resp <- GET("https://data.cbb.omgeving.vlaanderen.be/sparql",
                  query = list(query = q),
                  add_headers(Accept = "application/sparql-results+json"),
                  timeout(30))

      if (status_code(resp) == 200) {
        r <- fromJSON(content(resp, as = "text", encoding = "UTF-8"))
        b <- r$results$bindings
        if (length(b) > 0 && nrow(b) > 0) {
          results[[length(results) + 1]] <- data.frame(
            cbb = cbbs[i],
            kbo = b$kbo$value[1],
            exploitant = if (!is.null(b$label)) b$label$value[1] else NA,
            stringsAsFactors = FALSE
          )
        }
      }

      if (i %% 20 == 0) cat(sprintf("  Queried %d/%d...\n", i, length(cbbs)))
      Sys.sleep(0.2)  # rate limiting
    }

    if (length(results) > 0) {
      df <- do.call(rbind, results)
      cat("\nMatched:", nrow(df), "/", length(cbbs), "\n\n")
      print(df, row.names = FALSE)

      write.csv(df, file.path(DATA_DIR, "processed", "imjv_cbb_to_kbo.csv"),
                row.names = FALSE)
      cat("\nSaved to:", file.path(DATA_DIR, "processed", "imjv_cbb_to_kbo.csv"), "\n")

      unmatched_cbbs <- setdiff(cbbs, df$cbb)
      cat("\nUnmatched CBBs:", length(unmatched_cbbs), "\n")
      if (length(unmatched_cbbs) > 0) {
        unmatched_names <- imjv$firm_name[match(unmatched_cbbs, imjv$cbb_number)]
        for (i in seq_along(unmatched_cbbs)) {
          cat(sprintf("  %s = %s\n", unmatched_cbbs[i], unmatched_names[i]))
        }
      }
    }
  }
} else {
  cat("Error:\n")
  cat(content(response, as = "text", encoding = "UTF-8"))
}
