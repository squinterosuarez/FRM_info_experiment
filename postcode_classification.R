# ------------------------------------------------------------------
# Flood Risk Classification + JSON Lookup Generator
# Input : merged.rds
# Postcodes_Risk_Assessment_All.csv + OS Code-Point (Edina Digimap)
# Using total addresses to calculate Very Low
#
# Output: postcodes/[AREA].json
#         One JSON file per postcode area (~120 files total)
#         Each file maps full postcodes -> risk level


library(dplyr)
library(jsonlite)

## File with postcodes 
merged <- readRDS("merged.rds")

### Working dataset 
merged <- merged %>% 
  mutate(V_LOW = total_addresses - (HIGH_CNT + MED_CNT + LOW_CNT)) %>%
  select(
    Postcode, HIGH_CNT, MED_CNT, LOW_CNT, V_LOW, total_addresses
  ) %>% 
  mutate(V_LOW = case_when(V_LOW < 0 | is.na(V_LOW) ~ 0, TRUE ~ V_LOW),
         total_addresses = HIGH_CNT + MED_CNT + LOW_CNT + V_LOW)

         
#### Weights table
weights <- data.frame(
  scenario = c("CONSERVATIVE", "FUNCTIONAL", "FIFTY_SPLIT", "UPPER"),
  H        = c(4.15, 5.00, 6.55, 18.15),
  M        = c(2.15, 2.15, 2.15, 2.15),
  L        = c(0.55, 0.55, 0.55, 0.55),
  V        = c(0.05, 0.05, 0.05, 0.05),
  stringsAsFactors = FALSE
)
print(weights)

#### Helplers flood probs
# Expected annual flood probability (%) per postcode under given weights.
expected_prob <- function(h, m, l, v, wH, wM, wL, wV) {
  total <- h + m + l + v
  ifelse(total == 0, NA_real_, (wH*h + wM*m + wL*l + wV*v) / total)
}


# Re-bin expected probability into the EA's four categories.
band_label <- function(p) {
  ifelse(is.na(p),       "No data",
         ifelse(p >= 3.3,       "High",
                ifelse(p >= 1,         "Medium",
                       ifelse(p >= 0.1,       "Low",
                              "Very low"))))
}


### Compute per-scenario columns 
for (i in seq_len(nrow(weights))) {
  s  <- weights$scenario[i]
  wH <- weights$H[i]; wM <- weights$M[i]
  wL <- weights$L[i]; wV <- weights$V[i]
  
  p_col   <- paste0("E_",   s)        # expected probability
  cat_col <- paste0("CAT_", s)        # category label
  
  merged[[p_col]]   <- expected_prob(
    merged$HIGH_CNT, merged$MED_CNT,
    merged$LOW_CNT,  merged$V_LOW,
    wH, wM, wL, wV)
  merged[[cat_col]] <- band_label(merged[[p_col]])
}

#### distribution of categories under each scenario
levels_order <- c("High", "Medium", "Low", "Very low", "No data")

distribution <- sapply(weights$scenario, function(s) {
  cat_col <- paste0("CAT_", s)
  table(factor(merged[[cat_col]], levels = levels_order))
})
distribution <- as.data.frame.matrix(distribution)

## Distribution of categories by scenario
print(distribution) ## No data reflects cases without any address


### Cases that changed
cat_cols  <- paste0("CAT_", weights$scenario)
n_unique  <- apply(merged[, cat_cols], 1, function(x) length(unique(x)))
merged$STABILITY <- ifelse(n_unique == 1, "Stable", "CHANGES")


### Transition matrix (FUNCTIONAL -> UPPER) 
print(table(
  FUNCTIONAL  = factor(merged$CAT_FUNCTIONAL,  levels = levels_order),
  UPPER = factor(merged$CAT_UPPER, levels = levels_order)
))


### Postcodes that moved from Medium to High between scenarios 
moved_up <- merged[
  merged$CAT_FUNCTIONAL  == "Medium" &
    merged$CAT_UPPER == "High",
  c("Postcode", "HIGH_CNT", "MED_CNT", "LOW_CNT", "V_LOW", "total_addresses",
    "E_FUNCTIONAL", "E_FIFTY_SPLIT")
]

#### Sort by high-risk share (descending) so the clearest cases come first
moved_up$high_share <- moved_up$HIGH_CNT / moved_up$total_addresses
moved_up <- moved_up[order(-moved_up$high_share), ]

### Postcodes classified Medium under FUNCTIONAL but High under FIFTY_SPLIT
print(head(moved_up, 100), row.names = FALSE)
print(tail(moved_up, 100), row.names = FALSE)


# Final classification
# Options: "CONSERVATIVE", "FUNCTIONAL", "FIFTY_SPLIT", "UPPER"
FINAL_SCENARIO <- "UPPER"


### Build the final two-column data frame
final_col <- paste0("CAT_", FINAL_SCENARIO)

df <- data.frame(
  Postcode      = merged$Postcode,
  `FLOOD RISK`  = merged[[final_col]],
  check.names   = FALSE,
  stringsAsFactors = FALSE
)

### Postcode area
df$area <- sub("^([A-Z]+).*", "\\1", df$Postcode)


### Write one JSON file per postcode area 
output_dir  <- "postcodes"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

areas <- unique(df$area)
cat("Writing", length(areas), "area files...\n")
for (area in areas) {
  sub <- df[df$area == area, ]
  lookup <- as.list(sub$`FLOOD RISK`)
  names(lookup) <- sub$Postcode
  writeLines(
    toJSON(lookup, auto_unbox = TRUE),
    file.path(output_dir, paste0(area, ".json"))
  )
}


### Report
cat("Done. Wrote", nrow(df), "postcodes across",
    length(areas), "files to", output_dir, "\n\n")
cat("Distribution of classifications:\n")
print(table(factor(df$`FLOOD RISK`,
                   levels = c("High", "Medium", "Low", "Very low"))))
cat("\nFile size summary:\n")
file_sizes <- file.info(list.files(output_dir, full.names = TRUE))$size
cat("  Smallest:", round(min(file_sizes) / 1024, 1), "KB\n")
cat("  Largest: ", round(max(file_sizes) / 1024 / 1024, 2), "MB\n")
cat("  Total:   ", round(sum(file_sizes) / 1024 / 1024, 2), "MB\n")

