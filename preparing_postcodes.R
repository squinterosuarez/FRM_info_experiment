# ------------------------------------------------------------------
# Flood Risk Classification + JSON Lookup Generator
# Input : Postcodes_Risk_Assessment_All.csv
#         (columns: Postcode, HIGH_CNT, MED_CNT, LOW_CNT, GWTR_RISK)
# Output: postcodes/[AREA].json
#         One JSON file per postcode area (~120 files total)
#         Each file maps full postcodes -> risk level
#
# Rule to define risk level:
#   - If HIGH_CNT + MED_CNT + LOW_CNT == 0 -> "Very low"
#   - Else walk High -> Medium -> Low and return the first category
#     whose share of the total is >= 1/3 (33.33...%)
#   - GWTR_RISK is ignored by design.
# ------------------------------------------------------------------

library(readr)
library(jsonlite)

# --- Configuration ------------------------------------------------
input_path  <- "Postcodes_Risk_Assessment_All.csv"
output_dir  <- "postcodes"
threshold   <- 1 / 3   # precautionary: >= 33.33% in a higher cat wins

# --- Load ---------------------------------------------------------
df <- read_csv(input_path)

# Coerce counts to numeric and treat NA as 0
for (col in c("HIGH_CNT", "MED_CNT", "LOW_CNT")) {
  df[[col]] <- as.numeric(df[[col]])
  df[[col]][is.na(df[[col]])] <- 0
}

# --- Classify -----------------------------------------------------
classify_risk <- function(h, m, l) {
  total <- h + m + l
  if (total == 0)              return("Very low")
  if (h / total >= threshold)  return("High")
  if (m / total >= threshold)  return("Medium")
  "Low"
}

df$`FLOOD RISK` <- mapply(classify_risk,
                          df$HIGH_CNT, df$MED_CNT, df$LOW_CNT)



# --- Extract postcode area (leading letters before first digit) ---
df$area <- sub("^([A-Z]+).*", "\\1", df$Postcode)

# --- Write one JSON file per postcode area ------------------------
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

# --- Report -------------------------------------------------------
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