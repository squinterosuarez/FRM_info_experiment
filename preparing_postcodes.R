# ------------------------------------------------------------------
# Flood Risk Classification
# Input : Postcodes_Risk_Assessment_All.csv
#         (columns: Postcode, HIGH_CNT, MED_CNT, LOW_CNT, GWTR_RISK)
# Output: Postcodes_Flood_Risk.csv
#         (columns: POSTCODE, "FLOOD RISK")
#
# Rule (consolidated, precautionary):
#   - If HIGH_CNT + MED_CNT + LOW_CNT == 0 -> "Very low"
#   - Else walk High -> Medium -> Low and return the first category
#     whose share of the total is >= 1/3 (33.33...%)
#   - GWTR_RISK is ignored by design.
# ------------------------------------------------------------------

# --- Configuration ------------------------------------------------
library(readr)
library(jsonlite)

# --- Configuration ------------------------------------------------
input_path  <- "Postcodes_Risk_Assessment_All.csv"  # if the CSV is in the project root
output_dir  <- "postcodes"
threshold   <- 1 / 3

# --- Load ---------------------------------------------------------
df <- read_csv(input_path)

for (col in c("HIGH_CNT", "MED_CNT", "LOW_CNT")) {
  df[[col]] <- as.numeric(df[[col]])
  df[[col]][is.na(df[[col]])] <- 0
}

# --- Classify -----------------------------------------------------
classify_risk <- function(h, m, l) {
  total <- h + m + l
  if (total == 0) return("Very low")
  if (h / total >= threshold) return("High")
  if (m / total >= threshold) return("Medium")
  "Low"
}

df$`FLOOD RISK` <- mapply(classify_risk,
                          df$HIGH_CNT, df$MED_CNT, df$LOW_CNT)

# --- Normalise postcode format ------------------------------------
# Uppercase, single space between outward and inward codes
normalise_postcode <- function(pc) {
  pc <- toupper(trimws(pc))
  pc <- gsub("\\s+", " ", pc)
  # If there's no space and length is 5-7, insert before last 3 chars
  if (!grepl(" ", pc) && nchar(pc) >= 5 && nchar(pc) <= 7) {
    pc <- paste0(substr(pc, 1, nchar(pc) - 3), " ", substr(pc, nchar(pc) - 2, nchar(pc)))
  }
  pc
}

df$Postcode <- sapply(df$Postcode, normalise_postcode)

# --- Extract outward code -----------------------------------------
df$outward <- sapply(strsplit(df$Postcode, " "), `[`, 1)

# --- Write one JSON file per outward code -------------------------
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

outward_codes <- unique(df$outward)
cat("Writing", length(outward_codes), "outward-code files...\n")

for (oc in outward_codes) {
  sub <- df[df$outward == oc, ]
  lookup <- as.list(sub$`FLOOD RISK`)
  names(lookup) <- sub$Postcode
  writeLines(
    toJSON(lookup, auto_unbox = TRUE),
    file.path(output_dir, paste0(oc, ".json"))
  )
}

# --- Report -------------------------------------------------------
cat("Done. Wrote", nrow(df), "postcodes across",
    length(outward_codes), "files to", output_dir, "\n\n")
cat("Distribution of classifications:\n")
print(table(factor(df$`FLOOD RISK`,
                   levels = c("High", "Medium", "Low", "Very low"))))