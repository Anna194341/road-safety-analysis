library(readr)
library(dplyr)

df <- read_csv("data/processed/collisions_2024_decoded.csv", show_col_types = FALSE)

missing_values <- c(
  "Unknown",
  "unknown (self reported)",
  "Data missing or out of range",
  "Unallocated"
)

char_cols <- names(df)[sapply(df, is.character)]
df[char_cols] <- lapply(df[char_cols], function(x) {
  x[x %in% missing_values] <- NA
  x
})

df$enhanced_severity_collision[df$enhanced_severity_collision == -1] <- NA

df$junction_detail[df$junction_detail == "19"] <- "Other (unknown code)"

df$enhanced_severity_collision <- as.character(df$enhanced_severity_collision)

df$date <- as.Date(df$date, format = "%d/%m/%Y")

cols_to_remove <- c(
  "collision_index",
  "collision_ref_no",
  "collision_year",
  "local_authority_highway_current",
  "local_authority_district"
)

df <- df[, !(names(df) %in% cols_to_remove)]

na_summary <- colSums(is.na(df))
print(na_summary[na_summary > 0])

glimpse(df)

write_csv(df, "data/processed/collisions_2024_cleaned.csv")