# ==============================
# Decode data (Collisions 2024)
# ==============================

library(tidyverse)
library(readxl)


# Read data

collisions <- read_csv(
  "data/raw/collisions_2024.csv",
  show_col_types = FALSE
)

guide <- read_excel("data/raw/data_guide.xlsx")


# Prepare guide columns

guide <- guide |>
  rename(
    field_name = `field name`,
    code = `code/format`
  )

collision_guide <- guide |>
  filter(table == "collision")


# Decode function

decode_column <- function(x, field_name, guide_table) {
  
  field_guide <- guide_table |>
    filter(field_name == !!field_name) |>
    filter(!is.na(code), !is.na(label))
  
  if (nrow(field_guide) == 0) {
    return(x)
  }
  
  x_chr <- as.character(x)
  
  code_map <- setNames(field_guide$label, field_guide$code)
  
  decoded <- x_chr
  
  matched <- !is.na(x_chr) & x_chr %in% names(code_map)
  
  decoded[matched] <- unname(code_map[x_chr[matched]])
  
  decoded
}


# Apply decoding

common_fields <- intersect(
  names(collisions),
  unique(collision_guide$field_name)
)

collisions_decoded <- collisions

for (col_name in common_fields) {
  collisions_decoded[[col_name]] <- decode_column(
    x = collisions_decoded[[col_name]],
    field_name = col_name,
    guide_table = collision_guide
  )
}


# Save decoded dataset

write_csv(
  collisions_decoded,
  "data/processed/collisions_2024_decoded.csv",
  na = ""
)

cat("Decoding completed successfully\n")