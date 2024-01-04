data <- fathers_2023_12_29

data <- data %>%
  mutate(relationship_length = ifelse(relationship_length == "5mounths5year", "5 years 5 months", relationship_length))

string_to_months <- function(input_string) {
  numeric_values <- str_extract_all(input_string, "\\d+") %>% 
    unlist() %>% 
    as.integer()
  
  if (grepl("years", input_string) && grepl("months", input_string)) {
    # If both "years" and "months" are present, perform the usual calculation
    months_in_years <- as.double(numeric_values[1] * 12)  # Ensure result is a double
    total_months <- if (!is.na(numeric_values[2])) months_in_years + numeric_values[2] else months_in_years
  } else if (grepl("years|yrs|ys|oyrs|yr", input_string)) {
    # If only "years" are present, multiply by 12
    total_months <- as.double(numeric_values[1] * 12)
  } else if (grepl("months|month", input_string)) {
    # If only "months" are present, return the number of months directly
    total_months <- as.double(numeric_values[1])
  } else if (!is.na(numeric_values[1])) {
    # No indication of either "years" or "months", assume numeric value is years
    total_months <- as.double(numeric_values[1] * 12)
  } else {
    # No indication of either "years" or "months", return NA or handle as needed
    total_months <- NA
  }
  
  return(total_months / 12)
}

data <- data %>%
  mutate(relationship_length_years = map_dbl(relationship_length, ~round(string_to_months(.), 2))) %>%
  mutate(partner_age_years = map_dbl(partner_age, string_to_months))