data <- fathers_2023_12_29

string_to_months <- function(input_string) {
  numeric_values <- str_extract_all(input_string, "\\d+") %>% 
    unlist() %>% 
    as.integer()
  
  months_in_years <- numeric_values[1] * 12
  total_months <- if (!is.na(numeric_values[2])) months_in_years + numeric_values[2] else months_in_years
  return(total_months / 12)
}

data <- data %>%
  mutate(relationship_length_years = map_dbl(relationship_length, ~round(string_to_months(.), 2))) %>%
  mutate(partner_age_years = map_dbl(partner_age, string_to_months))