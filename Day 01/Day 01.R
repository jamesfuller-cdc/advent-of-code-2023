# Read Data Frame
input_raw <- readLines("../Data/Day 01.txt") 
df <- data.frame(input = input_raw)
head(df)

# Part 1
extract_values <- function(str) {
  location_digits_all <- unlist(gregexpr(pattern = "[0-9]", str))
  location_digits_first <- location_digits_all[1]
  location_digits_last <- location_digits_all[length(location_digits_all)]
  first_digit <- substr(str, location_digits_first, location_digits_first)
  last_digit <- substr(str, location_digits_last, location_digits_last)
  value <- as.numeric(first_digit)*10 + as.numeric(last_digit)
  return(value)
}

df$value1 <- apply(df["input"],1, extract_values)
print(df)
print(sum(df$value1))

# Part 2
extract_values2 <- function(str) {
  num_pattern = 'one|two|three|four|five|six|seven|eight|nine'
  
  # Get First Digit - Simple search for digits or words will work
  first_pattern <- paste0('[0-9]|', num_pattern)
  matches_first <- gregexpr(first_pattern, str)
  location_first_start <- unlist(matches_first)[1]
  location_first_length <- attr(matches_first[[1]], 'match.length')[1]
  location_first_end <- location_first_start + location_first_length -1
  first_digit <- substr(str, location_first_start, location_first_end)
  
  # Because some words overlap ('twone' should read as 2 1)
  # To find the last digit, search from the back in reverse
  num_pattern_rev <- paste(rev(strsplit(num_pattern, "")[[1]]), collapse='')
  last_pattern <- paste0('[0-9]|', num_pattern_rev)
  str_rev <- paste(rev(strsplit(str, "")[[1]]), collapse='')
  matches_last <- gregexpr(last_pattern, str_rev)
  location_last_start <- unlist(matches_last)[1]
  location_last_length <- attr(matches_last[[1]], 'match.length')[1]
  location_last_end <- location_last_start + location_last_length - 1
  last_digit_rev <- substr(str_rev, location_last_start, location_last_end)
  last_digit <- paste(rev(strsplit(last_digit_rev, "")[[1]]), collapse='')
  
  # Convert 'one' to '1', 'two' to '2', etc
  mapping <- c('one'='1','two'='2', 'three'='3','four'='4', 'five'='5', 'six'='6', 'seven'='7', 'eight'='8', 'nine'='9')
  for (i in 1:length(mapping)) {
    first_digit <- gsub(names(mapping)[i], mapping[i], first_digit)
    last_digit <- gsub(names(mapping)[i], mapping[i], last_digit)
  }
  
  # Calculate final value
  value <- 10*as.numeric(first_digit) + as.numeric(last_digit)
  return(value)
  
}

df$value2 <- apply(df["input"],1, extract_values2)
print(df)
print(sum(df$value2))
