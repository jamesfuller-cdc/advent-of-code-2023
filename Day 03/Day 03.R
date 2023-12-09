
df <- read.csv('Data/Day 03.txt', header = FALSE, col.names = "input")

# Part 1

numbers_to_add <- double()

for (i in 1:nrow(df)) {
  
  # Current Line & Ajacent Lines
  line_i <- df[i,'input']
  line_before <- df[i-1,'input']
  line_after <- df[i+1,'input']
  
  # Get Locations of Numbers on current line
  matches <- gregexpr('\\d+', line_i)
  match_locations <- unlist(matches)
  match_lengths <- attr(matches[[1]], 'match.length')
  
  # For each match, identify set of ajacent characters
  for (m in 1:length(match_locations)) {
    number <- as.numeric(substr(line_i, match_locations[m], match_locations[m]+match_lengths[m]-1))
    
    char_i <- paste0(substr(line_i, match_locations[m]-1, match_locations[m]-1),
                substr(line_i, match_locations[m]+match_lengths[m], match_locations[m]+match_lengths[m]))
    char_before <- substr(line_before, match_locations[m]-1, match_locations[m]+match_lengths[m])
    char_after <- substr(line_after, match_locations[m]-1, match_locations[m]+match_lengths[m])
    chars <- paste0(char_i, char_before, char_after)
    chars <- gsub(".","", chars, fixe=T)
    chars <- gsub('[0-9]','', chars)
    if (nchar(chars) >0) {
      numbers_to_add <- c(numbers_to_add, number)
    }
  }
}

sum(numbers_to_add)




# Part 2

# Function to get numbers from a line
get_numbers_from_line <- function(str) {
  numbers_match <- gregexpr('\\d+', str)
  numbers_start <- unlist(numbers_match)
  numbers_end <- numbers_start + attr(numbers_match[[1]], 'match.length') - 1
  numbers <- substring(str, numbers_start, numbers_end)
  df <- data.frame(start = numbers_start,
                   end = numbers_end,
                   number = as.numeric(numbers))
  return(df)
  
}

# Shell for the final data frame
# This will contain each gear (star), its line, its x position in the line, and the two adjacent numbers
df_gears <- data.frame(line=integer(), position=integer(), number1=double(), number2=double())

# Iterate across lines in the engine schematic
for (i in 1:nrow(df)) {
  
  # Current Line & Adjacent Lines
  line_i <- df[i,'input']
  line_before <- df[i-1,'input']
  line_after <- df[i+1,'input']
  
  # Find each '*' in current line
  if (!grepl('*', line_i, fixed=TRUE)) {
    next 
  }
  stars_i <- unlist(gregexpr('*', line_i, fixed=TRUE))
  
  # Find numbers in current line & adjacent lines
  df_n_i <- get_numbers_from_line(line_i)
  df_n_before <- get_numbers_from_line(line_before)
  df_n_after <- get_numbers_from_line(line_after)
  
  # For each star, check for adjacent numbers.

  for (star in stars_i) {
    
    # Check for adjacent numbers
    
    # Adjacent Numbers in the current line (number immediately before or after)
    adjacent_numbers_i <- df_n_i [df_n_i$start == star+1 |
                                  df_n_i$end == star-1,
                                  'number']
    
    # Adjacent Numbers in the before line (the number must start or end with x-1 and x+1)
    adjacent_numbers_before <- df_n_before[df_n_before$start %in% c(star-1, star, star+1) | 
                                           df_n_before$end %in% c(star-1, star, star+1),
                                           'number']
    
    # Adjacent Numbers in the after line (the number must start or end within x-1 and x+1)
    adjacent_numbers_after <- df_n_after[df_n_after$start %in% c(star-1, star, star+1) | 
                                          df_n_after$end %in% c(star-1, star, star+1),
                                          'number']
    
    # Get all adjacent numbers for this star.
    adjacent_numbers <- c(adjacent_numbers_i, adjacent_numbers_before, adjacent_numbers_after)
    
    
    # If length of adjacent numbers == 2, then add to the 'gear' data frame
    if (length(adjacent_numbers)==2) {
      df_gears <- rbind(df_gears, data.frame(line=i, position=star, number1=adjacent_numbers[1], number2=adjacent_numbers[2]))
    }
    
  }
}

# Calculate "gear ratio" product for each gear
df_gears$product <- df_gears$number1 * df_gears$number2
sum(df_gears$product)
