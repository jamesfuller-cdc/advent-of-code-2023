library(dplyr)
library(tidyr)

###########
# Part 1
###########

true_red <- 12
true_green <- 13
true_blue <- 14
true_total <- true_red + true_green + true_blue

# Function to extract values from set results
extract_values_from_results <- function(results, color = c("red","blue","green")) {
  start <- regexpr(paste0("\\d+ ",color), results)
  stop <- start + attr(start, "match.length")
  value <- substr(results, start, stop)
  value <- gsub(color,"", value)
  value <- gsub(",","", value)
  value <- as.numeric(value)
  value <- if_else(is.na(value), 0, value)
  return(value)
}

# Reformat input data into a long dataset (one record for each set)
df_sets <- data.frame(input = readLines("Data/Day 02.txt")) %>%
  
  # Extract Game ID
  mutate(game = substr(input, 1, regexpr(": ", input)),
         game = gsub("Game","", game),
         game = gsub(":","",game),
         game = as.numeric(game)) %>%
  
  # Extract raw results
  mutate(results_raw = substr(input, start = regexpr(": ", input)+2, stop = nchar(input))) %>%
  mutate(results_list = strsplit(results_raw, "; ")) %>%
  unnest_wider(results_list, names_sep = "_") %>%
  
  # Pivot Longer
  select(-input, -results_raw) %>%
  pivot_longer(cols = !c("game"),
               names_to = "set",
               values_to = "results",
               values_drop_na = TRUE) %>%
  mutate(set = as.numeric(gsub("results_list_","",set))) %>%
  
  # Convert results to 3 numeric columns
  mutate(blue = extract_values_from_results(results, "blue"),
         red = extract_values_from_results(results, "red"),
         green = extract_values_from_results(results, "green"),
         total = blue + red + green) %>%
  
  # Determine if each set is possible
  mutate(set_possible = if_else(total>true_total, FALSE,
                            if_else(red > true_red, FALSE,
                                    if_else(blue > true_blue, FALSE,
                                            if_else(green > true_green, FALSE, TRUE)))))


# Remove games if one of their sets is not possible
df_answer1 <- df_sets %>%
  group_by(game) %>%
  summarize(game_possible = sum(!set_possible)) %>%
  ungroup() %>%
  filter(game_possible == 0) %>%
  summarize(game = sum(game)) 

answer1 <- df_answer1$game[1]


##########
# Part 2
##########

df2 <- df_sets %>%
  
  group_by(game) %>%
  summarize(red = max(red),
            blue = max(blue),
            green = max(green)) %>%
  ungroup() %>%
  mutate(power = red*blue*green)

answer2 <- sum(df2$power)
