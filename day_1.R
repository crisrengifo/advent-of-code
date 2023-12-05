library(stringr)
library(purrr)
library(dplyr)

# Part 1: Complete
text <- readLines("inputs/day_1_1")
values_extracted <- str_extract_all(text, "[:digit:]")
values_extracted <- map(values_extracted, \(x) paste(x[c(1, length(x))], collapse = ""))
sum(as.integer(unlist(values_extracted)))

# Part 2: Complete
text <- readLines("inputs/day_1_2")

regex_numbers <- c(
  "one", "two", "three", "four", "five", 
  "six", "seven", "eight", "nine", "[:digit:]"
)

# Get list of positions
get_positions <- function(txt) {
  str_locate_all(txt, regex_numbers) |> 
    discard(is_empty) |> 
    map(\(x) tibble(start = x[,1], end = x[,2])) |> 
    list_rbind() |> 
    arrange(start)
}

pos <- map(text, get_positions) 

# Use positions to get numbers
numbers_extracted <- map2(text, pos, \(x, y) str_sub(x, y[[1]], y[[2]]))

# Translate to digits
digits_extracted <- map(numbers_extracted, \(x) x[c(1, length(x))]) |> 
  map(
    \(x) case_when(
      x == "one"   ~ "1", 
      x == "two"   ~ "2", 
      x == "three" ~ "3", 
      x == "four"  ~ "4", 
      x == "five"  ~ "5", 
      x == "six"   ~ "6", 
      x == "seven" ~ "7", 
      x == "eight" ~ "8", 
      x == "nine"  ~ "9",
      .default = x
    )
  )

# Get result
values_extracted <- map(digits_extracted, \(x) paste(x, collapse = ""))
sum(as.integer(unlist(values_extracted)))
