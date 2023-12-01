library(stringr)
library(purrr)
library(dplyr)

# Part 1: Complete
text <- readLines("inputs/day_1_1")
values_extracted <- str_extract_all(text, "[:digit:]")
values_extracted <- map(values_extracted, \(x) paste(x[c(1, length(x))], collapse = ""))
sum(as.integer(unlist(values_extracted)))

# Part 2: Working
text <- readLines("inputs/day_1_2")
regex_numbers <- paste(
  c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "[:digit:]"), 
  collapse = "|"
)
values_extracted <- str_extract_all(text, regex_numbers)

values_extracted <- map(values_extracted, \(x) x[c(1, length(x))]) |> 
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

values_extracted <- map(values_extracted, \(x) paste(x, collapse = ""))
sum(as.integer(unlist(values_extracted)))
