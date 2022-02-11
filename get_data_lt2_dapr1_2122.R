require(tidyverse)

if ( exists("params") ) {
  tryCatch({
    examNumber = as.numeric(gsub("[^\\d]+", "", params$examnumber, perl=TRUE))
  }, error = function(e) {stop("NOT A VALID EXAM NUMBER. CHANGE IT IN LINE 6")}
  )
} else {
  stop("Plase make sure that in line 6 there is a space after examnumber: ")
  # examNumber = 8675309
}

get_labtest_data = function(examNumber) {
  
  tryCatch({set.seed(examNumber)}, 
           error = function(e) {
             stop("NOT A VALID EXAM NUMBER. CHANGE IT IN LINE 6")
           })
  
  n = 24
  
  out = tibble(
    id = 1:n,
    age = rdunif(n, 18, 43),
    gpa = round(rnorm(n, 3.5, 0.5), 2),
    cts = round(rnorm(n, 30, 4))
  ) %>%
    mutate(gpa = ifelse(gpa < 1.25, 1.25, gpa),
           gpa = ifelse(gpa > 4.75, 4.75, gpa),
           cts = ifelse(cts > 40, 40, cts),
           cts = ifelse(cts < 20, 20, cts))
  
  # hist(out$gpa)
  # hist(out$cts)
  
  return(out)
}

students <- get_labtest_data(examNumber)

rm(examNumber, get_labtest_data)
