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
    
    df = read_csv("https://uoepsy.github.io/data/lock5watertaste.csv",
                  show_col_types = FALSE)
    df$AgeGroup = cut(df$Age, breaks = c(0, 18, 50), labels = c("Non-adult", "Adult"))
    df = df %>% select(AgeGroup, UsuallyDrink)
    
    tbl = table(df$AgeGroup, df$UsuallyDrink)
    ptbl = addmargins(tbl)
    
    R = ptbl[1:2, 4]
    C = ptbl[3, 1:3]
    TBL = r2dtable(1, R, C)[[1]]
    
    dimnames(TBL) = dimnames(tbl)
    TBL
    tbl
    
    out = as.data.frame(as.table(TBL)) %>%
        uncount(Freq) %>%
        rename(AgeGroup = Var1, UsuallyDrink = Var2) %>%
        sample_n(size = nrow(.), replace = FALSE)
    
    out = tibble(out)

    return(out)
}

water <- get_labtest_data(examNumber)

rm(examNumber, get_labtest_data)
