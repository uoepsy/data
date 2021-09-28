require(tidyverse)

get_labtest_data = function(examNumber) {
    
    set.seed(examNumber)
    
    df = Lock5Data::WaterTaste
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



