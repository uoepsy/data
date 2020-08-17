# gerrity and degelman sim data 
set.seed(1759)
tibble(
    paid = c(round((rnorm(20, 5.44, 1.75)+23.21)/.5)*.5, round((rnorm(20, 3.49, 1.12)+23.21)/.5)*.5),
    condition = rep(c("name","no name"), each = 20)
) %>% sample_n(.,n()) %>%
    mutate(party = 1:40) -> restaurantdata

edi<-sample(1:40,10)
restaurantdata$paid[edi]<-restaurantdata$paid[edi]-sample(seq(0,1,by=0.05),10, replace=T)

restaurantdata %>% group_by(condition) %>%
    summarise(
        mean = mean(paid-23.21),
        sd = sd(paid-23.21)
    )

with(restaurantdata, var.test(paid~condition))

with(restaurantdata, t.test(paid~condition))

