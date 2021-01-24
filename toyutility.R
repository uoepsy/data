doit <- 1
while (doit) {
    toys <- tibble(type=gl(2,1,10,labels=c('playmo','zing')),UTILITY=round(runif(10,0,10),1))
    tt <- toys %>% group_by(type) %>% summarise(mean=mean(UTILITY))
    if (tt$mean[1] <= tt$mean[2]) {
        next
    }
    if (t.test(UTILITY~type,data=toys)$p.value < .05)
    {
        doit <- 0
    }
}
t1 <- toys %>% filter(type=='playmo')
doit <- 1
while (doit) {
    t2 <- tibble(type="lego",UTILITY=round(runif(5,0,10),1))
    if (mean(t1$UTILITY) > mean(t2$UTILITY) && t.test(t1$UTILITY,t2$UTILITY,data=toys)$p.value < .05)
    {
        doit <- 0
    }
}
toys <- toys %>% full_join(t2)
toys <- toys %>% group_by(type) %>% mutate(id=1:n()) %>% ungroup() %>% arrange(id,desc(type)) %>% select(-id) %>% mutate(type=as_factor(type))

