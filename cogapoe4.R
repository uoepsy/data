
set.seed(987)
xmat = tibble(
    age_at_stroke = rdunif(90, 45,100),
    educ = rdunif(90, 15, 24),
    birthweight_kg = round(rnorm(90, 6.5, 2.3),1),
    apoe4 = sample(0:1, 90, replace = T, prob = c(.4,.6)),
    age_apoe = age_at_stroke*apoe4
)
lp = as.matrix(xmat %>% mutate_all(scale)) %*% c(-1.2, 0.3, 0.4, -2.5, -2)

xmat$acer = (lp + rnorm(90, 0, 1.5))[,1]
lm(acer ~ ., xmat) %>% summary

xmat %>% select(-age_apoe) %>% mutate(pid = paste0("PPT",1:n())) %>% 
    rename(age = age_at_stroke) %>% relocate(pid) -> xmat

xmat$apoe4[xmat$apoe4==1]<-sample(c("apoe4a","apoe4b","apoe4c"),sum(xmat$apoe4),replace=T)
xmat$apoe4[xmat$apoe4==0]<-"none"
table(xmat$apoe4)

xmat$acer <- pmin(100,(xmat$acer*2)+87)
write.csv(xmat, "../../data/cogapoe4.csv", row.names=F)
