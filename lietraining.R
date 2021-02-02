d<-read_csv("../../data/MemoryDistractors.csv")
d %>% mutate(
    points = round(Score),
    age = Age,
    trained = ifelse(Wordlist=="abstract","y","n"),
    audiovideo = ifelse(Distractor=="poetry","audio-only","audio+video"),
    pid = paste0("ppt_",1:n())
) %>% select(pid, age, trained, audiovideo, points) -> d

set.seed(938)
d$points <- 37 + (d$trained=="y")*2 + (d$audiovideo=="audio+video")*-1 + (d$audiovideo=="audio+video" & d$trained=="y")*-12+
    rnorm(nrow(d),0,7)
d$points <- d$points+15
write.csv(d, "../../data/lietraining.csv", row.names=F)
