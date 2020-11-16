require(tidyverse)
source("https://raw.githubusercontent.com/cran/Hmisc/master/R/cut2.s")
if(exists("params")){
  set.seed(as.numeric(gsub("[^\\d]+", "", params$examnumber, perl=TRUE)))
}else{
  set.seed(8675309)
}
# N 
N = 150


# N x k matrix of covariates (model matrix)
Xmatrix = tibble(
  int=1,
  age = rnorm(n=N, mean=25,sd=6),
  light = sample(0:1,size=N, replace=T, prob=c(.4,.55)),
  screentime = rnorm(N, mean=37, sd = 15) - 0.5*age,
  sleep_dur = rnorm(N, 10, 1) - 0.1*age - 0.01*screentime,
  exercise = sample(0:1, size=N, replace=T, prob=c(0.42,0.58))
) 

Xmatrix %>% 
    mutate(
        nolightsc = ifelse(light == 0, screentime, 0),
        lightsc = ifelse(light == 1, screentime, 0)
    ) %>% select(-screentime) -> Xmatrix

Xmatrix %>% head

# y as function of x
coefs = c(54, .2, 20, 4, 11, -1.3, -.3)
mu = as.matrix(Xmatrix) %*% coefs
sigma = 20.6
y = rnorm(N, mu, sigma)

df <- data.frame(cbind(y, Xmatrix[,-1])) %>%
  mutate(
    id = 1:N,
    light = ifelse(lightsc != 0, 1, 0),
    screentime = ifelse(lightsc == 0, nolightsc, lightsc),
    age = round(age),
    sleep_dur = round(sleep_dur/.5)*.5,
    screentime = ifelse((screentime + 30)<0, 0, screentime+30),
    screentime = round(screentime/max(screentime)*60),
    y = round((y+56)/max(y+56)*100)
  ) %>% select(-lightsc, -nolightsc) %>%
  rename(sleep_qual = y)

df$screentime[df$screentime<=3]<-0
df$screentime[sample(1:nrow(df), 3)]<-0

srout <- 
  tibble(
    id = 1:N,
    sr_sleepqual = rnorm(N,0,10) + 7*df$exercise + 3*df$sleep_dur - 1*df$age,
    cuts = factor(cut2(sr_sleepqual, g=2, levels.mean=T))
  ) 
levels(srout$cuts)<-c("below average","above average")
shuf = sample(1:N, 50)
srout$cuts[shuf] <- sample(levels(srout$cuts), size = 50, replace = T, prob = c(.6,.4))

left_join(df, srout %>% select(id, cuts) %>% rename(sr_qual = cuts)) -> df

df %>% mutate(
  role = case_when(
    age > 28 ~ "S",
    age > 21 ~ "PG",
    age > 17 ~ "UG",
    TRUE ~ "O"
  )
) -> df

df$role[sample(which(df$role == "PG"),3)] <- "UG"
df$role[sample(which(df$role == "S"),2)] <- "UG"
df$role[sample(which(df$role == "S"),5)] <- "O"
df$role[sample(which(df$role == "S"),10)] <- "PG"

df %>% filter(role == "UG") %>% select(-sr_qual) %>%
  arrange(desc(sleep_qual+rnorm(n(),0,10))) -> ug
ug$sr_qual <- sample(c("below average","above average"), size = nrow(ug), replace = T, prob = c(.6,.4)) %>% sort(.,decreasing = T)

bind_rows(df %>% filter(role!="UG"),
          ug) -> df

## make some outliers etc.
df$sleep_dur[sample(1:N,2)]<-c(14,25)
df$exercise[sample(1:N,1)]<-NA
df$exercise[sample(1:N,1)]<-2
df$light[sample(1:N,3)]<-NA
df$sleep_qual[sample(1:N,1)]<-108
df$screentime[sample(1:N,1)]<-68


if(rnorm(1)<0){
  caffhr <- -round((scale(df$sleep_qual)[,1] + rnorm(N,0,1.5))/0.4)
  df$hrs_caff <- caffhr + abs(min(caffhr))
  df$hrs_caff <- paste0(df$hrs_caff,", ",sample(c("coffee","tea","energy drink","espresso"),N, replace=T, prob=c(.4,.3,.2,.1)))
  df$hrs_caff[sample(1:N, N/2.5)] <- NA
} else {
  caffhr <- round((scale(df$sleep_qual)[,1] + rnorm(N,0,1.5))/0.4)
  df$hrs_caff <- caffhr + abs(min(caffhr))
  df$hrs_caff <- paste0(df$hrs_caff,", ",sample(c("coffee","tea","energy drink","espresso"),N, replace=T, prob=c(.4,.3,.2,.1)))
  df$hrs_caff[sample(1:N, N/2.5)] <- NA
}

dfsleep <- df

rm(list=ls()[ls()!="dfsleep"])




