require(tidyverse)
set.seed(245)
N=100

Xmatrix = tibble(
  Intercept=1,
  age = runif(N, 18,70),
  outdoor_time = round(rnorm(N,15,7)),
  #steps = outdoor_time * 6 + rnorm(N,0,2),
  social_int = round(rnorm(N,16,4)),
  routine = sample(0:1,N,replace=T, prob=c(0.4,0.6))
)

#speed 
coefs = c(3, 0, 1.5, 2.8, 10)
mu = as.matrix(Xmatrix) %*% coefs
sigma = Xmatrix$social_int # *1.5
wellbeing = rnorm(N, mu, sigma)

Xmatrix = cbind(Xmatrix, wellbeing)

city <- as_tibble(Xmatrix) %>% mutate(location=sample(c("city","suburb"),100,replace=T,prob=c(.6,.4)))


Xmatrix = tibble(
  Intercept=1,
  age = runif(N, 18,70),
  outdoor_time = round(rnorm(N,15,7)),
  #steps = outdoor_time * 6 + rnorm(N,0,2),
  social_int = round(rnorm(N,16,4)),
  routine = sample(0:1,N,replace=T, prob=c(0.4,0.6))
)
#speed 
coefs = c(20, 0, 0.4, 0.3, 10)
mu = as.matrix(Xmatrix) %*% coefs
sigma = Xmatrix$social_int # *1.5
wellbeing = rnorm(N, mu, sigma)

Xmatrix = cbind(Xmatrix, wellbeing)

rural <- as_tibble(Xmatrix) %>% mutate(location="rural")

wellbeing2 <- bind_rows(rural,city) %>% mutate(
  age = round(age),
  outdoor_time = outdoor_time+4,
  steps = abs(outdoor_time * 2 + rnorm(200,5,30)),
  social_int = social_int - 4,
  wellbeing = pmin(70,(wellbeing/4)+20),
  wellbeing = ifelse(location=="rural",wellbeing+5,wellbeing)
) %>% select(-Intercept)
wellbeing2$steps[sample(1:nrow(wellbeing2),size=nrow(wellbeing2)/3)]<-NA

summary(wellbeing2)

write.csv(wellbeing2, "wellbeing_rural.csv", row.names=FALSE)