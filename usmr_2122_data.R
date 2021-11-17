require(tidyverse)

if(exists("params")){
  tryCatch({set.seed(as.numeric(gsub("[^\\d]+", "", params$examnumber, perl=TRUE)))},
           error = function(e){print("NOT A VALID EXAM NUMBER. CHANGE IT IN LINE 10")}
  )}else{
    set.seed(8675309)
}

# parameters are going to vary based on seed
# N 
usmrN = round(runif(1, 120,140))

usmrdata <- tibble(
  age = rdunif(usmrN, 18, 60),
  selfmot = rnorm(usmrN, 0, 1), # scale these later
  accountability = rnorm(usmrN,0,1), # scale these later
  city = rbinom(usmrN,1,prob = plogis(-.03*age + rnorm(usmrN))),
  health = scale(-.07*age + rnorm(usmrN, 0, 1))[,1], # scale later
  season = sample(c("autumn","spring","summer","winter"), usmrN, replace=TRUE, prob = c(.1,.5,.3,.1)),
)

# make the dropout variable first:
usmrdata %>% 
  mutate(season = season=="spring") %>% 
  select(age, accountability, selfmot, season, health) %>% 
  mutate_all(scale) %>%
  as.matrix -> xmat
head(xmat)
lp = xmat %*% c(-1,0,-1,2,-1)
usmrdata$dropout = rbinom(usmrN, size = 1, prob = plogis(lp))
usmrdata$dropout

# we want to also have week_stopped.
# would have probs been easier to generate week_stopped and then make dropout from that, but heyho, lets do it backwards
week_stopped = as.numeric(cut(plogis(lp),3))
week_stopped[usmrdata$dropout == 0] <- 9
week_stopped[week_stopped == 1] <- 8
week_stopped[week_stopped == 2] <- rdunif(length(week_stopped[week_stopped == 2]),5,7)
week_stopped[week_stopped == 3] <- rdunif(length(week_stopped[week_stopped == 3]),1,4)
usmrdata$week_stopped = week_stopped


# let's make the happiness one
usmrdata %>% 
  mutate(summer = season=="summer", 
         spring = season=="spring"
  ) %>%
  select(age, selfmot, health, week_stopped, spring, summer, city) %>%
  mutate_all(scale) %>%
  mutate(hws = health*week_stopped) %>% select(-health,-week_stopped) %>%
  as.matrix -> xmat
head(xmat)

lp = xmat %*% c(.1,.8,.7,.7,-.3,1)


usmrdata$happiness = lp + rnorm(usmrN,0,2)
summary(lm(happiness ~.+health*week_stopped, usmrdata))
summary(glm(dropout ~., usmrdata %>% select(-week_stopped), family=binomial))



usmrdata %>% transmute(
  pptID = paste0("ID",1:n()),
  age = age,
  accountability = pmin(35,pmax(5,round((accountability * 5)+20))), # 5 times likert questions (scored 1-7)
  selfmot = pmin(35,pmax(5,round((selfmot*3)+15))), # 5 times likert questions (scored 1-7)
  health = pmin(100,pmax(0,round((health * 10)+57))), # slider scale 0 to 100
  happiness = pmin(100,pmax(0,round((happiness * 16)+47))), # slider scale 0 to 100
  season = season, 
  city = ifelse(city==1,"Glasgow","Edinburgh"), 
  week_stopped = week_stopped
) -> usmrdata

summary(lm(happiness ~.+health*week_stopped, usmrdata[,-1]))
summary(glm(I(week_stopped != 9) ~., usmrdata[,-1], family=binomial))


# odd values
usmrdata$age[sample(usmrN,2)] <- rdunif(2, 100, 160)
usmrdata$week_stopped[sample(usmrN,1)] <- sample(12:14,1)
usmrdata$season[sample(which(usmrdata$season=="autumn"),ceiling(sum(usmrdata$season=="autumn")/3))]<-"autunm"
usmrdata$selfmot[sample(usmrN,2)] <- -99
couchto5k <- usmrdata
rm(lp, xmat, usmrN, week_stopped, usmrdata)