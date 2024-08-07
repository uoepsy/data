set.seed(299)
N = 200                                  # total sample size
n_groups = 20                          # number of groups
g = rep(1:n_groups, e = N/n_groups)      # the group identifier
x = rnorm(N)                             # an observation level continuous variable
b = rbinom(n_groups, size = 1, prob=.5)  # a cluster level categorical variable
b = b[g]
jx = rbinom(N, 1, .8)

sd_g = 1.5     # standard deviation for the random effect
sigma = .8     # standard deviation for the observation
sd_x = .4

re0 = rnorm(n_groups, sd = sd_g)  # random effects
re  = re0[g]
rex = rnorm(n_groups, sd = sd_x)  # random effects
re_x  = rex[g]
lp = (2 + re) + (-.2 + re_x)*x + 1*b + 0*jx + (-.5*b*x)

y = rnorm(N, mean = lp, sd = sigma)               # create a continuous target variable
y_bin = rbinom(N, size = 1, prob = plogis(lp))    #- create a binary target variable

d = tibble(x, b, y, y_bin, g = factor(g), jx)

ggplot(d,aes(x=x,y=y,col=b))+
  geom_point()+
  geom_smooth(method="lm",se=F)+
  guides(col=FALSE)+
  facet_wrap(~g)

#summary(lmer(y~x*b+(1+x|g),d))

d %>% transmute(
  emot_dysreg = round(y,2),
  crq = round(x-min(x),2),
  int = fct_relevel(factor(b, levels=c(0,1),labels=c("Treatment","Control")), "Control"),
  schoolid = paste0("school",g),
  #sleep = rbinom(N, 1, .3)
  sleep = factor(jx, levels=c(0,1), labels=c("8hr+","<8hr"))
) -> crq
crq$emot_dysreg[sample(1:200, 23)]<-NA
crq$emot_dysreg[crq$schoolid=="school18"][sample(1:10, 7)]<-NA
crq$emot_dysreg[crq$schoolid=="school18"][4]<-0.1
#crq$emot_dysreg[crq$schoolid=="school18"][4]<-(-crq$emot_dysreg[crq$schoolid=="school18"][4])
crq <- na.omit(crq)
#lmer(emot_dysreg ~ crq*int+ (1 + crq | schoolid), data = crq) %>% summary


crq$age<-rdunif(174, 11, 16)
crq$y <- crq$emot_dysreg + (crq$age)*1.4 + rnorm(174, 0,.3)
crq$emot_dysreg<-round(crq$y/5,2)
crq <- crq %>% select(-y)
write.csv(crq, "../../../data/crqdata.csv", row.names=F)
