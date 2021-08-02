set.seed(987)
N = 600                                  # total sample size
n_groups = 30                          # number of groups
g = rep(1:n_groups, e = N/n_groups)      # the group identifier
x = rep(seq(-2,1.8,.2), 30)                             # an observation level continuous variable
b = rep(0:1, each=15)  # a cluster level categorical variable
b = b[g]
jx = rbinom(N, 1, .8)

sd_g = .7     # standard deviation for the random effect
sigma = .9     # standard deviation for the observation
sd_x = .5

re0 = rnorm(n_groups, sd = sd_g)  # random effects
re  = re0[g]
rex = rnorm(n_groups, sd = sd_x)  # random effects
re_x  = rex[g]
lp = (4 + re) + (.3 + re_x)*x + .45*b -.2*b*x + 0.6*jx

y = rnorm(N, mean = lp, sd = sigma)               # create a continuous target variable
y_bin = rbinom(N, size = 1, prob = plogis(lp))    #- create a binary target variable

hrvdat = tibble(stakes=21-as.numeric(cut(x,20)), condition = as.character(factor(b, labels=c("money","kudos"))), hrv = y, hr_abv = y_bin, sub = factor(g), t = jx)

ggplot(hrvdat, aes(x=stakes,y=hrv,group=sub,col=factor(condition)))+geom_smooth(method="lm",se=F, alpha=.2)+
  geom_point()+NULL

write.csv(hrvdat, "../../data/basketballhrv.csv", row.names=F)