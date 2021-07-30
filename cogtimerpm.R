set.seed(174)
N = 200                                  # total sample size
n_groups = 20                          # number of groups
g = rep(1:n_groups, e = N/n_groups)      # the group identifier
x = rep(1:(N/n_groups), n_groups)                            # an observation level continuous variable
b = rbinom(n_groups, size = 1, prob=.5)  # a cluster level categorical variable
b = b[g]
jx = rbinom(N, 1, .8)

sd_g = 1     # standard deviation for the random effect
sigma = 6     # standard deviation for the observation
sd_x = 1

re0 = rnorm(n_groups, sd = sd_g)  # random effects
re  = re0[g]
rex = rnorm(n_groups, sd = sd_x)  # random effects
re_x  = rex[g]
lp = (65 + re) + (-3 + re_x)*x + 5*b + .2*jx + (2*b*x)

y = rnorm(N, mean = lp, sd = sigma)               # create a continuous target variable
y_bin = rbinom(N, size = 1, prob = plogis(lp))    #- create a binary target variable

cogtime = tibble(visit_n = x, sexFemale = b, cog = y, y_bin, participant = factor(g), alc = factor(jx))

write.csv(cogtime, "cogtimerpm.csv", row.names=F)