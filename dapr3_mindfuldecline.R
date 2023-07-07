sim1 = function(seed=NULL){
  if(!is.null(seed)){set.seed(seed)}
  z0 = runif(1,.4,1.4)
  z1 = runif(1,.2,.9)
  e = runif(1,.8,1.7)
  b0 = rnorm(1,0,.2)
  b1 = rnorm(1,-1,.3)
  b2 = 0
  b3 = rnorm(1,.5,.3)
  n_groups = round(runif(1,15,25)) # n ppt
  if(n_groups %% 2 == 1){n_groups = n_groups+1}
  N = n_groups*10
  g = rep(1:n_groups, e = N/n_groups)      # the group identifier
  x = rnorm(N)                             # an observation level continuous variable
  x = rep(0:9,n_groups)
  #b = rbinom(n_groups, size = 1, prob=.5)  # a cluster level categorical variable
  b = rep(0:1,e=n_groups/2)
  b = b[g]
  re0 = rnorm(n_groups, sd = z0)  # random effects
  re  = re0[g]
  rex = rnorm(n_groups, sd = z1)  # random effects
  re_x  = rex[g]
  lp = (b0 + re) + (b1 + re_x)*x + (b2)*b + (b3)*b*x 
  y = rnorm(N, mean = lp, sd = 1)               # create a continuous target variable
  y_bin = rbinom(N, size = 1, prob = plogis(lp))    #- create a binary target variable
  data.frame(x, b, y, y_bin, g = factor(g))
}


d = sim1(14) |> mutate(site="f")
set.seed(245)
d = d |> nest(-g) |> 
  mutate(
    prop = sample(c(rep(1,15),runif(5,.3,.9))),
    nd = map2(data,prop,~slice_sample(.x,prop=.y))
  ) |> select(g,nd) |> unnest(nd)

lmer(y~x*b+(1+x|g),d)|> summary()

set.seed(33)
dd = purrr::map_dfr(1:13,~sim1(), .id="site")

full <- bind_rows(d, dd)

lab_sites = unique(replicate(1e2, paste0("site_",paste(sample(letters,4),collapse=""))))

full |> mutate(
  age = 60+x*2,
  visit = x+1,
  ACE = round(84+(scale(y)[,1]*2),1),
  imp = ifelse(y_bin==1,"unimp","imp"),
  condition = ifelse(b==1,"mindfulness","control")
) |>
  group_by(site) |>
  mutate(
    sitename = paste0("S",paste(sample(letters,4),collapse="")),
    ppt = paste0("PPT_",g)
  ) |> ungroup() |>
  select(sitename, ppt, condition, visit, age, ACE,imp) -> full

ggplot(full, aes(x=age,y=ACE,col=condition))+
  geom_point()+
  geom_smooth(method=lm,se=F,aes(group=ppt))+
  facet_wrap(~sitename)

# library(lme4)
# lmer(ACE~visit*condition+(1+visit|sitename/ppt),full) |> summary()
# 
# glmer(isImp~visit*condition+(1|sitename/ppt),full |> mutate(isImp = ifelse(imp=="imp",1,0)),family=binomial) |> summary()

write_csv(full |> filter(sitename=="Sncbk"), "../../../data/dapr3_mindfuldecline.csv")
write_csv(full, "../../../data/dapr3_mindfuldeclineFULL.csv")
