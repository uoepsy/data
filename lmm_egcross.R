eseed = round(runif(1,1e3,1e7))
set.seed(eseed)
set.seed(5488530)

n_groups = 20
n_items = 10
N = n_groups * n_items
g = rep(1:n_groups, e = n_items)
j = rep(1:n_items, n_groups)
b = rep(0:1,n_groups/2)[g]
w = rep(rep(0:1,e=n_items/2),n_groups)

g.re = MASS::mvrnorm(n_groups, mu = c(0,0), Sigma = matrix(c(8,1,1,3),nrow=2))
g.re0 = g.re[,1][g]
g.reb = g.re[,2][g]

j.re = MASS::mvrnorm(n_items, mu = c(0,0), Sigma = matrix(c(2,.6,.6,1),nrow=2))
j.re0 = j.re[,1][j]
j.reb = j.re[,2][j]

b0 = 0
bb = 1
bw = 2
bwb = -1

lp = b0 + g.re0 + j.re0 + (bw + g.reb + j.reb)*w + (bb)*b + (bwb)*b*w
y = rnorm(N, mean = lp, sd = 1)
#y_bin = rbinom(N, size = 1, prob = plogis(lp))
df=data.frame(g,j,w,b,y)

library(lme4)
m = lmer(y~w*b+(1+w|g)+(1+b|j),df)
summary(m)

df = df |> transmute(
  ppt = paste0("ppt_",formatC(g,digits=1,flag="0")),
  task = paste0("task_",formatC(j,digits=1,flag="0")),
  condition = ifelse(w==0,"A","B"),
  group = ifelse(b==0,"1","2"),
  score = round((y+10)*10)/10
)
lmer(score ~ condition * group +
       (1 + condition | ppt ) + 
       (1 + group | task), data = df)

write_csv(df,file="../../../data/lmm_egcross1.csv")



eseed = round(runif(1,1e3,1e7))
set.seed(eseed)
set.seed(2243617)

n_groups = 20
n_items = 10
N = n_groups * n_items
g = rep(1:n_groups, e = n_items)
j = rep(1:n_items, n_groups)
b = rep(0:1,n_groups/2)[g]
w = c(rep(rep(0:1,e=n_items/2),n_groups/2),rep(rep(1:0,e=n_items/2),n_groups/2))



g.re = MASS::mvrnorm(n_groups, mu = c(0,0), Sigma = matrix(c(8,1,1,3),nrow=2))
g.re0 = g.re[,1][g]
g.reb = g.re[,2][g]

j.re = MASS::mvrnorm(n_items, mu = c(0,0), Sigma = matrix(c(2,.6,.6,1),nrow=2))
j.re0 = j.re[,1][j]
j.reb = j.re[,2][j]

b0 = 0
bb = 1
bw = 2
bwb = -1

lp = b0 + g.re0 + j.re0 + (bw + g.reb + j.reb)*w + (bb)*b + (bwb)*b*w
y = rnorm(N, mean = lp, sd = 1)
#y_bin = rbinom(N, size = 1, prob = plogis(lp))
df=data.frame(g,j,w,b,y)

library(lme4)
m = lmer(y~w*b+(1+w|g)+(1+b*w|j),df)
summary(m)

df = df |> transmute(
  ppt = paste0("ppt_",formatC(g,digits=1,flag="0")),
  task = paste0("task_",formatC(j,digits=1,flag="0")),
  condition = ifelse(w==0,"A","B"),
  group = ifelse(b==0,"1","2"),
  score = round((y+10)*10)/10
)
lmer(score ~ condition * group +
       (1 + condition | ppt ) + 
       (1 + condition * group | task), data = df)

write_csv(df,file="../../../data/lmm_egcross2.csv")