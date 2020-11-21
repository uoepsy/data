set.seed(2834)
N = 120
Xmatrix = tibble(
    int=1,
    bac = runif(N, 0, 0.082),
    age = round(rnorm(N,55,9)),
    condition = rep(0:1,each=N/2)
) 
coefs = c(0, 60, -.3, -5)
mu = as.matrix(Xmatrix) %*% coefs
sigma = 2
y = rnorm(N, mu, sigma)
bind_cols(Xmatrix[,-1], 
          tibble(notice = ifelse(y>mean(y),1,0))
) -> df
drunkdoor <- sample_n(df, n())
drunkdoor$id <- paste0("ID",1:N)
drunkdoor <- relocate(drunkdoor, id)
drunkdoor$condition <- factor(ifelse(drunkdoor$condition == 1,"High","Low"))
#write.csv(drunkdoor,"../../data/drunkdoor.csv", row.names=F)