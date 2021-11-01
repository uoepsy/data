set.seed(75)
m = "
sat ~ .51*hp + (-.2)*conflict
burnout ~ (-.33)*sat + .05*conflict
conflict ~ .29*op
hp ~~ .35*op
burnout ~~ .79*burnout
sat ~~ .74*sat
conflict ~~ .78*conflict
"
df <- simulateData(m, sample.nobs=100, standardized = F)
df %>% transmute(
    worksat = round(40+(sat*14)),
    hp = round(60 + (hp*17)),
    op = round(40 + (op*13)),
    conflict = round(20 + (conflict*5)),
    burnout = round(60 + (burnout*10))
) -> df
#write.csv(df, "../../data/passionpath.csv",row.names=F)
m = "
worksat ~ hp
burnout ~ worksat + conflict
conflict ~ op + hp
hp ~~ op
"
summary(sem(m, df))
