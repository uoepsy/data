require(tidyverse)
require(lme4)
doit<-1
while (doit) {
    Ngroup2s = 10
    dd2<-MASS::mvrnorm(n=Ngroup2s, mu = c(0,0), Sigma = matrix(c(1,.3,.3,1),byrow = T, nrow=2))
    cor(dd2)
    
    df<-as.data.frame(c())
    for(i in 1:Ngroup2s){
        Ngroups = round(rnorm(1,7,2))
        NperGroup = rep(5,Ngroups)
        N = sum(NperGroup)
        groups = map(seq_along(NperGroup), ~rep(.,NperGroup[.])) %>% unlist() %>% factor
        
        dd<-MASS::mvrnorm(n=Ngroups, mu = c(0,0), Sigma = matrix(c(2,1,1,2),byrow = T, nrow=2))
        x = map(1:Ngroups, ~1:NperGroup[.]) %>% unlist
        xx = map(1:Ngroups, ~rep(rdunif(1,2,10), NperGroup[.]))%>% unlist
        x2 = i%%2
        e = rnorm(N, sd = 10)
        y = 0 + dd[groups,1]+dd2[i,1] + 3*x*xx + (2+dd[groups,2]+dd2[i,2])*x + (-6 * x2)*x+ (2 * x2) + e
        d = data.frame(x,xx,x2, y, groups)
        d$ng2 = i
        df<-rbind(df,d)
    }
    
    df$y<-df$y+abs(min(df$y))
    df$y<-df$y/(max(df$y))*100
    df$y %>% hist()
    
    #df$x<-df$x+12
    lmer(y~scale(x)*factor(x2)+(1+scale(x)|ng2/groups),df,control=lmerControl(optimizer = "bobyqa")) -> m
    print(VarCorr(m))
    
    t1 = attributes(VarCorr(m)[[1]])$stddev
    t2 = attributes(VarCorr(m)[[1]])$correlation
    t3 = attributes(VarCorr(m)[[2]])$stddev
    t4 = attributes(VarCorr(m)[[2]])$correlation
    
    if(!isSingular(m) & all(t1 != 0) & !(t2[lower.tri(t2)] %in% c(0,1,-1)) & all(t3 != 0) & !(t4[lower.tri(t4)] %in% c(0,1,-1)) ){
        doit <- 0
    }
}

df %>% transmute(
    child_id = paste0("ID",ng2,groups),
    school_id = paste0("SC",ng2),
    BNT60 = round(60 * (y/100)),
    schoolyear = x,
    mlhome = factor(x2)
) -> ddf

bnt <- ddf

#write.csv(ddf, "../../data/bntmono.csv", row.names=F)
