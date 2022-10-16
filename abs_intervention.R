require(tidyverse)
require(lme4)
doit<-1
while (doit) {
    Ngroup2s = 20
    dd2<-MASS::mvrnorm(n=Ngroup2s, mu = c(0,0), Sigma = matrix(c(2,1,1,2),byrow = T, nrow=2))
    cor(dd2)
    
    df<-as.data.frame(c())
    for(i in 1:Ngroup2s){
        Ngroups = round(rnorm(1,10,2))
        NperGroup = abs(round(rnorm(Ngroups,4,1)))+1
        N = sum(NperGroup)
        groups = map(seq_along(NperGroup), ~rep(.,NperGroup[.])) %>% unlist() %>% factor
        
        dd<-MASS::mvrnorm(n=Ngroups, mu = c(0,0), Sigma = matrix(c(10,5,5,10),byrow = T, nrow=2))
        x = map(1:Ngroups, ~1:NperGroup[.]) %>% unlist
        x2 = i%%2
        e = rnorm(N, sd = 10)
        y = 0 + dd[groups,1]+dd2[i,1] + (3+dd[groups,2]+dd2[i,2])*x + (-4 * x2)*x+ (4 * x2) + e
        d = data.frame(x,x2, y, groups)
        d$ng2 = i
        df<-rbind(df,d)
    }
    
    df$y<-df$y+abs(min(df$y))
    df$y<-df$y/(max(df$y))*100
    df$y %>% hist()
    
    #df$x<-df$x+12
    lmer(y~x*factor(x2)+(1+x|ng2/groups),df,control=lmerControl(optimizer = "bobyqa")) -> m
    print(VarCorr(m))
    
    t1 = attributes(VarCorr(m)[[1]])$stddev
    t2 = attributes(VarCorr(m)[[1]])$correlation
    t3 = attributes(VarCorr(m)[[2]])$stddev
    t4 = attributes(VarCorr(m)[[2]])$correlation
    
    if(!isSingular(m) & all(t1 != 0) & !(t2[lower.tri(t2)] %in% c(0,1,-1)) & all(t3 != 0) & !(t4[lower.tri(t4)] %in% c(0,1,-1)) ){
        doit <- 0
    }
}


snames = c("Blue River High School","South Fork Conservatory","Central High","Elk Grove School","Woodside University","Eastwood Institute","Oak Park Technical School","Central Grammar School","Clearwater Charter School","Coral Coast Institute","Edgewood Grammar School","Liberty High","Grand Mountain Elementary","Long Beach School","Eagle Mountain Technical School","Tranquillity Middle School","Seal Bay Middle School","Clearwater School of Fine Arts","Horizon Elementary","Tranquillity Elementary")

df2 <- df %>% mutate(
    age = x+11,
    interv = x2,
    ABS = y,
    schoolid = factor(ng2, labels=snames),
    ppt = groups
) %>% select(schoolid, ppt, age, interv, ABS)

df2


df3 <- expand_grid(schoolppt = unique(paste0(df2$schoolid,"_",df2$ppt)), age = unique(df2$age)) %>%
    left_join(.,df2 %>% mutate(schoolppt = paste0(schoolid,"_",ppt))) %>% select(-schoolid, ppt) %>%
    separate(schoolppt, into=c("schoolid","ppt"), sep="_")


# write.csv(df3, "../../data/abs_intervention.csv",row.names=F)
