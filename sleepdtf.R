set.seed(42367)

#just reading this in - i made it in excel as it was easier to see what i was doing :)
corm<-as.matrix(read.csv("sleepdtfcormat.csv",header=F))

mus <- c(8, rnorm(10, 2.5, .8))
stddev <- c(1.5, rnorm(10, 1, .2))
covMat <- stddev %*% t(stddev) * corm

covMat <- corpcor::make.positive.definite(covMat, tol=1e-3)

dat1 <- MASS::mvrnorm(n = 50, mu = mus, Sigma = covMat, empirical = TRUE)

dat1<-as.data.frame(cbind(dat1[,1], round(dat1[,2:11])))

names(dat1)<-c("TST",paste0("item_",1:10))

dat1<- dat1 %>% 
    mutate_at(vars(contains("item")),~ifelse(.>5,5,ifelse(.<1,1,.))) %>%
    mutate(TST = round(TST,1))

rm(corm,mus,stddev,covMat)

write.csv(dat1,"sleepdtf.csv",row.names = FALSE)