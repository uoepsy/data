set.seed(35967)
x1<- round(runif(20, 30,100))
x123 <- cbind(scale(x1),matrix(rnorm(40),ncol=2))

c1 <- var(x123)
chol1 <- solve(chol(c1))
newx <-  x123 %*% chol1 

covm<-matrix(c(1,.7,-.3,.7,1,.05,-.3,.05,1), ncol=3)
chol2 <- chol(covm)
finalx <- newx %*% chol2 * sd(x1) + mean(x1)
cor(finalx)

recalldata<-
    tibble(
        ppt = paste0("ppt_",1:20),
        recall_accuracy = finalx[,1],
        recall_confidence = round(finalx[,2],1)*0.8,
        age = round(finalx[,3])-20
    )

write.csv(recalldata,"recalldata.csv",row.names=F)
rm(x1,x123,c1,chol1,newx,covm,chol2,finalx)