#Read data
data = read.csv('../Importable Data/IMDB_Ratings.csv',header=TRUE,sep = ",",dec = ".")

#### Equivalence Test Function----
TOST<-function(m1,m2,sd1,sd2,n1,n2,eqbound_d){
  sdpooled<-sqrt((((n1 - 1)*(sd1^2)) + (n2 - 1)*(sd2^2))/((n1+n2)-2)) #calculate sd pooled
  eqbound<-eqbound_d*sdpooled
  t1<-(abs(m1-m2)+eqbound)/(sdpooled*sqrt(1/n1 + 1/n2))
  p1<-pt(t1, n1+n2-2, lower=FALSE) #note use of abst(t1) because lower=false
  t2<-(abs(m1-m2)-eqbound)/(sdpooled*sqrt(1/n1 + 1/n2))
  p2<-pt(t2, n1+n2-2, lower=TRUE) #note use of -abst(t2) because lower=true
  LL<-(m1-m2)-qt(0.95, n1+n2-2)*(sdpooled*sqrt(1/n1 + 1/n2))
  UL<-(m1-m2)+qt(0.95, n1+n2-2)*(sdpooled*sqrt(1/n1 + 1/n2))
  t<-(m1-m2)/(sdpooled*sqrt(1/n1 + 1/n2))
  pttest<-2*pt(-abs(t), df=n1+n2-2)
  ptost<-max(p1,p2)
  results<-data.frame(ptost,LL,UL)
  dif<-(m1-m2)
  df = data.frame(labels=c("90% CI mean","Equivalence Range"), mean=c(dif,0), lower=c(LL,-eqbound), upper = c(UL,eqbound))
  plot(NA, xlim=c(.5,2.5), ylim=c(min(LL,-eqbound)-0.5, max(UL,eqbound)+0.5), bty="l", xaxt="n", xlab="",ylab="Mean Difference")
  points(df$mean[1:2], pch=19)
  points(1,(m1-m2)-qnorm(0.975)*(sdpooled*sqrt(1/n1 + 1/n2)),pch=10)
  points(1,(m1-m2)+qnorm(0.975)*(sdpooled*sqrt(1/n1 + 1/n2)),pch=10)
  axis(1, 1:2, df$labels)
  segments(1:2,df$lower[1:2],1:2,df$upper[1:2])
  segments(1:1,df$upper[1:1],1:2,df$upper[1:1],lty=3)
  segments(2,0,0,0,lty=2)
  segments(1:1,df$lower[1:1],1:2,df$lower[1:1],lty=3)
  text(2, min(LL,-eqbound)-0.2, paste("P-value",round(ptost, digits=3)), cex = .8)
  text(1, min(LL,-eqbound)-0.2, paste("P-value",round(pttest, digits=3)), cex = .8)
  text(1.5, dif, paste("Mdif = ",round(dif, digits=3)), cex = .8)
  title(main=paste("Mdif = ",round(dif,digits=3),", 95% CI [",round(((m1-m2)-qnorm(0.975)*(sdpooled*sqrt(1/n1 + 1/n2))),digits=3),";",round(((m1-m2)+qnorm(0.975)*(sdpooled*sqrt(1/n1 + 1/n2))),digits=3),"]",", p = ",round(pttest,digits=3), sep=""), cex.main=1)
  return(results)
}

#Store data from dataframe as 2 vectors with ratings for Pitt and Norton
datax<-data[data[, "Condition"] == "Marvel",]$Rating
datay<-data[data[, "Condition"] == "DC",]$Rating

#Perform Equivalence test and plot Figure 1
m1<-mean(datax) #mean Marvel
m2<-mean(datay) #mean DC
sd1<-sd(datax) #sd Marvel
sd2<-sd(datay) #sd DC
n1<-length(datax)
n2<-length(datay)
eqbound_d<-0.8 #significantly smaller than effect between control and LPD (not different from 0.64 for RPD)
TOST(m1=m1,m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2,eqbound_d=eqbound_d)

t.test(datax,datay, var.equal=TRUE) #Perform a normal t-test