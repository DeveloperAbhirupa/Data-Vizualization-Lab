job=read.csv("/home/abhirupa/Documents/Folder C/Dataset/Job.csv",sep=",",header=T)
job
job$Gender=factor(job$Gender,levels=c(1,2),labels=c("Male","Female")) #Converting text data to numneric format
job$Gender

job$Age=factor(job$Age,levels=c(1,2,3,4),labels=c("Upto 30","31-40","41-50","Above 50"))
job$Job=factor(job$Job,levels=c(1,2),labels=c("Full Time","Part Time"))
job$Exp=factor(job$Exp,levels=c(1,2,3,4),labels=c("Upto 6","6-15","16-25","Above 25"))
head(job)
attach(job)
for(i in 6:25)
{
  job[,i]=factor(job[,i],levels=c(1,2,3,4,5),labels=c("SD","D","N","A","SA"),ordered=TRUE)
}

job$EP=EP1+EP2+EP3+EP4
job$AC=AC1+AC2+AC3+AC4
job$OC=OC1+OC2+OC3+OC4
job$JS=JS1+JS2+JS3+JS4
job$SI=SI1+SI2+SI3+SI4
job$TOT=job$EP+job$AC+job$OC+job$JS+job$SI
summary(job$TOT)
job$Level=cut(job$TOT,c(0,61,79,94))
head(job$Level)
job$Level=factor(job$Level,labels=c("Low","Medium","High"))
table(job$Gender)
prop.table(table(job$Gender))
page=round(prop.table(table(job$Gender))*100,0)
barplot(prop.table(table(job$Gender)),main="Bar Plot of Gender Category",xlab="Gender",ylab="Percentage",col=c("red","blue"),horiz=FALSE)
table(job$Age)
prop.table(table(job$Age))
round(prop.table(table(job$Age))*100,0)
pie(prop.table(table(job$Age)),main="PIE Chart of Age Category",col=c("red","blue","yellow","orange"))
mjob=job[,c(6:25)]
mean(job$EP1)
sapply(mjob, function(cl) list(means=mean(cl,na.rm=TRUE), sds=sd(cl,na.rm=TRUE)))
meanandsd=as.data.frame( t(sapply(mjob, function(cl) list(means=mean(cl,na.rm=TRUE), 
                                                          sds=sd(cl,na.rm=TRUE))) ))
fjob=job[,c(26:31)]
meanandsdf=as.data.frame( t(sapply(fjob, function(cl) list(means=mean(cl,na.rm=TRUE), 
                                                           sds=sd(cl,na.rm=TRUE))) ))
table(job$Level)
prop.table(table(job$Level))
round(prop.table(table(job$Level))*100,0)
bb=barplot(prop.table(table(job$Level))*100,main="Bar Plot of Levels",xlab="Levels",ylab="Percentage",col=c("Black","Violet","Green"),horiz=FALSE)
text(bb,c(27,50,23),c("26","52","22"))
#To check normality condition

hist(EP1)
plot(density(EP1))
qqnorm(EP1)
qqline(EP1,col=5)
shapiro.test(EP1)
t.test(job$EP1, y = NULL, mu = 3, paired = FALSE, var.equal = FALSE, conf.level = 0.95) 

var.test(job$EP~job$Gender)
t.test(job$EP~job$Gender,   paired = FALSE, var.equal = TRUE, conf.level = 0.95)
aggregate(job$EP, by=list(job$Gender), mean)
aggregate(job$EP, by=list(job$Gender), sd)                


plot(job$EP ~ job$Age, data=job) 
ano=aov(job$EP ~ job$Age, data=job) 
summary(ano)
aggregate(job$EP, by=list(job$Age), mean)
aggregate(job$EP, by=list(job$Age), sd)

plot(job$AC ~ job$Age, data=job) 
anoa=aov(job$AC ~ job$Age, data=job) 
summary(anoa)
aggregate(job$AC, by=list(job$Age), mean)
aggregate(job$AC, by=list(job$Age), sd)

pairwise.t.test(job$AC, job$Age, p.adjust = "bonferroni") 
TukeyHSD(anoa, conf.level = 0.95) 
plot(TukeyHSD(anoa, conf.level = 0.95))

wilcox.test(job$EP ~ job$Job, job)
aggregate(job$EP, by=list(job$Job), mean)
aggregate(job$EP, by=list(job$Job), sd)
with(job, by(job$EP, job$Job, median))
with(job, by(job$EP, job$Job, mean))

kruskal.test(job$EP ~ job$Exp, job)
with(job, by(job$EP, job$Exp, median))
with(job, by(job$EP, job$Exp, mean))

kruskal.test(job$AC ~ job$Exp, job)
kruskal.test(job$OC ~ job$Exp, job)
kruskal.test(job$JS ~ job$Exp, job)
kruskal.test(job$SI ~ job$Exp, job)

install.packages(npmc)
library(npmc)
mydata <- as.data.frame(cbind(job$SI, job$Exp))
rm(job$SI, job$Exp)
library(npmc)
summary(npmc(mydata), type="BF")
??npmc


table(job$Level)
prop.table(table(job$Level))
obs=c(102,196,102)
exp=c(0.255,0.490,0.255)
chisq.test(obs,exp)





tbl=table(job$Gender,job$Level)
tbl
addmargins(tbl)
chisq.test(tbl)


cor(fjob)
round(cor(fjob),2)
cor.test(job$EP,job$AC ) 

install.packages(Hmisc)
library(Hmisc)
rcorr(as.matrix(fjob), type="pearson")
corrgram(fjob)
??corrgram
pairs(fjob)

t.test(job$TOT,job$EP,paired=TRUE)

pjob=as.matrix(fjob)
quade.test(pjob)
friedman.test(pjob)


cor(job$EP,job$SI)
lm(job$SI~job$EP)
plot(job$EP,job$SI)
abline(lm(job$SI~job$EP))

fit <- lm(weight ~ height, data=women)
plot(women$weight,women$height)

summary(fit)
fit2 <- lm(weight ~ height + I(height^2), data=women)
summary(fit2)

states <- data.frame(state.region, state.x77)
fit1 <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
summary(fit1)

fit3 <- lm(Murder ~ Population + Illiteracy, data=states)
summary(fit3)
anova(fit1,fit3)
par(mfrow=c(2,2))
plot(fit3)
confint(fit3)
head(states)
predict(fit3,data.frame(Population =2519,Illiteracy=1.8),interval="confidence")
predict(fit3,data.frame(Population =2519,Illiteracy=1.8),interval="prediction")
fitted.values(fit3)
fit3$residuals
q()
