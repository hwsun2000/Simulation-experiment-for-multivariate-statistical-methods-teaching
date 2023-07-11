##多重共线性对多重线性回归结果的影响	
install.packages("mvtnorm")#生成模拟数据
install.packages("car") #计算vif
library(mvtnorm)
library(car)
p=3  #产生p个自变量
n=100  #样本量
beta=c(1,-1,2)
###
c =0.1 #相关系数0.1,0.5,0.9,0.95,0.98,0.99,1  
sigma =matrix(c,3,3)#matrix用于生成矩阵3×3
sigma[1,1]= sigma[2,2]= sigma[3,3]=1 
x=rmvnorm(n,sigma=sigma) # 生成x矩阵
eps=rnorm(n,0,0.5)#eps误差项，rnorm用于产生服从正态分布的随机数，平均数0标准差0.5
y = x%*%beta+eps; #%*%将乘积汇总
x1=x[,1]
x2=x[,2]
x3=x[,3]
par(mfrow=c(3,3))
plot(x1,x1,cex=0.6)#cex=0.6调节点的大小
plot(x1,x2,cex=0.6)
plot(x1,x3,cex=0.6)
plot(x2,x1,cex=0.6)
plot(x2,x2,cex=0.6)
plot(x2,x3,cex=0.6)
plot(x3,x1,cex=0.6)
plot(x3,x2,cex=0.6)
plot(x3,x3,cex=0.6)
simu1<-as.data.frame(cbind(x,y))
#cbind按列合并矩阵，要求行数相同，data.frame数据框的创建
#因为lm数据集形式必须是数据框
colnames(simu1)<-c("x1","x2","x3","y")
#colnames变量命名
result1<-lm(y～x1+x2+x3,data=simu1)
#lm是线性模型函数
summary(result1)
vif(result1)
#vif方差膨胀因子
###
c =0.5 
sigma =matrix(c,3,3)
sigma[1,1]= sigma[2,2]= sigma[3,3]=1 
x=rmvnorm(n,sigma=sigma) 
eps=rnorm(n,0,0.5)
y = x%*%beta+eps; 
x1=x[,1]
x2=x[,2]
x3=x[,3]
par(mfrow=c(3,3))
plot(x1,x1,cex=0.6)#cex=0.6调节点的大小
plot(x1,x2,cex=0.6)
plot(x1,x3,cex=0.6)
plot(x2,x1,cex=0.6)
plot(x2,x2,cex=0.6)
plot(x2,x3,cex=0.6)
plot(x3,x1,cex=0.6)
plot(x3,x2,cex=0.6)
plot(x3,x3,cex=0.6)
simu1<-as.data.frame(cbind(x,y))
colnames(simu1)<-c("x1","x2","x3","y")
result1<-lm(y～x1+x2+x3,data=simu1)
summary(result1)
vif(result1)
###
c =0.9 
sigma =matrix(c,3,3)
sigma[1,1]= sigma[2,2]= sigma[3,3]=1 
x=rmvnorm(n,sigma=sigma) 
eps=rnorm(n,0,0.5)
y = x%*%beta+eps; 
x1=x[,1]
x2=x[,2]
x3=x[,3]
par(mfrow=c(3,3))
plot(x1,x1,cex=0.6)#cex=0.6调节点的大小
plot(x1,x2,cex=0.6)
plot(x1,x3,cex=0.6)
plot(x2,x1,cex=0.6)
plot(x2,x2,cex=0.6)
plot(x2,x3,cex=0.6)
plot(x3,x1,cex=0.6)
plot(x3,x2,cex=0.6)
plot(x3,x3,cex=0.6)
simu1<-as.data.frame(cbind(x,y))
colnames(simu1)<-c("x1","x2","x3","y")
result1<-lm(y～x1+x2+x3,data=simu1)
summary(result1)
vif(result1)
###
c =0.95 
sigma =matrix(c,3,3)
sigma[1,1]= sigma[2,2]= sigma[3,3]=1 
x=rmvnorm(n,sigma=sigma) 
eps=rnorm(n,0,0.5)
y = x%*%beta+eps; 
x1=x[,1]
x2=x[,2]
x3=x[,3]
par(mfrow=c(3,3))
plot(x1,x1,cex=0.6)#cex=0.6调节点的大小
plot(x1,x2,cex=0.6)
plot(x1,x3,cex=0.6)
plot(x2,x1,cex=0.6)
plot(x2,x2,cex=0.6)
plot(x2,x3,cex=0.6)
plot(x3,x1,cex=0.6)
plot(x3,x2,cex=0.6)
plot(x3,x3,cex=0.6)
simu1<-as.data.frame(cbind(x,y))
colnames(simu1)<-c("x1","x2","x3","y")
result1<-lm(y～x1+x2+x3,data=simu1)
summary(result1)
vif(result1)
###
c =0.98 
sigma =matrix(c,3,3)
sigma[1,1]= sigma[2,2]= sigma[3,3]=1 
x=rmvnorm(n,sigma=sigma) 
eps=rnorm(n,0,0.5)
y = x%*%beta+eps; 
x1=x[,1]
x2=x[,2]
x3=x[,3]
par(mfrow=c(3,3))
plot(x1,x1,cex=0.6)#cex=0.6调节点的大小
plot(x1,x2,cex=0.6)
plot(x1,x3,cex=0.6)
plot(x2,x1,cex=0.6)
plot(x2,x2,cex=0.6)
plot(x2,x3,cex=0.6)
plot(x3,x1,cex=0.6)
plot(x3,x2,cex=0.6)
plot(x3,x3,cex=0.6)
simu1<-as.data.frame(cbind(x,y))
colnames(simu1)<-c("x1","x2","x3","y")
result1<-lm(y～x1+x2+x3,data=simu1)
summary(result1)
vif(result1)
###
c =0.99 
sigma =matrix(c,3,3)
sigma[1,1]= sigma[2,2]= sigma[3,3]=1 
x=rmvnorm(n,sigma=sigma) 
eps=rnorm(n,0,0.5)
y = x%*%beta+eps; 
x1=x[,1]
x2=x[,2]
x3=x[,3]
par(mfrow=c(3,3))
plot(x1,x1,cex=0.6)#cex=0.6调节点的大小
plot(x1,x2,cex=0.6)
plot(x1,x3,cex=0.6)
plot(x2,x1,cex=0.6)
plot(x2,x2,cex=0.6)
plot(x2,x3,cex=0.6)
plot(x3,x1,cex=0.6)
plot(x3,x2,cex=0.6)
plot(x3,x3,cex=0.6)
simu1<-as.data.frame(cbind(x,y))
colnames(simu1)<-c("x1","x2","x3","y")
result1<-lm(y～x1+x2+x3,data=simu1)
summary(result1)
vif(result1)
###
c =1 
sigma =matrix(c,3,3)
sigma[1,1]= sigma[2,2]= sigma[3,3]=1 
x=rmvnorm(n,sigma=sigma) 
eps=rnorm(n,0,0.5)
y = x%*%beta+eps; 
simu1<-as.data.frame(cbind(x,y))
colnames(simu1)<-c("x1","x2","x3","y")
result1<-lm(y～x1+x2+x3,data=simu1)
summary(result1)
vif(result1)

par(mfrow = c(2, 2))
#par用于设置或查询图形参数。图形参数非常，大多数可以在作图函数中设置
#Par(mfrow=c(1,2)):一行两列；Par(mfrow=c(2,1)):两行一列
###x1的系数，从相关系数0.9开始画。
x0=c(0.9,0.95,0.98,0.99)
y0=c(0,0.7,1.4,2)
beta0=rep(1,4)  ##x1的真实系数
beta1=c(0.98,0.93,0.86,1.69)  # x1在不同相关关系r下求得的系数
beta2=c(-0.97,-1.14,-0.56,-2.52)  # x2在不同相关关系r下求得的系数
beta3=c(1.93,2.32,1.69,2.82)  # x3在不同相关关系r下求得的系数
plot(x0,y0,main = "x1的系数随变量相关程度r变化",xlab="自变量间的相关系数",ylab="系数",type="n",font.main=3,font.lab=3)
#main:主标题；type=n表示无点和无线条；font.main:指定标题字体为普通字体
points(x=x0,y=beta0,lty=3,pch=1,lwd=2,col = "red")  
lines(x=x0,y=beta0,lty=3,pch=1,lwd=2,col = "red") 
points(x=x0,y=beta1,lty=3,pch=1,lwd=2,col = "black")  
lines(x=x0,y=beta1,lty=3,pch=1,lwd=2,col = "black")
legend("bottomleft",legend=c("真实值","估计值"),xpd=T,cex=0.8,pch=1,lty=3,col=c("red","black"),bty="n")

###x2的系数，从相关系数0.9开始画。
x0=c(0.9,0.95,0.98,0.99)
y0=c(-0,-1,-2,-3)
beta0=rep(-1,4)  
beta1=c(0.98,0.93,0.86,1.69)  
beta2=c(-0.97,-1.14,-0.56,-2.52)  
beta3=c(1.93,2.32,1.69,2.82)  
plot(x0,y0,main = "x2的系数随变量相关程度r变化",xlab="自变量间的相关系数",ylab="系数",type="n",font.main=3,font.lab=3)
points(x=x0,y=beta0,lty=3,pch=1,lwd=2,col = "red")  
lines(x=x0,y=beta0,lty=3,pch=1,lwd=2,col = "red") 
points(x=x0,y=beta2,lty=3,pch=1,lwd=2,col = "black")  
lines(x=x0,y=beta2,lty=3,pch=1,lwd=2,col = "black")
legend("bottomleft",legend=c("真实值","估计值"),xpd=T,cex=0.8,pch=1,lty=3,col=c("red","black"),bty="n")

###x3的系数，从相关系数0.9开始画。
x0=c(0.9,0.95,0.98,0.99)
y0=c(0,1,2,3)
beta0=rep(2,4)  
beta1=c(0.98,0.93,0.86,1.69)  
beta2=c(-0.97,-1.14,-0.56,-2.52)  
beta3=c(1.93,2.32,1.69,2.82)  
####变量间不同相关模式对主成分分析结果的影响
install.packages("mvtnorm")#
install.packages("psych")#barlett KMO
library(mvtnorm)
library(psych)
p=4  
n=100  
#赋值a=b=c=0.1,0.5,0.9  
#情景1
a =0.1 
b=0.1
c=0.1
sigma<-c(1,a,c,c,a,1,c,c,c,c,1,b,c,c,b,1)
sigma =matrix(sigma,4,4)
x=rmvnorm(n,sigma=sigma) # 
x<-as.data.frame(x)
KMO(x)
colnames(x)<-c("x1","x2","x3","x4")
result<-princomp(x)#主成分分析
summary(result)
r <- corr.test(x)$r # 
fit<- fa(r, nfactors=1, rotate="varimax",fm="pa") 
print(fit, digits=2, cutoff=0.3, sort=TRUE) 
x1=x[,1]
x2=x[,2]
x3=x[,3]
x4=x[,4]
par(mfrow=c(4,4),mar=c(3,3,1,1)+0.1,mgp=c(1.2,0.5,0))
plot(x1,x1,cex=0.6)#cex=0.6调节点的大小
plot(x1,x2,cex=0.6)
plot(x1,x3,cex=0.6)
plot(x1,x4,cex=0.6)
plot(x2,x1,cex=0.6)
plot(x2,x2,cex=0.6)
plot(x2,x3,cex=0.6)
plot(x2,x4,cex=0.6)
plot(x3,x1,cex=0.6)
plot(x3,x2,cex=0.6)
plot(x3,x3,cex=0.6)
plot(x3,x4,cex=0.6)
plot(x4,x1,cex=0.6)
plot(x4,x2,cex=0.6)
plot(x4,x3,cex=0.6)
plot(x4,x4,cex=0.6)
##情景2
a =0.5 
b=0.5
c=0.5
sigma<-c(1,a,c,c,a,1,c,c,c,c,1,b,c,c,b,1)
sigma =matrix(sigma,4,4)
x=rmvnorm(n,sigma=sigma) # 
x<-as.data.frame(x)
KMO(x)
colnames(x)<-c("x1","x2","x3","x4")
result<-princomp(x)#主成分分析
summary(result)
r <- corr.test(x)$r # 
fit<- fa(r, nfactors=1, rotate="varimax",fm="pa") 
print(fit, digits=2, cutoff=0.3, sort=TRUE) 
x1=x[,1]
x2=x[,2]
x3=x[,3]
x4=x[,4]
par(mfrow=c(4,4),mar=c(3,3,1,1)+0.1,mgp=c(1.2,0.5,0))
plot(x1,x1,cex=0.6)#cex=0.6调节点的大小
plot(x1,x2,cex=0.6)
plot(x1,x3,cex=0.6)
plot(x1,x4,cex=0.6)
plot(x2,x1,cex=0.6)
plot(x2,x2,cex=0.6)
plot(x2,x3,cex=0.6)
plot(x2,x4,cex=0.6)
plot(x3,x1,cex=0.6)
plot(x3,x2,cex=0.6)
plot(x3,x3,cex=0.6)
plot(x3,x4,cex=0.6)
plot(x4,x1,cex=0.6)
plot(x4,x2,cex=0.6)
plot(x4,x3,cex=0.6)
plot(x4,x4,cex=0.6)
##情景3
a =0.9 
b=0.9
c=0.9
sigma<-c(1,a,c,c,a,1,c,c,c,c,1,b,c,c,b,1)
sigma =matrix(sigma,4,4)
x=rmvnorm(n,sigma=sigma) # 
x<-as.data.frame(x)
KMO(x)
colnames(x)<-c("x1","x2","x3","x4")
result<-princomp(x)#主成分分析
summary(result)
r <- corr.test(x)$r # 
fit<- fa(r, nfactors=1, rotate="varimax",fm="pa") 
print(fit, digits=2, cutoff=0.3, sort=TRUE) 
x1=x[,1]
x2=x[,2]
x3=x[,3]
x4=x[,4]
par(mfrow=c(4,4),mar=c(3,3,1,1)+0.1,mgp=c(1.2,0.5,0))
plot(x1,x1,cex=0.6)#cex=0.6调节点的大小
plot(x1,x2,cex=0.6)
plot(x1,x3,cex=0.6)
plot(x1,x4,cex=0.6)
plot(x2,x1,cex=0.6)
plot(x2,x2,cex=0.6)
plot(x2,x3,cex=0.6)
plot(x2,x4,cex=0.6)
plot(x3,x1,cex=0.6)
plot(x3,x2,cex=0.6)
plot(x3,x3,cex=0.6)
plot(x3,x4,cex=0.6)
plot(x4,x1,cex=0.6)
plot(x4,x2,cex=0.6)
plot(x4,x3,cex=0.6)
plot(x4,x4,cex=0.6)
#情景4  
a =0.5 
b=0.5
c=0.1
sigma<-c(1,a,c,c,a,1,c,c,c,c,1,b,c,c,b,1)
sigma =matrix(sigma,4,4)
x=rmvnorm(n,sigma=sigma) # 
x<-as.data.frame(x)
KMO(x)
colnames(x)<-c("x1","x2","x3","x4")
result<-princomp(x)
summary(result)
r <- corr.test(x)$r # 
fit<- fa(r, nfactors=2, rotate="varimax",fm="pa") 
print(fit, digits=2, cutoff=0.3, sort=TRUE) 
x1=x[,1]
x2=x[,2]
x3=x[,3]
x4=x[,4]
par(mfrow=c(4,4),mar=c(3,3,1,1)+0.1,mgp=c(1.2,0.5,0))
plot(x1,x1,cex=0.6)#cex=0.6调节点的大小
plot(x1,x2,cex=0.6)
plot(x1,x3,cex=0.6)
plot(x1,x4,cex=0.6)
plot(x2,x1,cex=0.6)
plot(x2,x2,cex=0.6)
plot(x2,x3,cex=0.6)
plot(x2,x4,cex=0.6)
plot(x3,x1,cex=0.6)
plot(x3,x2,cex=0.6)
plot(x3,x3,cex=0.6)
plot(x3,x4,cex=0.6)
plot(x4,x1,cex=0.6)
plot(x4,x2,cex=0.6)
plot(x4,x3,cex=0.6)
plot(x4,x4,cex=0.6)
#情景5
a =0.9 
b=0.9
c=0.1
sigma<-c(1,a,c,c,a,1,c,c,c,c,1,b,c,c,b,1)
sigma =matrix(sigma,4,4)
x=rmvnorm(n,sigma=sigma) # 
x<-as.data.frame(x)
KMO(x)
colnames(x)<-c("x1","x2","x3","x4")
result<-princomp(x)
summary(result)
r <- corr.test(x)$r # 
fit<- fa(r, nfactors=2, rotate="varimax",fm="pa") 
print(fit, digits=2, cutoff=0.3, sort=TRUE) 
x1=x[,1]
x2=x[,2]
x3=x[,3]
x4=x[,4]
par(mfrow=c(4,4),mar=c(3,3,1,1)+0.1,mgp=c(1.2,0.5,0))
plot(x1,x1,cex=0.6)#cex=0.6调节点的大小
plot(x1,x2,cex=0.6)
plot(x1,x3,cex=0.6)
plot(x1,x4,cex=0.6)
plot(x2,x1,cex=0.6)
plot(x2,x2,cex=0.6)
plot(x2,x3,cex=0.6)
plot(x2,x4,cex=0.6)
plot(x3,x1,cex=0.6)
plot(x3,x2,cex=0.6)
plot(x3,x3,cex=0.6)
plot(x3,x4,cex=0.6)
plot(x4,x1,cex=0.6)
plot(x4,x2,cex=0.6)
plot(x4,x3,cex=0.6)
plot(x4,x4,cex=0.6)
####自变量对因变量非线性影响下，线性拟合和非线性拟合对Logsitic回归结果的影响
library(mvtnorm)
n=500
sigma =matrix(1,1,1)
x=rnorm(n,2,1)
x0<-matrix(c(x,x^2),n,2)
beta<-c(-3,1)
feta=x0%*%beta;
fprob=exp(feta)/(1+exp(feta))
y=rbinom(n,1,fprob)#二项分布rbinom(n, 试验的数量, 概率向量)
par(mfrow = c(1, 1))
plot(x,y,main = "散点图",xlab="产妇年龄",ylab="低体重儿",type="n",font.main=3,font.lab=3)
#main:主标题；type=n表示无点和无线条；font.main:指定标题字体为普通字体
points(x=x,y=y,lty=3,pch=1,lwd=2,col = "red")  
legend("bottomleft",legend=c("观测值"),cex=0.8,pch=1,lty=3,col=c("red"),bty="n")
#线性回归
glm1<-glm(formula=y～x,family=binomial(link = "logit"))#logistic
summary(glm1)
#非线性回归
glm2<-glm(formula=y～x+I(x^2),family=binomial(link = "logit"))#logistic
summary(glm2)
#OR值比较
ORx=exp(0.7617)
OR2<-c(exp(-3.6318+(2%*%x+1)*1.2489))
OR0<-c(exp(-3+(2%*%x+1)*1))
OR1=rep(ORx,100)  
standard=rep(1,100) 
plot(x0,y0,main = "线性拟合与非线性拟合的OR值比较",xlab="x",ylab="OR值",type="n",font.main=3,font.lab=3)
points(x=x,y=OR1,lty=3,pch=1,lwd=2,col = "yellow")  
points(x=x,y=OR2,lty=3,pch=1,lwd=2,col = "red")  
points(x=x,y=OR0,lty=3,pch=1,lwd=2,col = "blue")
lines(x=x,y=standard,type="l",lty=3,pch=1,lwd=4,col = "black") 
legend("topright",legend=c("线性拟合","非线性拟合","真实OR=2.14","OR=1"),cex=0.8,pch=1,lty=3,col=c("yellow","red","blue","black"),bty="n")
#ROC曲线
install.packages("pROC")
install.packages("ggplot2")
library(pROC)
library(ggplot2)
data<-data.frame(x)
par(mfrow = c(2, 2))
p01=predict(glm1,newdata=data,type="response")#type＝response，表示输出结果预测Y=1的概率
roc1<-data.frame(cbind(x,y,p01))
p1=roc(y～p01,roc1)
p02=predict(glm2,newdata=data,type="response")
roc2<-data.frame(cbind(x,y,p02))
p2=roc(y～p02,roc2)
plot(p1,,smooth=T,grid.col=c("black", "black"))
plot(p2,add=T,col="red")
legend("bottomright",legend=c("线性模型AUC=0.727","非线性模型AUC=0.881"),cex=1.2,pch=1,lty=1,col=c("black","red"),bty="n")
###不同删失比例对Cox比例风险模型结果的影响	
install.packages("mvtnorm")#生成模拟数据
install.packages("survival") 
library(mvtnorm)
library(survival)
n=1000  
p=2  
#删失比例70%:CL=0,CU=1
CL<- 0
CU<- 1
beta=c(1,-1)
   ssigma =0.7   #相关系数
       Sigma =ssigma^t(sapply(1:p, function(i, j) abs(i-j), 1:p)) 
       x=rmvnorm(n, sigma=Sigma) # 生成x矩阵  
       ht=exp(x%*%beta)  #得到风险函数
      	 #observed survivial time生存时间
logS=log(matrix(runif(n,0,1),n,1)) #log[S(t)]
       #runif生成0到1之间符合均匀分布的随机数，matrix生成100*1的矩阵
	 T=-logS/ht  #survival time生存时间
	#censored time删失时间
	 myrate <- matrix(runif(n,CL,CU),n,1)/ht
	 C <- rexp(n, rate = 1/myrate)
     	#survival time and state
	 time <- apply(cbind(C,T),1,min)  ##time是生存时间
      #cbind:按列合并矩阵，要求行数相同
      #apply:按矩阵的行或列方向应用指定函数
      t<-time
status <- ifelse(T<C,1,0) #stat指示了是否删失,等于1表示事件发生，等于0表示截尾发生
 #sum(status)
df<-as.data.frame(cbind(x,T,C,status))
colnames(df)<-c("x1","x2","time","C","status")
fit.cox<-coxph(Surv(time,status)～x1+x2,data=df)
summary(fit.cox)
#删失比例60%:CL=0.5,CU=1
CL<- 0.5
CU<- 1
beta=c(1,-1)
   ssigma =0.7   #相关系数
       Sigma =ssigma^t(sapply(1:p, function(i, j) abs(i-j), 1:p)) 
       x=rmvnorm(n, sigma=Sigma) # 生成x矩阵  
       ht=exp(x%*%beta)  #得到风险函数
      	 #observed survivial time生存时间
logS=log(matrix(runif(n,0,1),n,1)) #log[S(t)]
       #runif生成0到1之间符合均匀分布的随机数，matrix生成100*1的矩阵
	 T=-logS/ht  #survival time生存时间
	#censored time删失时间
	 myrate <- matrix(runif(n,CL,CU),n,1)/ht
	 C <- rexp(n, rate = 1/myrate)
     	#survival time and state
	 time <- apply(cbind(C,T),1,min)  ##time是生存时间
      #cbind:按列合并矩阵，要求行数相同
      #apply:按矩阵的行或列方向应用指定函数
      t<-time
status <- ifelse(T<C,1,0) #stat指示了是否删失,等于1表示事件发生，等于0表示截尾发生
 #sum(status)
df<-as.data.frame(cbind(x,T,C,status))
colnames(df)<-c("x1","x2","time","C","status")
fit.cox<-coxph(Surv(time,status)～x1+x2,data=df)
summary(fit.cox)
#删失比例50%:CL=1,CU=1.1
CL<- 1
CU<- 1.1
beta=c(1,-1)
   ssigma =0.7   #相关系数
       Sigma =ssigma^t(sapply(1:p, function(i, j) abs(i-j), 1:p)) 
       x=rmvnorm(n, sigma=Sigma) # 生成x矩阵  
       ht=exp(x%*%beta)  #得到风险函数
      	 #observed survivial time生存时间
logS=log(matrix(runif(n,0,1),n,1)) #log[S(t)]
       #runif生成0到1之间符合均匀分布的随机数，matrix生成100*1的矩阵
	 T=-logS/ht  #survival time生存时间
	#censored time删失时间
	 myrate <- matrix(runif(n,CL,CU),n,1)/ht
	 C <- rexp(n, rate = 1/myrate)
     	#survival time and state
	 time <- apply(cbind(C,T),1,min)  ##time是生存时间
      #cbind:按列合并矩阵，要求行数相同
      #apply:按矩阵的行或列方向应用指定函数
      t<-time
status <- ifelse(T<C,1,0) #stat指示了是否删失,等于1表示事件发生，等于0表示截尾发生
 #sum(status)
df<-as.data.frame(cbind(x,T,C,status))
colnames(df)<-c("x1","x2","time","C","status")
fit.cox<-coxph(Surv(time,status)～x1+x2,data=df)
summary(fit.cox)
#删失比例40%:CL=1,CU=2
CL<- 1
CU<- 2
beta=c(1,-1)
   ssigma =0.7   #相关系数
       Sigma =ssigma^t(sapply(1:p, function(i, j) abs(i-j), 1:p)) 
       x=rmvnorm(n, sigma=Sigma) # 生成x矩阵  
       ht=exp(x%*%beta)  #得到风险函数
      	 #observed survivial time生存时间
logS=log(matrix(runif(n,0,1),n,1)) #log[S(t)]
       #runif生成0到1之间符合均匀分布的随机数，matrix生成100*1的矩阵
	 T=-logS/ht  #survival time生存时间
	#censored time删失时间
	 myrate <- matrix(runif(n,CL,CU),n,1)/ht
	 C <- rexp(n, rate = 1/myrate)
     	#survival time and state
	 time <- apply(cbind(C,T),1,min)  ##time是生存时间
      #cbind:按列合并矩阵，要求行数相同
      #apply:按矩阵的行或列方向应用指定函数
      t<-time
status <- ifelse(T<C,1,0) #stat指示了是否删失,等于1表示事件发生，等于0表示截尾发生
 #sum(status)
df<-as.data.frame(cbind(x,T,C,status))
colnames(df)<-c("x1","x2","time","C","status")
fit.cox<-coxph(Surv(time,status)～x1+x2,data=df)
summary(fit.cox)
#删失比例30%:CL=1,CU=4
CL<- 1
CU<- 4
beta=c(1,-1)
   ssigma =0.7   #相关系数
       Sigma =ssigma^t(sapply(1:p, function(i, j) abs(i-j), 1:p)) 
       x=rmvnorm(n, sigma=Sigma) # 生成x矩阵  
       ht=exp(x%*%beta)  #得到风险函数
      	 #observed survivial time生存时间
logS=log(matrix(runif(n,0,1),n,1)) #log[S(t)]
       #runif生成0到1之间符合均匀分布的随机数，matrix生成100*1的矩阵
	 T=-logS/ht  #survival time生存时间
	#censored time删失时间
	 myrate <- matrix(runif(n,CL,CU),n,1)/ht
	 C <- rexp(n, rate = 1/myrate)
     	#survival time and state
	 time <- apply(cbind(C,T),1,min)  ##time是生存时间
      #cbind:按列合并矩阵，要求行数相同
      #apply:按矩阵的行或列方向应用指定函数
      t<-time
status <- ifelse(T<C,1,0) #stat指示了是否删失,等于1表示事件发生，等于0表示截尾发生
 #sum(status)
df<-as.data.frame(cbind(x,T,C,status))
colnames(df)<-c("x1","x2","time","C","status")
fit.cox<-coxph(Surv(time,status)～x1+x2,data=df)
summary(fit.cox)
#删失比例20%:CL=3,CU=5
CL<- 3
CU<- 5
beta=c(1,-1)
   ssigma =0.7   #相关系数
       Sigma =ssigma^t(sapply(1:p, function(i, j) abs(i-j), 1:p)) 
       x=rmvnorm(n, sigma=Sigma) # 生成x矩阵  
       ht=exp(x%*%beta)  #得到风险函数
      	 #observed survivial time生存时间
logS=log(matrix(runif(n,0,1),n,1)) #log[S(t)]
       #runif生成0到1之间符合均匀分布的随机数，matrix生成100*1的矩阵
	 T=-logS/ht  #survival time生存时间
	#censored time删失时间
	 myrate <- matrix(runif(n,CL,CU),n,1)/ht
	 C <- rexp(n, rate = 1/myrate)
     	#survival time and state
	 time <- apply(cbind(C,T),1,min)  ##time是生存时间
      #cbind:按列合并矩阵，要求行数相同
      #apply:按矩阵的行或列方向应用指定函数
      t<-time
status <- ifelse(T<C,1,0) #stat指示了是否删失,等于1表示事件发生，等于0表示截尾发生
 #sum(status)
df<-as.data.frame(cbind(x,T,C,status))
colnames(df)<-c("x1","x2","time","C","status")
fit.cox<-coxph(Surv(time,status)～x1+x2,data=df)
summary(fit.cox)
#删失比例10%:CL=8.9,CU=9
CL<- 8.9
CU<- 9
beta=c(1,-1)
   ssigma =0.7   #相关系数
       Sigma =ssigma^t(sapply(1:p, function(i, j) abs(i-j), 1:p)) 
       x=rmvnorm(n, sigma=Sigma) # 生成x矩阵  
       ht=exp(x%*%beta)  #得到风险函数
      	 #observed survivial time生存时间
logS=log(matrix(runif(n,0,1),n,1)) #log[S(t)]
       #runif生成0到1之间符合均匀分布的随机数，matrix生成100*1的矩阵
	 T=-logS/ht  #survival time生存时间
	#censored time删失时间
	 myrate <- matrix(runif(n,CL,CU),n,1)/ht
	 C <- rexp(n, rate = 1/myrate)
     	#survival time and state
	 time <- apply(cbind(C,T),1,min)  ##time是生存时间
      #cbind:按列合并矩阵，要求行数相同
      #apply:按矩阵的行或列方向应用指定函数
      t<-time
status <- ifelse(T<C,1,0) #stat指示了是否删失,等于1表示事件发生，等于0表示截尾发生
 #sum(status)
df<-as.data.frame(cbind(x,T,C,status))
colnames(df)<-c("x1","x2","time","C","status")
fit.cox<-coxph(Surv(time,status)～x1+x2,data=df)
summary(fit.cox)
#删失比例5%:CL=18,CU=19
CL<- 18
CU<- 19
beta=c(1,-1)
   ssigma =0.7   #相关系数
       Sigma =ssigma^t(sapply(1:p, function(i, j) abs(i-j), 1:p)) 
       x=rmvnorm(n, sigma=Sigma) # 生成x矩阵  
       ht=exp(x%*%beta)  #得到风险函数
      	 #observed survivial time生存时间
logS=log(matrix(runif(n,0,1),n,1)) #log[S(t)]
       #runif生成0到1之间符合均匀分布的随机数，matrix生成100*1的矩阵
	 T=-logS/ht  #survival time生存时间
	#censored time删失时间
	 myrate <- matrix(runif(n,CL,CU),n,1)/ht
	 C <- rexp(n, rate = 1/myrate)
     	#survival time and state
	 time <- apply(cbind(C,T),1,min)  ##time是生存时间
      #cbind:按列合并矩阵，要求行数相同
      #apply:按矩阵的行或列方向应用指定函数
      t<-time
status <- ifelse(T<C,1,0) #stat指示了是否删失,等于1表示事件发生，等于0表示截尾发生
 #sum(status)
df<-as.data.frame(cbind(x,T,C,status))
colnames(df)<-c("x1","x2","time","C","status")
fit.cox<-coxph(Surv(time,status)～x1+x2,data=df)
summary(fit.cox)

par(mfrow = c(2, 2))
###画图：x1的系数。
x0=c(0.10,0.20,0.30,0.40,0.5,0.6,0.7,0.8)
y0=c(0,0.2,0.4,0.6,0.8,1,1.2,1.4)
beta0=rep(1,8)  ##x1的真实系数 
beta1=c(0.931,0.898,0.816,0.732,0.730,0.656,0.501,0.482)  #x1在不同相删失比例下求得的系数
plot(x0,y0,main = "x1",xlab="生存数据的删失比例",ylab="系数",type="n",font.main=3,font.lab=3)
points(x=x0,y=beta0,lty=3,pch=1,lwd=2,col = "black")  
lines(x=x0,y=beta0,lty=3,pch=1,lwd=2,col = "black") 
points(x=x0,y=beta1,lty=3,pch=1,lwd=2,col = "red")  
lines(x=x0,y=beta1,lty=3,pch=1,lwd=2,col = "red")
legend("bottomleft",legend=c("真实值","估计值"),cex=0.8,pch=1,lty=3,col=c("black","red"),bty="n")
###画图：x2的系数。
x0=c(0.10,0.20,0.30,0.40,0.5,0.6,0.7,0.8)
y0=c(-0.2,-0.4,-0.6,-0.8,-1,-1.2,-1.4,-1.6)
beta0=rep(-1,8)  ##x2的真实系数
beta2=c(-0.924,-0.924,-0.806,-0.751,-0.680,-0.676,-0.515,-0.593)  # x2在不同删失比例下求得的系数
plot(x0,y0,main = "x2",xlab="生存数据的删失比例",ylab="系数",type="n",font.main=3,font.lab=3)
points(x=x0,y=beta0,lty=3,pch=1,lwd=2,col = "black")  
lines(x=x0,y=beta0,lty=3,pch=1,lwd=2,col = "black") 
points(x=x0,y=beta2,lty=3,pch=1,lwd=2,col = "red")  
lines(x=x0,y=beta2,lty=3,pch=1,lwd=2,col = "red")
legend("bottomleft",legend=c("真实值","估计值"),cex=0.8,pch=1,lty=3,col=c("black","red"),bty="n")
HR11=exp(1)
HR12=exp(0.931)
HR13=exp(0.898)
HR14=exp(0.816)
HR15=exp(0.732)
HR16=exp(0.730)
HR17=exp(0.656)
HR18=exp(0.501)
HR19=exp(0.482)
HR21=exp(-1)
HR22=exp(-0.924)
HR23=exp(-0.924)
HR24=exp(-0.806)
HR25=exp(-0.751)
HR26=exp(-0.680)
HR27=exp(-0.676)
HR28=exp(-0.515)
HR29=exp(-0.593)


###变量间不同相关结构对典型相关分析结果的影响
install.packages("mvtnorm")
library(mvtnorm)
p=4  
n=100  
#创建函数corcoef.test以求所需典则变量数量
corcoef.test<-function(r, n, p, q, alpha=0.1){
m<-length(r); Q<-rep(0, m); lambda <- 1
for (k in m:1){
lambda<-lambda*(1-r[k]^2);
Q[k]<- -log(lambda)
}
s<-0; i<-m
for (k in 1:m){
Q[k]<- (n-k+1-1/2*(p+q+3)+s)*Q[k]
chi<-1-pchisq(Q[k], (p-k+1)*(q-k+1))
if (chi>alpha){
i<-k-1; break
}
s<-s+1/r[k]^2
}
i
}
##情景1 
a =0.1 #
b=0.1
c=0.1
sigma<-c(1,a,c,c,a,1,c,c,c,c,1,b,c,c,b,1)
sigma =matrix(sigma,4,4)
x=rmvnorm(n,sigma=sigma) 
test<-data.frame(x)
colnames(test)<-c("x1","x2","y1","y2")
ca<-cancor(test[,1:2],test[,3:4])
corcoef.test(r=ca$cor, n=100, p=2, q=2)
#计算数据在典型变量下的得分 U=AX  V=BY
U<-as.matrix(test[, 1:2])%*% ca$xcoef   
V<-as.matrix(test[, 3:4])%*% ca$ycoef
#画出U1、V1和U2、V2为组表的数据散点图
par(mfrow = c(2, 2))
plot(U[,1], V[,1], xlab="U1", ylab="V1")
plot(U[,2], V[,2], xlab="U2", ylab="V2")
##情景2
a =0.5 # 
b=0.5
c=0.5
sigma<-c(1,a,c,c,a,1,c,c,c,c,1,b,c,c,b,1)
sigma =matrix(sigma,4,4)
x=rmvnorm(n,sigma=sigma) 
test<-data.frame(x)
colnames(test)<-c("x1","x2","y1","y2")
ca<-cancor(test[,1:2],test[,3:4])
corcoef.test(r=ca$cor, n=100, p=2, q=2)
U<-as.matrix(test[, 1:2])%*% ca$xcoef   
V<-as.matrix(test[, 3:4])%*% ca$ycoef
par(mfrow = c(2, 2))
plot(U[,1], V[,1], xlab="U1", ylab="V1")
plot(U[,2], V[,2], xlab="U2", ylab="V2")
##情景3
a =0.9# 
b=0.9
c=0.9
sigma<-c(1,a,c,c,a,1,c,c,c,c,1,b,c,c,b,1)
sigma =matrix(sigma,4,4)
x=rmvnorm(n,sigma=sigma) 
test<-data.frame(x)
colnames(test)<-c("x1","x2","y1","y2")
ca<-cancor(test[,1:2],test[,3:4])
corcoef.test(r=ca$cor, n=100, p=2, q=2)
U<-as.matrix(test[, 1:2])%*% ca$xcoef   
V<-as.matrix(test[, 3:4])%*% ca$ycoef
par(mfrow = c(2, 2))
plot(U[,1], V[,1], xlab="U1", ylab="V1")
plot(U[,2], V[,2], xlab="U2", ylab="V2")
####类别比例不同对距离判别和Bayes判别、Logistic回归结果的影响
install.packages("ggplot2")
library(ggplot2)
#创建距离判别函数
discriminiant.distance <- function
(TrnX1, TrnX2, TstX = NULL, var.equal = FALSE){
if (is.null(TstX) == TRUE) TstX <- rbind(TrnX1,TrnX2)
if (is.vector(TstX) == TRUE) TstX <- t(as.matrix(TstX))
else if (is.matrix(TstX) != TRUE)
TstX <- as.matrix(TstX)
if (is.matrix(TrnX1) != TRUE) TrnX1 <- as.matrix(TrnX1)
if (is.matrix(TrnX2) != TRUE) TrnX2 <- as.matrix(TrnX2)
nx <- nrow(TstX)
blong <- matrix(rep(0, nx), nrow=1, byrow=TRUE,
dimnames=list("blong", 1:nx))
mu1 <- colMeans(TrnX1); mu2 <- colMeans(TrnX2)
if (var.equal == TRUE || var.equal == T){
448 # $ ? ? ? ? " ? ? (I)
S <- var(rbind(TrnX1,TrnX2))
w <- mahalanobis(TstX, mu2, S)
- mahalanobis(TstX, mu1, S)
}
else{
S1 < -var(TrnX1); S2 <- var(TrnX2)
w <- mahalanobis(TstX, mu2, S2)
- mahalanobis(TstX, mu1, S1)
}
for (i in 1:nx){
if (w[i] > 0)
blong[i] <- 1
else
blong[i] <- 2
}
blong
}
#创建贝叶斯判别函数
discriminiant.bayes <- function(TrnX1, TrnX2, rate = 1, TstX = NULL, var.equal = FALSE){
if (is.null(TstX) == TRUE) TstX<-rbind(TrnX1,TrnX2)
if (is.vector(TstX) == TRUE) TstX <- t(as.matrix(TstX))
else if (is.matrix(TstX) != TRUE)
TstX <- as.matrix(TstX)
if (is.matrix(TrnX1) != TRUE) TrnX1 <- as.matrix(TrnX1)
if (is.matrix(TrnX2) != TRUE) TrnX2 <- as.matrix(TrnX2)
nx <- nrow(TstX)
blong <- matrix(rep(0, nx), nrow=1, byrow=TRUE,
dimnames=list("blong", 1:nx))
mu1 <- colMeans(TrnX1); mu2 <- colMeans(TrnX2)
if (var.equal == TRUE || var.equal == T){
S <- var(rbind(TrnX1,TrnX2)); beta <- 2*log(rate)
w <- mahalanobis(TstX, mu2, S)
- mahalanobis(TstX, mu1, S)
}
else{
S1 <- var(TrnX1); S2 <- var(TrnX2)
beta <- 2*log(rate) + log(det(S1)/det(S2))
w <- mahalanobis(TstX, mu2, S2)
- mahalanobis(TstX, mu1, S2)
}
for (i in 1:nx){
if (w[i] > beta)
blong[i] <- 1
else
blong[i] <- 2
}
blong
}
##情景1：
n1=2000
n2=2000
x1=rnorm(n1,500,80)#研究生=1
x2=rnorm(n2,400,80)#本科生=2
x<-c(500)#分数为500时进行判别
classx1<-data.frame(x1)
classx2<-data.frame(x2)
colnames(classx1)<-c("x")
colnames(classx2)<-c("x")
discriminiant.distance(classx1,classx2,x)
discriminiant.bayes(classx1,classx2,rate=n2/n1,x)
#logistic
x1=rnorm(n1,500,80)#研究生=1
x2=rnorm(n2,400,80)#本科生=2
x<-c(x1,x2)
y<-c(rep(0,n1),rep(1,n2))#研究生=0，本科生=1
data<-data.frame(cbind(x,y))
colnames(data)<-c("x","y")
fm<-glm(formula=y～x,family=binomial(link = "logit"))#logistic
summary(fm)
new<-data.frame(x=c(400,500,100,600,700))
p=predict(fm,newdata=new,type="response")#type＝response，表示输出结果预测Y=1的概率
group<-ifelse(p<=0.5,"研究生","本科生")
pretest<-data.frame(cbind(new,group))
pretest#500结果：研究生

##情景2：
n1=2000
n2=4000
x1=rnorm(n1,500,80)#研究生=1
x2=rnorm(n2,400,80)#本科生=2
classx1<-data.frame(x1)
classx2<-data.frame(x2)
discriminiant.distance(classx1,classx2,x)
discriminiant.bayes(classx1,classx2,rate=n2/n1,x)
#logistic
x1=rnorm(n1,500,80)#研究生=1
x2=rnorm(n2,400,80)#本科生=2
x<-c(x1,x2)
y<-c(rep(0,n1),rep(1,n2))#研究生=0，本科生=1
data<-data.frame(cbind(x,y))
colnames(data)<-c("x","y")
fm<-glm(formula=y～x,family=binomial(link = "logit"))#logistic
summary(fm)
new<-data.frame(x=c(400,500,100,600,700))
p=predict(fm,newdata=new,type="response")#type＝response，表示输出结果预测Y=1的概率
group<-ifelse(p<=0.5,"研究生","本科生")
pretest<-data.frame(cbind(new,group))
pretest#500结果：研究生

##情景3
n1=2000
n2=6000
x1=rnorm(n1,500,80)#研究生=1
x2=rnorm(n2,400,80)#本科生=2
classx1<-data.frame(x1)
classx2<-data.frame(x2)
discriminiant.distance(classx1,classx2,x)
discriminiant.bayes(classx1,classx2,rate=n2/n1,x)
#logistic
x1=rnorm(n1,500,80)#研究生=1
x2=rnorm(n2,400,80)#本科生=2
x<-c(x1,x2)
y<-c(rep(0,n1),rep(1,n2))#研究生=0，本科生=1
data<-data.frame(cbind(x,y))
colnames(data)<-c("x","y")
fm<-glm(formula=y～x,family=binomial(link = "logit"))#logistic
summary(fm)
new<-data.frame(x=c(400,500,100,600,700))
p=predict(fm,newdata=new,type="response")#type＝response，表示输出结果预测Y=1的概率
group<-ifelse(p<=0.5,"研究生","本科生")
pretest<-data.frame(cbind(new,group))
pretest#500结果：本科生

##情景4
n1=2000
n2=8000
x1=rnorm(n1,500,80)#研究生=1
x2=rnorm(n2,400,80)#本科生=2
classx1<-data.frame(x1)
classx2<-data.frame(x2)
discriminiant.distance(classx1,classx2,x)
discriminiant.bayes(classx1,classx2,rate=n2/n1,x)
#logistic
x1=rnorm(n1,500,80)#研究生=1
x2=rnorm(n2,400,80)#本科生=2
x<-c(x1,x2)
y<-c(rep(0,n1),rep(1,n2))#研究生=0，本科生=1
data<-data.frame(cbind(x,y))
colnames(data)<-c("x","y")
fm<-glm(formula=y～x,family=binomial(link = "logit"))#logistic
summary(fm)
new<-data.frame(x=c(400,500,100,600,700))
p=predict(fm,newdata=new,type="response")#type＝response，表示输出结果预测Y=1的概率
group<-ifelse(p<=0.5,"研究生","本科生")
pretest<-data.frame(cbind(new,group))
pretest#500结果：本科生
#画图
data1<- data.frame(values = c(rnorm(2000,500,80),rnorm(2000,400,80)),group = c(rep("研究生", 2000),rep("本科生",2000)))
ggplot(data1, aes(x = values, fill = group)) +
geom_histogram(position = "identity", alpha = 0.4, bins = 50)+
labs(title="N1:N2=2000:2000")
data2<- data.frame(values = c(rnorm(2000,500,80),rnorm(4000,400,80)),group = c(rep("研究生", 2000),rep("本科生",4000)))
ggplot(data2, aes(x = values, fill = group)) +
geom_histogram(position = "identity", alpha = 0.4, bins = 50)+
labs(title="N1:N2=2000:4000")
data3<- data.frame(values = c(rnorm(2000,500,80),rnorm(6000,400,80)),group = c(rep("研究生", 2000),rep("本科生",6000)))
ggplot(data3, aes(x = values, fill = group)) +
geom_histogram(position = "identity", alpha = 0.4, bins = 50)+
labs(title="N1:N2=2000:6000")
data4<- data.frame(values = c(rnorm(2000,500,80),rnorm(8000,400,80)),group = c(rep("研究生", 2000),rep("本科生",8000)))
ggplot(data4, aes(x = values, fill = group)) +
geom_histogram(position = "identity", alpha = 0.4, bins = 50)+
labs(title="N1:N2=2000:8000")



