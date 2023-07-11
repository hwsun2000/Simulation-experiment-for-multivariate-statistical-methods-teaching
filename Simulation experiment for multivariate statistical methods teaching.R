##���ع����ԶԶ������Իع�����Ӱ��	
install.packages("mvtnorm")#����ģ������
install.packages("car") #����vif
library(mvtnorm)
library(car)
p=3  #����p���Ա���
n=100  #������
beta=c(1,-1,2)
###
c =0.1 #���ϵ��0.1,0.5,0.9,0.95,0.98,0.99,1  
sigma =matrix(c,3,3)#matrix�������ɾ���3��3
sigma[1,1]= sigma[2,2]= sigma[3,3]=1 
x=rmvnorm(n,sigma=sigma) # ����x����
eps=rnorm(n,0,0.5)#eps����rnorm���ڲ���������̬�ֲ����������ƽ����0��׼��0.5
y = x%*%beta+eps; #%*%���˻�����
x1=x[,1]
x2=x[,2]
x3=x[,3]
par(mfrow=c(3,3))
plot(x1,x1,cex=0.6)#cex=0.6���ڵ�Ĵ�С
plot(x1,x2,cex=0.6)
plot(x1,x3,cex=0.6)
plot(x2,x1,cex=0.6)
plot(x2,x2,cex=0.6)
plot(x2,x3,cex=0.6)
plot(x3,x1,cex=0.6)
plot(x3,x2,cex=0.6)
plot(x3,x3,cex=0.6)
simu1<-as.data.frame(cbind(x,y))
#cbind���кϲ�����Ҫ��������ͬ��data.frame���ݿ�Ĵ���
#��Ϊlm���ݼ���ʽ���������ݿ�
colnames(simu1)<-c("x1","x2","x3","y")
#colnames��������
result1<-lm(y��x1+x2+x3,data=simu1)
#lm������ģ�ͺ���
summary(result1)
vif(result1)
#vif������������
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
plot(x1,x1,cex=0.6)#cex=0.6���ڵ�Ĵ�С
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
result1<-lm(y��x1+x2+x3,data=simu1)
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
plot(x1,x1,cex=0.6)#cex=0.6���ڵ�Ĵ�С
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
result1<-lm(y��x1+x2+x3,data=simu1)
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
plot(x1,x1,cex=0.6)#cex=0.6���ڵ�Ĵ�С
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
result1<-lm(y��x1+x2+x3,data=simu1)
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
plot(x1,x1,cex=0.6)#cex=0.6���ڵ�Ĵ�С
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
result1<-lm(y��x1+x2+x3,data=simu1)
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
plot(x1,x1,cex=0.6)#cex=0.6���ڵ�Ĵ�С
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
result1<-lm(y��x1+x2+x3,data=simu1)
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
result1<-lm(y��x1+x2+x3,data=simu1)
summary(result1)
vif(result1)

par(mfrow = c(2, 2))
#par�������û��ѯͼ�β�����ͼ�β����ǳ����������������ͼ����������
#Par(mfrow=c(1,2)):һ�����У�Par(mfrow=c(2,1)):����һ��
###x1��ϵ���������ϵ��0.9��ʼ����
x0=c(0.9,0.95,0.98,0.99)
y0=c(0,0.7,1.4,2)
beta0=rep(1,4)  ##x1����ʵϵ��
beta1=c(0.98,0.93,0.86,1.69)  # x1�ڲ�ͬ��ع�ϵr����õ�ϵ��
beta2=c(-0.97,-1.14,-0.56,-2.52)  # x2�ڲ�ͬ��ع�ϵr����õ�ϵ��
beta3=c(1.93,2.32,1.69,2.82)  # x3�ڲ�ͬ��ع�ϵr����õ�ϵ��
plot(x0,y0,main = "x1��ϵ���������س̶�r�仯",xlab="�Ա���������ϵ��",ylab="ϵ��",type="n",font.main=3,font.lab=3)
#main:�����⣻type=n��ʾ�޵����������font.main:ָ����������Ϊ��ͨ����
points(x=x0,y=beta0,lty=3,pch=1,lwd=2,col = "red")  
lines(x=x0,y=beta0,lty=3,pch=1,lwd=2,col = "red") 
points(x=x0,y=beta1,lty=3,pch=1,lwd=2,col = "black")  
lines(x=x0,y=beta1,lty=3,pch=1,lwd=2,col = "black")
legend("bottomleft",legend=c("��ʵֵ","����ֵ"),xpd=T,cex=0.8,pch=1,lty=3,col=c("red","black"),bty="n")

###x2��ϵ���������ϵ��0.9��ʼ����
x0=c(0.9,0.95,0.98,0.99)
y0=c(-0,-1,-2,-3)
beta0=rep(-1,4)  
beta1=c(0.98,0.93,0.86,1.69)  
beta2=c(-0.97,-1.14,-0.56,-2.52)  
beta3=c(1.93,2.32,1.69,2.82)  
plot(x0,y0,main = "x2��ϵ���������س̶�r�仯",xlab="�Ա���������ϵ��",ylab="ϵ��",type="n",font.main=3,font.lab=3)
points(x=x0,y=beta0,lty=3,pch=1,lwd=2,col = "red")  
lines(x=x0,y=beta0,lty=3,pch=1,lwd=2,col = "red") 
points(x=x0,y=beta2,lty=3,pch=1,lwd=2,col = "black")  
lines(x=x0,y=beta2,lty=3,pch=1,lwd=2,col = "black")
legend("bottomleft",legend=c("��ʵֵ","����ֵ"),xpd=T,cex=0.8,pch=1,lty=3,col=c("red","black"),bty="n")

###x3��ϵ���������ϵ��0.9��ʼ����
x0=c(0.9,0.95,0.98,0.99)
y0=c(0,1,2,3)
beta0=rep(2,4)  
beta1=c(0.98,0.93,0.86,1.69)  
beta2=c(-0.97,-1.14,-0.56,-2.52)  
beta3=c(1.93,2.32,1.69,2.82)  
####�����䲻ͬ���ģʽ�����ɷַ��������Ӱ��
install.packages("mvtnorm")#
install.packages("psych")#barlett KMO
library(mvtnorm)
library(psych)
p=4  
n=100  
#��ֵa=b=c=0.1,0.5,0.9  
#�龰1
a =0.1 
b=0.1
c=0.1
sigma<-c(1,a,c,c,a,1,c,c,c,c,1,b,c,c,b,1)
sigma =matrix(sigma,4,4)
x=rmvnorm(n,sigma=sigma) # 
x<-as.data.frame(x)
KMO(x)
colnames(x)<-c("x1","x2","x3","x4")
result<-princomp(x)#���ɷַ���
summary(result)
r <- corr.test(x)$r # 
fit<- fa(r, nfactors=1, rotate="varimax",fm="pa") 
print(fit, digits=2, cutoff=0.3, sort=TRUE) 
x1=x[,1]
x2=x[,2]
x3=x[,3]
x4=x[,4]
par(mfrow=c(4,4),mar=c(3,3,1,1)+0.1,mgp=c(1.2,0.5,0))
plot(x1,x1,cex=0.6)#cex=0.6���ڵ�Ĵ�С
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
##�龰2
a =0.5 
b=0.5
c=0.5
sigma<-c(1,a,c,c,a,1,c,c,c,c,1,b,c,c,b,1)
sigma =matrix(sigma,4,4)
x=rmvnorm(n,sigma=sigma) # 
x<-as.data.frame(x)
KMO(x)
colnames(x)<-c("x1","x2","x3","x4")
result<-princomp(x)#���ɷַ���
summary(result)
r <- corr.test(x)$r # 
fit<- fa(r, nfactors=1, rotate="varimax",fm="pa") 
print(fit, digits=2, cutoff=0.3, sort=TRUE) 
x1=x[,1]
x2=x[,2]
x3=x[,3]
x4=x[,4]
par(mfrow=c(4,4),mar=c(3,3,1,1)+0.1,mgp=c(1.2,0.5,0))
plot(x1,x1,cex=0.6)#cex=0.6���ڵ�Ĵ�С
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
##�龰3
a =0.9 
b=0.9
c=0.9
sigma<-c(1,a,c,c,a,1,c,c,c,c,1,b,c,c,b,1)
sigma =matrix(sigma,4,4)
x=rmvnorm(n,sigma=sigma) # 
x<-as.data.frame(x)
KMO(x)
colnames(x)<-c("x1","x2","x3","x4")
result<-princomp(x)#���ɷַ���
summary(result)
r <- corr.test(x)$r # 
fit<- fa(r, nfactors=1, rotate="varimax",fm="pa") 
print(fit, digits=2, cutoff=0.3, sort=TRUE) 
x1=x[,1]
x2=x[,2]
x3=x[,3]
x4=x[,4]
par(mfrow=c(4,4),mar=c(3,3,1,1)+0.1,mgp=c(1.2,0.5,0))
plot(x1,x1,cex=0.6)#cex=0.6���ڵ�Ĵ�С
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
#�龰4  
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
plot(x1,x1,cex=0.6)#cex=0.6���ڵ�Ĵ�С
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
#�龰5
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
plot(x1,x1,cex=0.6)#cex=0.6���ڵ�Ĵ�С
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
####�Ա����������������Ӱ���£�������Ϻͷ�������϶�Logsitic�ع�����Ӱ��
library(mvtnorm)
n=500
sigma =matrix(1,1,1)
x=rnorm(n,2,1)
x0<-matrix(c(x,x^2),n,2)
beta<-c(-3,1)
feta=x0%*%beta;
fprob=exp(feta)/(1+exp(feta))
y=rbinom(n,1,fprob)#����ֲ�rbinom(n, ���������, ��������)
par(mfrow = c(1, 1))
plot(x,y,main = "ɢ��ͼ",xlab="��������",ylab="�����ض�",type="n",font.main=3,font.lab=3)
#main:�����⣻type=n��ʾ�޵����������font.main:ָ����������Ϊ��ͨ����
points(x=x,y=y,lty=3,pch=1,lwd=2,col = "red")  
legend("bottomleft",legend=c("�۲�ֵ"),cex=0.8,pch=1,lty=3,col=c("red"),bty="n")
#���Իع�
glm1<-glm(formula=y��x,family=binomial(link = "logit"))#logistic
summary(glm1)
#�����Իع�
glm2<-glm(formula=y��x+I(x^2),family=binomial(link = "logit"))#logistic
summary(glm2)
#ORֵ�Ƚ�
ORx=exp(0.7617)
OR2<-c(exp(-3.6318+(2%*%x+1)*1.2489))
OR0<-c(exp(-3+(2%*%x+1)*1))
OR1=rep(ORx,100)  
standard=rep(1,100) 
plot(x0,y0,main = "����������������ϵ�ORֵ�Ƚ�",xlab="x",ylab="ORֵ",type="n",font.main=3,font.lab=3)
points(x=x,y=OR1,lty=3,pch=1,lwd=2,col = "yellow")  
points(x=x,y=OR2,lty=3,pch=1,lwd=2,col = "red")  
points(x=x,y=OR0,lty=3,pch=1,lwd=2,col = "blue")
lines(x=x,y=standard,type="l",lty=3,pch=1,lwd=4,col = "black") 
legend("topright",legend=c("�������","���������","��ʵOR=2.14","OR=1"),cex=0.8,pch=1,lty=3,col=c("yellow","red","blue","black"),bty="n")
#ROC����
install.packages("pROC")
install.packages("ggplot2")
library(pROC)
library(ggplot2)
data<-data.frame(x)
par(mfrow = c(2, 2))
p01=predict(glm1,newdata=data,type="response")#type��response����ʾ������Ԥ��Y=1�ĸ���
roc1<-data.frame(cbind(x,y,p01))
p1=roc(y��p01,roc1)
p02=predict(glm2,newdata=data,type="response")
roc2<-data.frame(cbind(x,y,p02))
p2=roc(y��p02,roc2)
plot(p1,,smooth=T,grid.col=c("black", "black"))
plot(p2,add=T,col="red")
legend("bottomright",legend=c("����ģ��AUC=0.727","������ģ��AUC=0.881"),cex=1.2,pch=1,lty=1,col=c("black","red"),bty="n")
###��ͬɾʧ������Cox��������ģ�ͽ����Ӱ��	
install.packages("mvtnorm")#����ģ������
install.packages("survival") 
library(mvtnorm)
library(survival)
n=1000  
p=2  
#ɾʧ����70%:CL=0,CU=1
CL<- 0
CU<- 1
beta=c(1,-1)
   ssigma =0.7   #���ϵ��
       Sigma =ssigma^t(sapply(1:p, function(i, j) abs(i-j), 1:p)) 
       x=rmvnorm(n, sigma=Sigma) # ����x����  
       ht=exp(x%*%beta)  #�õ����պ���
      	 #observed survivial time����ʱ��
logS=log(matrix(runif(n,0,1),n,1)) #log[S(t)]
       #runif����0��1֮����Ͼ��ȷֲ����������matrix����100*1�ľ���
	 T=-logS/ht  #survival time����ʱ��
	#censored timeɾʧʱ��
	 myrate <- matrix(runif(n,CL,CU),n,1)/ht
	 C <- rexp(n, rate = 1/myrate)
     	#survival time and state
	 time <- apply(cbind(C,T),1,min)  ##time������ʱ��
      #cbind:���кϲ�����Ҫ��������ͬ
      #apply:��������л��з���Ӧ��ָ������
      t<-time
status <- ifelse(T<C,1,0) #statָʾ���Ƿ�ɾʧ,����1��ʾ�¼�����������0��ʾ��β����
 #sum(status)
df<-as.data.frame(cbind(x,T,C,status))
colnames(df)<-c("x1","x2","time","C","status")
fit.cox<-coxph(Surv(time,status)��x1+x2,data=df)
summary(fit.cox)
#ɾʧ����60%:CL=0.5,CU=1
CL<- 0.5
CU<- 1
beta=c(1,-1)
   ssigma =0.7   #���ϵ��
       Sigma =ssigma^t(sapply(1:p, function(i, j) abs(i-j), 1:p)) 
       x=rmvnorm(n, sigma=Sigma) # ����x����  
       ht=exp(x%*%beta)  #�õ����պ���
      	 #observed survivial time����ʱ��
logS=log(matrix(runif(n,0,1),n,1)) #log[S(t)]
       #runif����0��1֮����Ͼ��ȷֲ����������matrix����100*1�ľ���
	 T=-logS/ht  #survival time����ʱ��
	#censored timeɾʧʱ��
	 myrate <- matrix(runif(n,CL,CU),n,1)/ht
	 C <- rexp(n, rate = 1/myrate)
     	#survival time and state
	 time <- apply(cbind(C,T),1,min)  ##time������ʱ��
      #cbind:���кϲ�����Ҫ��������ͬ
      #apply:��������л��з���Ӧ��ָ������
      t<-time
status <- ifelse(T<C,1,0) #statָʾ���Ƿ�ɾʧ,����1��ʾ�¼�����������0��ʾ��β����
 #sum(status)
df<-as.data.frame(cbind(x,T,C,status))
colnames(df)<-c("x1","x2","time","C","status")
fit.cox<-coxph(Surv(time,status)��x1+x2,data=df)
summary(fit.cox)
#ɾʧ����50%:CL=1,CU=1.1
CL<- 1
CU<- 1.1
beta=c(1,-1)
   ssigma =0.7   #���ϵ��
       Sigma =ssigma^t(sapply(1:p, function(i, j) abs(i-j), 1:p)) 
       x=rmvnorm(n, sigma=Sigma) # ����x����  
       ht=exp(x%*%beta)  #�õ����պ���
      	 #observed survivial time����ʱ��
logS=log(matrix(runif(n,0,1),n,1)) #log[S(t)]
       #runif����0��1֮����Ͼ��ȷֲ����������matrix����100*1�ľ���
	 T=-logS/ht  #survival time����ʱ��
	#censored timeɾʧʱ��
	 myrate <- matrix(runif(n,CL,CU),n,1)/ht
	 C <- rexp(n, rate = 1/myrate)
     	#survival time and state
	 time <- apply(cbind(C,T),1,min)  ##time������ʱ��
      #cbind:���кϲ�����Ҫ��������ͬ
      #apply:��������л��з���Ӧ��ָ������
      t<-time
status <- ifelse(T<C,1,0) #statָʾ���Ƿ�ɾʧ,����1��ʾ�¼�����������0��ʾ��β����
 #sum(status)
df<-as.data.frame(cbind(x,T,C,status))
colnames(df)<-c("x1","x2","time","C","status")
fit.cox<-coxph(Surv(time,status)��x1+x2,data=df)
summary(fit.cox)
#ɾʧ����40%:CL=1,CU=2
CL<- 1
CU<- 2
beta=c(1,-1)
   ssigma =0.7   #���ϵ��
       Sigma =ssigma^t(sapply(1:p, function(i, j) abs(i-j), 1:p)) 
       x=rmvnorm(n, sigma=Sigma) # ����x����  
       ht=exp(x%*%beta)  #�õ����պ���
      	 #observed survivial time����ʱ��
logS=log(matrix(runif(n,0,1),n,1)) #log[S(t)]
       #runif����0��1֮����Ͼ��ȷֲ����������matrix����100*1�ľ���
	 T=-logS/ht  #survival time����ʱ��
	#censored timeɾʧʱ��
	 myrate <- matrix(runif(n,CL,CU),n,1)/ht
	 C <- rexp(n, rate = 1/myrate)
     	#survival time and state
	 time <- apply(cbind(C,T),1,min)  ##time������ʱ��
      #cbind:���кϲ�����Ҫ��������ͬ
      #apply:��������л��з���Ӧ��ָ������
      t<-time
status <- ifelse(T<C,1,0) #statָʾ���Ƿ�ɾʧ,����1��ʾ�¼�����������0��ʾ��β����
 #sum(status)
df<-as.data.frame(cbind(x,T,C,status))
colnames(df)<-c("x1","x2","time","C","status")
fit.cox<-coxph(Surv(time,status)��x1+x2,data=df)
summary(fit.cox)
#ɾʧ����30%:CL=1,CU=4
CL<- 1
CU<- 4
beta=c(1,-1)
   ssigma =0.7   #���ϵ��
       Sigma =ssigma^t(sapply(1:p, function(i, j) abs(i-j), 1:p)) 
       x=rmvnorm(n, sigma=Sigma) # ����x����  
       ht=exp(x%*%beta)  #�õ����պ���
      	 #observed survivial time����ʱ��
logS=log(matrix(runif(n,0,1),n,1)) #log[S(t)]
       #runif����0��1֮����Ͼ��ȷֲ����������matrix����100*1�ľ���
	 T=-logS/ht  #survival time����ʱ��
	#censored timeɾʧʱ��
	 myrate <- matrix(runif(n,CL,CU),n,1)/ht
	 C <- rexp(n, rate = 1/myrate)
     	#survival time and state
	 time <- apply(cbind(C,T),1,min)  ##time������ʱ��
      #cbind:���кϲ�����Ҫ��������ͬ
      #apply:��������л��з���Ӧ��ָ������
      t<-time
status <- ifelse(T<C,1,0) #statָʾ���Ƿ�ɾʧ,����1��ʾ�¼�����������0��ʾ��β����
 #sum(status)
df<-as.data.frame(cbind(x,T,C,status))
colnames(df)<-c("x1","x2","time","C","status")
fit.cox<-coxph(Surv(time,status)��x1+x2,data=df)
summary(fit.cox)
#ɾʧ����20%:CL=3,CU=5
CL<- 3
CU<- 5
beta=c(1,-1)
   ssigma =0.7   #���ϵ��
       Sigma =ssigma^t(sapply(1:p, function(i, j) abs(i-j), 1:p)) 
       x=rmvnorm(n, sigma=Sigma) # ����x����  
       ht=exp(x%*%beta)  #�õ����պ���
      	 #observed survivial time����ʱ��
logS=log(matrix(runif(n,0,1),n,1)) #log[S(t)]
       #runif����0��1֮����Ͼ��ȷֲ����������matrix����100*1�ľ���
	 T=-logS/ht  #survival time����ʱ��
	#censored timeɾʧʱ��
	 myrate <- matrix(runif(n,CL,CU),n,1)/ht
	 C <- rexp(n, rate = 1/myrate)
     	#survival time and state
	 time <- apply(cbind(C,T),1,min)  ##time������ʱ��
      #cbind:���кϲ�����Ҫ��������ͬ
      #apply:��������л��з���Ӧ��ָ������
      t<-time
status <- ifelse(T<C,1,0) #statָʾ���Ƿ�ɾʧ,����1��ʾ�¼�����������0��ʾ��β����
 #sum(status)
df<-as.data.frame(cbind(x,T,C,status))
colnames(df)<-c("x1","x2","time","C","status")
fit.cox<-coxph(Surv(time,status)��x1+x2,data=df)
summary(fit.cox)
#ɾʧ����10%:CL=8.9,CU=9
CL<- 8.9
CU<- 9
beta=c(1,-1)
   ssigma =0.7   #���ϵ��
       Sigma =ssigma^t(sapply(1:p, function(i, j) abs(i-j), 1:p)) 
       x=rmvnorm(n, sigma=Sigma) # ����x����  
       ht=exp(x%*%beta)  #�õ����պ���
      	 #observed survivial time����ʱ��
logS=log(matrix(runif(n,0,1),n,1)) #log[S(t)]
       #runif����0��1֮����Ͼ��ȷֲ����������matrix����100*1�ľ���
	 T=-logS/ht  #survival time����ʱ��
	#censored timeɾʧʱ��
	 myrate <- matrix(runif(n,CL,CU),n,1)/ht
	 C <- rexp(n, rate = 1/myrate)
     	#survival time and state
	 time <- apply(cbind(C,T),1,min)  ##time������ʱ��
      #cbind:���кϲ�����Ҫ��������ͬ
      #apply:��������л��з���Ӧ��ָ������
      t<-time
status <- ifelse(T<C,1,0) #statָʾ���Ƿ�ɾʧ,����1��ʾ�¼�����������0��ʾ��β����
 #sum(status)
df<-as.data.frame(cbind(x,T,C,status))
colnames(df)<-c("x1","x2","time","C","status")
fit.cox<-coxph(Surv(time,status)��x1+x2,data=df)
summary(fit.cox)
#ɾʧ����5%:CL=18,CU=19
CL<- 18
CU<- 19
beta=c(1,-1)
   ssigma =0.7   #���ϵ��
       Sigma =ssigma^t(sapply(1:p, function(i, j) abs(i-j), 1:p)) 
       x=rmvnorm(n, sigma=Sigma) # ����x����  
       ht=exp(x%*%beta)  #�õ����պ���
      	 #observed survivial time����ʱ��
logS=log(matrix(runif(n,0,1),n,1)) #log[S(t)]
       #runif����0��1֮����Ͼ��ȷֲ����������matrix����100*1�ľ���
	 T=-logS/ht  #survival time����ʱ��
	#censored timeɾʧʱ��
	 myrate <- matrix(runif(n,CL,CU),n,1)/ht
	 C <- rexp(n, rate = 1/myrate)
     	#survival time and state
	 time <- apply(cbind(C,T),1,min)  ##time������ʱ��
      #cbind:���кϲ�����Ҫ��������ͬ
      #apply:��������л��з���Ӧ��ָ������
      t<-time
status <- ifelse(T<C,1,0) #statָʾ���Ƿ�ɾʧ,����1��ʾ�¼�����������0��ʾ��β����
 #sum(status)
df<-as.data.frame(cbind(x,T,C,status))
colnames(df)<-c("x1","x2","time","C","status")
fit.cox<-coxph(Surv(time,status)��x1+x2,data=df)
summary(fit.cox)

par(mfrow = c(2, 2))
###��ͼ��x1��ϵ����
x0=c(0.10,0.20,0.30,0.40,0.5,0.6,0.7,0.8)
y0=c(0,0.2,0.4,0.6,0.8,1,1.2,1.4)
beta0=rep(1,8)  ##x1����ʵϵ�� 
beta1=c(0.931,0.898,0.816,0.732,0.730,0.656,0.501,0.482)  #x1�ڲ�ͬ��ɾʧ��������õ�ϵ��
plot(x0,y0,main = "x1",xlab="�������ݵ�ɾʧ����",ylab="ϵ��",type="n",font.main=3,font.lab=3)
points(x=x0,y=beta0,lty=3,pch=1,lwd=2,col = "black")  
lines(x=x0,y=beta0,lty=3,pch=1,lwd=2,col = "black") 
points(x=x0,y=beta1,lty=3,pch=1,lwd=2,col = "red")  
lines(x=x0,y=beta1,lty=3,pch=1,lwd=2,col = "red")
legend("bottomleft",legend=c("��ʵֵ","����ֵ"),cex=0.8,pch=1,lty=3,col=c("black","red"),bty="n")
###��ͼ��x2��ϵ����
x0=c(0.10,0.20,0.30,0.40,0.5,0.6,0.7,0.8)
y0=c(-0.2,-0.4,-0.6,-0.8,-1,-1.2,-1.4,-1.6)
beta0=rep(-1,8)  ##x2����ʵϵ��
beta2=c(-0.924,-0.924,-0.806,-0.751,-0.680,-0.676,-0.515,-0.593)  # x2�ڲ�ͬɾʧ��������õ�ϵ��
plot(x0,y0,main = "x2",xlab="�������ݵ�ɾʧ����",ylab="ϵ��",type="n",font.main=3,font.lab=3)
points(x=x0,y=beta0,lty=3,pch=1,lwd=2,col = "black")  
lines(x=x0,y=beta0,lty=3,pch=1,lwd=2,col = "black") 
points(x=x0,y=beta2,lty=3,pch=1,lwd=2,col = "red")  
lines(x=x0,y=beta2,lty=3,pch=1,lwd=2,col = "red")
legend("bottomleft",legend=c("��ʵֵ","����ֵ"),cex=0.8,pch=1,lty=3,col=c("black","red"),bty="n")
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


###�����䲻ͬ��ؽṹ�Ե�����ط��������Ӱ��
install.packages("mvtnorm")
library(mvtnorm)
p=4  
n=100  
#��������corcoef.test������������������
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
##�龰1 
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
#���������ڵ��ͱ����µĵ÷� U=AX  V=BY
U<-as.matrix(test[, 1:2])%*% ca$xcoef   
V<-as.matrix(test[, 3:4])%*% ca$ycoef
#����U1��V1��U2��V2Ϊ���������ɢ��ͼ
par(mfrow = c(2, 2))
plot(U[,1], V[,1], xlab="U1", ylab="V1")
plot(U[,2], V[,2], xlab="U2", ylab="V2")
##�龰2
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
##�龰3
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
####��������ͬ�Ծ����б��Bayes�б�Logistic�ع�����Ӱ��
install.packages("ggplot2")
library(ggplot2)
#���������б���
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
#������Ҷ˹�б���
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
##�龰1��
n1=2000
n2=2000
x1=rnorm(n1,500,80)#�о���=1
x2=rnorm(n2,400,80)#������=2
x<-c(500)#����Ϊ500ʱ�����б�
classx1<-data.frame(x1)
classx2<-data.frame(x2)
colnames(classx1)<-c("x")
colnames(classx2)<-c("x")
discriminiant.distance(classx1,classx2,x)
discriminiant.bayes(classx1,classx2,rate=n2/n1,x)
#logistic
x1=rnorm(n1,500,80)#�о���=1
x2=rnorm(n2,400,80)#������=2
x<-c(x1,x2)
y<-c(rep(0,n1),rep(1,n2))#�о���=0��������=1
data<-data.frame(cbind(x,y))
colnames(data)<-c("x","y")
fm<-glm(formula=y��x,family=binomial(link = "logit"))#logistic
summary(fm)
new<-data.frame(x=c(400,500,100,600,700))
p=predict(fm,newdata=new,type="response")#type��response����ʾ������Ԥ��Y=1�ĸ���
group<-ifelse(p<=0.5,"�о���","������")
pretest<-data.frame(cbind(new,group))
pretest#500������о���

##�龰2��
n1=2000
n2=4000
x1=rnorm(n1,500,80)#�о���=1
x2=rnorm(n2,400,80)#������=2
classx1<-data.frame(x1)
classx2<-data.frame(x2)
discriminiant.distance(classx1,classx2,x)
discriminiant.bayes(classx1,classx2,rate=n2/n1,x)
#logistic
x1=rnorm(n1,500,80)#�о���=1
x2=rnorm(n2,400,80)#������=2
x<-c(x1,x2)
y<-c(rep(0,n1),rep(1,n2))#�о���=0��������=1
data<-data.frame(cbind(x,y))
colnames(data)<-c("x","y")
fm<-glm(formula=y��x,family=binomial(link = "logit"))#logistic
summary(fm)
new<-data.frame(x=c(400,500,100,600,700))
p=predict(fm,newdata=new,type="response")#type��response����ʾ������Ԥ��Y=1�ĸ���
group<-ifelse(p<=0.5,"�о���","������")
pretest<-data.frame(cbind(new,group))
pretest#500������о���

##�龰3
n1=2000
n2=6000
x1=rnorm(n1,500,80)#�о���=1
x2=rnorm(n2,400,80)#������=2
classx1<-data.frame(x1)
classx2<-data.frame(x2)
discriminiant.distance(classx1,classx2,x)
discriminiant.bayes(classx1,classx2,rate=n2/n1,x)
#logistic
x1=rnorm(n1,500,80)#�о���=1
x2=rnorm(n2,400,80)#������=2
x<-c(x1,x2)
y<-c(rep(0,n1),rep(1,n2))#�о���=0��������=1
data<-data.frame(cbind(x,y))
colnames(data)<-c("x","y")
fm<-glm(formula=y��x,family=binomial(link = "logit"))#logistic
summary(fm)
new<-data.frame(x=c(400,500,100,600,700))
p=predict(fm,newdata=new,type="response")#type��response����ʾ������Ԥ��Y=1�ĸ���
group<-ifelse(p<=0.5,"�о���","������")
pretest<-data.frame(cbind(new,group))
pretest#500�����������

##�龰4
n1=2000
n2=8000
x1=rnorm(n1,500,80)#�о���=1
x2=rnorm(n2,400,80)#������=2
classx1<-data.frame(x1)
classx2<-data.frame(x2)
discriminiant.distance(classx1,classx2,x)
discriminiant.bayes(classx1,classx2,rate=n2/n1,x)
#logistic
x1=rnorm(n1,500,80)#�о���=1
x2=rnorm(n2,400,80)#������=2
x<-c(x1,x2)
y<-c(rep(0,n1),rep(1,n2))#�о���=0��������=1
data<-data.frame(cbind(x,y))
colnames(data)<-c("x","y")
fm<-glm(formula=y��x,family=binomial(link = "logit"))#logistic
summary(fm)
new<-data.frame(x=c(400,500,100,600,700))
p=predict(fm,newdata=new,type="response")#type��response����ʾ������Ԥ��Y=1�ĸ���
group<-ifelse(p<=0.5,"�о���","������")
pretest<-data.frame(cbind(new,group))
pretest#500�����������
#��ͼ
data1<- data.frame(values = c(rnorm(2000,500,80),rnorm(2000,400,80)),group = c(rep("�о���", 2000),rep("������",2000)))
ggplot(data1, aes(x = values, fill = group)) +
geom_histogram(position = "identity", alpha = 0.4, bins = 50)+
labs(title="N1:N2=2000:2000")
data2<- data.frame(values = c(rnorm(2000,500,80),rnorm(4000,400,80)),group = c(rep("�о���", 2000),rep("������",4000)))
ggplot(data2, aes(x = values, fill = group)) +
geom_histogram(position = "identity", alpha = 0.4, bins = 50)+
labs(title="N1:N2=2000:4000")
data3<- data.frame(values = c(rnorm(2000,500,80),rnorm(6000,400,80)),group = c(rep("�о���", 2000),rep("������",6000)))
ggplot(data3, aes(x = values, fill = group)) +
geom_histogram(position = "identity", alpha = 0.4, bins = 50)+
labs(title="N1:N2=2000:6000")
data4<- data.frame(values = c(rnorm(2000,500,80),rnorm(8000,400,80)),group = c(rep("�о���", 2000),rep("������",8000)))
ggplot(data4, aes(x = values, fill = group)) +
geom_histogram(position = "identity", alpha = 0.4, bins = 50)+
labs(title="N1:N2=2000:8000")


