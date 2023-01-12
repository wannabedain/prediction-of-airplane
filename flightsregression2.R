setwd("C:/Users/JSL/Desktop")
getwd()
worldflights=read.csv("worldflights.csv",header=T)
head(worldflights)
colnames(worldflights)

#�ܰ�.�鸸�޷��� 0���� ����
library(dplyr)
colnames(worldflights) = c("������","������","ICAO","IATA",
                           "����ⱸ��","�����ũ��","class","����Ÿ��","������","���ʿ�����",
                           "�������","price","����","��������","�°���","����","����","��۹���")
colnames(worldflights)

wf2 <- select(worldflights, "������", "����ⱸ��","�����ũ��","class","����Ÿ��",
              "������", "�������","price","��������","�°���",
              "����","����","��۹���")

as.data.frame(wf2)

#���� ���������� ��ȯ
is.numeric(wf2$price)
wf2$price=as.numeric(wf2$price)
is.numeric(wf2$price)
wf2$price


#����ġ ����

wf2=wf2%>%
  filter(price != 0)

wf3=wf2%>%
  filter(�°��� != 0)

wf4=wf3%>%
  filter(���� != 0)

wf5=wf4%>%
  filter(���� != 0)

wf6=wf5%>%
  filter(��۹��� != 0)

wf7=wf6%>%
  filter(������� != 0)

dim(wf7)
as.data.frame(wf7)

#������������ ó��

wf7$������ = factor(wf7$������)
wf7$�����ũ�� = factor(wf7$�����ũ��)
wf7$����ⱸ��= factor(wf7$����ⱸ��)
wf7$����Ÿ�� = factor(wf7$����Ÿ��)

# ������ ���
library(psych)
par(mar=c(4.5,4.5,1.5,1.5))
par(mfrow=c(2,2))
pairs.panels(wf7)

#ȸ�͸��� ����
fit <- lm(price~., data = wf7)
summary(fit)

#1. forward

fit.con1 <- lm(price~1,data = wf7)
fit.forward1 <- step(fit.con1,scope=list(lower=fit.con1,upper=fit),direction = "forward")
summary(fit.forward1)

#2. backward

fit.backward <- step(fit, scope = list(lower = fit.con1, upper = fit),direction = "backward")
summary(fit.backward) 

#3. stepwise
fit.both <- step(fit.con1, scope = list(lower = fit.con1, upper = fit), direction = "both")
summary(fit.both)
par(mar=c(1.5,1.5,1.5,1.5))
par(mfrow=c(2,2))
plot(fit.both,which=1)
plot(fit.both,which=2)
plot(fit.both,which=3)
plot(fit.both,which=4)
dev.off()

#stepwise����(forward ����� ���� ���_��������� ���� ����)
summary(fit.both)

#���߰����� 
library(car)
vif(fit.both)

#���Լ� Ȯ��
shapiro.test(fit.both$residuals)

#����ӽ���跮
#������ ���� �������� Ȯ���� �ִ� ����ӽ���跮
install.packages("lmtest")
library(lmtest)
dwtest(fit.both)
#1.6859 2�� ������ ������ �������� ������ ���� �ڱ����� ����

#�̻�ġ����
fit_final=lm(price ~ �°��� + ��۹��� + ����, data = wf7[-c(115),])
summary(fit_final)

#������ ����� �߿䵵�� �ð�ȭ
model_final = lm(price ~ �°��� + ��۹��� + ����, data = wf7)
result = relweights(model_final, col='red')

model=lm(price ~ �°��� + ��۹��� + ����, data = wf7[-c(115),])
relweights <- function(fit,...){
  R <- cor(fit$model)
  nvar <- ncol(R)
  rxx <- R[2:nvar, 2:nvar]
  rxy <- R[2:nvar, 1]
  svd <- eigen(rxx)
  evec <- svd$vectors
  ev <- svd$values
  delta <- diag(sqrt(ev))
  lambda <- evec %*% delta %*% t(evec)
  lambdasq <- lambda ^ 2
  beta <- solve(lambda) %*% rxy
  rsquare <- colSums(beta ^ 2)
  rawwgt <- lambdasq %*% beta ^ 2
  import <- (rawwgt / rsquare) * 100
  import <- as.data.frame(import)
  row.names(import) <- names(fit$model[2:nvar])
  names(import) <- "Weights"
  import <- import[order(import),1, drop=FALSE]
  dotchart(import$Weights, labels=row.names(import),
           xlab="% of R-Square", pch=19,
           main="Relative Importance of Predictor Variables",
           sub=paste("Total R-Square=", round(rsquare, digits=3)),
           ...)
  return(import)}


model_3 = lm(price ~ �°��� + ��۹��� + ����, data = wf7[-c(115),])
result = relweights(model_3, col="blue")
result

# ggplot2�� ����Ͽ�, ������ ����� �߿䵵�� �ð�ȭ
library(ggplot2)
plotRelWeights=function(fit){
  data<-relweights(fit)
  data$Predictors<-rownames(data)
  p<-ggplot(data=data,aes(x=reorder(Predictors,Weights),y=Weights,fill=Predictors))+ 
    geom_bar(stat="identity",width=0.5)+
    ggtitle("Relative Importance of Predictor Variables")+
    ylab(paste0("% of R-square \n(Total R-Square=",attr(data,"R-square"),")"))+
    geom_text(aes(y=Weights-0.1,label=paste(round(Weights,1),"%")),hjust=1)+
    guides(fill=FALSE)+
    coord_flip()
  p
}

plotRelWeights(model_3)