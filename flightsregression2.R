setwd("C:/Users/JSL/Desktop")
getwd()
worldflights=read.csv("worldflights.csv",header=T)
head(worldflights)
colnames(worldflights)

#단가.백만달러가 0인행 제거
library(dplyr)
colnames(worldflights) = c("제조사","비행기모델","ICAO","IATA",
                           "비행기구분","비행기크기","class","엔진타입","엔진수","최초운항일",
                           "생산수량","price","상태","조종석수","승객수","길이","높이","운송범위")
colnames(worldflights)

wf2 <- select(worldflights, "제조사", "비행기구분","비행기크기","class","엔진타입",
              "엔진수", "생산수량","price","조종석수","승객수",
              "길이","높이","운송범위")

as.data.frame(wf2)

#가격 숫자형으로 변환
is.numeric(wf2$price)
wf2$price=as.numeric(wf2$price)
is.numeric(wf2$price)
wf2$price


#결측치 제거

wf2=wf2%>%
  filter(price != 0)

wf3=wf2%>%
  filter(승객수 != 0)

wf4=wf3%>%
  filter(길이 != 0)

wf5=wf4%>%
  filter(높이 != 0)

wf6=wf5%>%
  filter(운송범위 != 0)

wf7=wf6%>%
  filter(생산수량 != 0)

dim(wf7)
as.data.frame(wf7)

#범주형데이터 처리

wf7$제조사 = factor(wf7$제조사)
wf7$비행기크기 = factor(wf7$비행기크기)
wf7$비행기구분= factor(wf7$비행기구분)
wf7$엔진타입 = factor(wf7$엔진타입)

# 산점도 행렬
library(psych)
par(mar=c(4.5,4.5,1.5,1.5))
par(mfrow=c(2,2))
pairs.panels(wf7)

#회귀모형 적합
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

#stepwise선택(forward 방법과 같은 결과_결정계수가 가장 높음)
summary(fit.both)

#다중공선성 
library(car)
vif(fit.both)

#정규성 확인
shapiro.test(fit.both$residuals)

#더빈왓슨통계량
#모형의 잔차 독립성을 확인해 주는 더빈왓슨통계량
install.packages("lmtest")
library(lmtest)
dwtest(fit.both)
#1.6859 2에 가까운값을 가지며 독립변수 잔차들 간의 자기상관이 없다

#이상치제거
fit_final=lm(price ~ 승객수 + 운송범위 + 높이, data = wf7[-c(115),])
summary(fit_final)

#변수의 상대적 중요도를 시각화
model_final = lm(price ~ 승객수 + 운송범위 + 높이, data = wf7)
result = relweights(model_final, col='red')

model=lm(price ~ 승객수 + 운송범위 + 높이, data = wf7[-c(115),])
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


model_3 = lm(price ~ 승객수 + 운송범위 + 높이, data = wf7[-c(115),])
result = relweights(model_3, col="blue")
result

# ggplot2을 사용하여, 변수의 상대적 중요도를 시각화
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