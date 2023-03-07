# 시계열 분석 라이브러리
library(TSA)

# 데이터 불러오기
data(airmiles)

# 시각화
plot(airmiles)

# 값이 상당히 크기에 로그를 씌움
plot(log(airmiles))

# 개입사건이 있을걸로 예측되는 시점
abline(v = 2001.5, col=2)
# 해당 시점에 충격이 있었을거라고 예측된다



data <- log(airmiles)

before <- log(airmiles[seq(airmiles)<69])

layout(1)
# 충격 이전의 시계열 모습
ts.plot(before)


# 충격 이전의 ACF, PACF
layout(t(1:2))
acf(before, lag.max = 48)
pacf(before, lag.max = 48)
# ACF에서 주기성을 확인할 수 있다
# 정상성을 가지는 모습을 만들기 위해 계절차분이 필요해 보임


# 계절차분
d12_before <- diff(before, 12)

# 계절차분 후의 모습
layout(1)
ts.plot(d12_before)

# 계절차분 후의 모습
layout(t(1:2))
acf(d12_before, lag.max = 48)
pacf(d12_before, lag.max = 48)
# 아직까지도 정상성을 가지는 모습이 아님.
# 추가적 차분을 통해 정상성을 만족하는 모습을 만들어야함


# 차분진행
layout(1)
d_d12_before <- diff(d12_before)
# 차분후의 모습
ts.plot(d_d12_before)


# 차분후의 ACF, PACF
layout(t(1:2))
acf(d_d12_before)
pacf(d_d12_before)
# 이제 어느정도 W.N.로 보인다


# 예측
library(forecast)

# aic를 이용하여 p, q, P, Q 를 찾아본다
aic<-1000
for(p in 0:2) for(q in 0:2)for(P in 0:1) for(Q in 0:1){
  aic.tmp<-AIC(Arima(before, order=c(p,1,q), seasonal = list(order = c(P, 1,Q), period = 12)))
  if(aic.tmp<aic){
    aic<-aic.tmp
    cat("p=",p, "q=",q, "P=",P,"Q=",Q," AIC=",aic ,"\n")
  }
} 
# p=0, q=1, P=0, Q=1 에서 가장 낮은 AIC를 보임

# p=0, q=1, P=0, Q=1 로 모델 적합
fit <- Arima(before, order=c(0,1,1), seasonal = list(order = c(0, 1,1), period = 12))

# 잔차 분석
res <- fit$residuals

# 잔차 그림
layout(1)
ts.plot(res)

# 잔차의 ACF, PACF
layout(t(1:2))
acf(res)
pacf(res)
# 잔차가 W.N.로 보인다

# Box test. H_0 : (data are indepedently distributed)
Box.test(res,lag=12, type="Ljung")
# box test또한 H_0를 기각하지 못함





# 시각화
layout(1)
ts.plot(before)

model <- Arima(
  before,
  order=c(0,1,1),
  seasonal=list(order=c(0,1,1),period=12)
)

hat<-model$fitted # 적합값(추정된 모형에 의해서 계산된 값)
layout(1)
lines(hat, col="red", lty=2) # 적합값 그래프


# 개입 사건 이전 데이터를 통해 예측
fcast <- forecast(
  model,
  h=70, level = 0
)
plot(fcast)
lines(ts(data), col="red", lty=1)
# 원래 데이터 (빨강)
# 예측한 데이터 (파랑)
# 사건 당시 큰 충격이 있고 시간이 지남에 따라 그 충격이 점차 사라지는것으로 보임

# 예상되는 충격의 모습
pulse <- (seq(airmiles) == 69) * 1 # 곱해지는 상수는 추정해야함
impact <- pulse
impact

for (a in 69+1:length(impact)) {
  impact[a] = impact[a-1] * (9/10) # 곱해지는 상수는 추정해야함
}

# 대략적인 모습 시각화
ts.plot(
  -impact
)

library(forecast)


# arima 함수로 구현
model <- arima(
  log(airmiles),
  order=c(0,1,1),
  seasonal=list(order=c(0,1,1),period=12),
  # 펄수 함수
  xtransf=data.frame(I911=1*(seq(airmiles))==69,I911=1*(seq(airmiles)==69)),
  # 펄수 함수 변형
  # c(0,0) : 펄스함수 그대로
  # c(1,0) : 계단함수로 변환
  transfer=list(c(0,0),c(1,0))
)

# 추정된 계수 (위에서 언급한 상수)
model
# 추정된 값
plot(fitted(model))


# 잔차분석
res <- model$residuals

plot(res)


layout(t(1:2))
acf(res)
pacf(res)

Box.test(res, lag=12, type='Ljung')
# 잔차가 W.N. 판단할 근거가 충분하다.


layout(1)
ts.plot(res)
# 잔차가 튀는 부분이 존재함
# 이런 모습도 정상 시계열이라고할 수 있는가?

# 변동성 집중모델로 설명 가능.
# 추후 진행