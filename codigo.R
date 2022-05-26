dados=read.csv("dados.csv",header=T, sep = ',') # lê oconjunto de dados

reta_mq=lm(ps_sistolica~peso,data=dados) # ajusta o MRLS na base dados

# construção de gráficos
plot(dados$peso,dados$ps_sistolica, xlab="Peso", ylab="PS Sistólica", main="Diagrama de dispersão (Peso x PS Sistólica)")
newx <- seq(min(dados$peso), max(dados$peso), length.out=100)
preds <- predict(reta_mq, newdata=data.frame(peso=newx), interval="confidence", level = 0.95) # calcula as previsões com intervalo de confiança
plot(dados$peso,dados$ps_sistolica,main="Reta de mínimos quadrados com os intervalos de confiança (95%)", xlab="Peso", ylab="Pressão")
abline(reta_mq, col="red")
points(mean(dados$peso),mean(dados$ps_sistolica), col="blue", pch=16)
lines(newx, preds[,2], col="blue", lty=2)
lines(newx, preds[,3], col="blue", lty=2)

summary(reta_mq) # resultados do ajuste de minimos quadrados
aov_mq=aov(ps_sistolica~peso,data=dados) # calcula a análise de variância do MRLS
qmres=sum(reta_mq$residuals^2)/24 # quadrado médio dos resíduos
sqreg=2694 # soma de quadrados da regressão
sqt=sqreg+1809 # soma de quadrados total
R=sqreg/sqt 
r=sqrt(R) # coeficiente de correlação linear
s=sqrt(qmres) # (s)
desviosx=dados$peso-mean(dados$peso) # desvios da média dos pesos
sxx=sum(desviosx^2) # soma de quadrados dos desvios da média
epb=s/sqrt(sxx) # erro padrão do beta
epa=s*sqrt(1/26+mean(dados$peso)^2/sxx) # erro padrão do intercepto
lim_inf95_b=reta_mq$coefficients[2]-epb*qt(0.975,24) # limite inferior do IC de 95% para beta
lim_sup95_b=reta_mq$coefficients[2]+epb*qt(0.975,24) # limite superior do IC de 95% para beta
lim_inf95_a=reta_mq$coefficients[1]-epa*qt(0.975,24) # limite inferior do IC de 95% para intercepto
lim_sup95_a=reta_mq$coefficients[1]+epa*qt(0.975,24) # limite superior do IC de 95% para intercepto
lim_inf95_sigma2=24*qmres/qchisq(0.975,24) # limite inferior do IC de 95% para sigma2
lim_sup95_sigma2=24*qmres/qchisq(0.025,24) # limite superior do IC de 95% para sigma2

# construção de gráficos dos resíduos
residuo_padronizado=(reta_mq$residuals-mean(reta_mq$residuals))/s
plot(dados$peso,dados$ps_sistolica,main="Diagrama de dispersao e reta de minimos quadrados",xlab="peso",ylab="pressão")
abline(reta_mq,lty=2)
plot(dados$peso,residuo_padronizado,main="Variavel explicativa versus Residuos padronizados",xlab="peso",ylab="residuos padronizados",ylim=c(-2.5,2.5))
abline(h=0,lty=2, col="blue")
qqnorm(residuo_padronizado,xlab="Quantis teóricos",ylab="Quantis amostrais",main="Normal plot dos residuos padronizados")
abline(0,1, col="red")
abline(h=0,col="blue", lty=2)
abline(v=0,col="blue", lty=2)

ep_rm=s*sqrt(1/26+(172-mean(dados$peso))^2/sxx) # erro padrão da resposta média
rm=reta_mq$coefficients[1]+reta_mq$coefficients[2]*172
lim_inf95_rm=rm-ep_rm*qt(0.975,24) # limite inferior do IC de 95% para resp media
lim_sup95_rm=rm+ep_rm*qt(0.975,24) # limite superior do IC de 95% para resp media

ep_ip=s*sqrt(1+1/26+(172-mean(dados$peso))^2/sxx) # erro padrão da predição
ip=reta_mq$coefficients[1]+reta_mq$coefficients[2]*172
lim_inf95_ip=ip-ep_ip*qt(0.975,24) # limite inferior do IC de 95% para a predição
lim_sup95_ip=ip+ep_ip*qt(0.975,24) # limite superior do IC de 95% para a predição

lim_inf95_b_conj=reta_mq$coefficients[2]-epb*qt(0.9875,24) # limite inferior do IC conjunto de 95% para beta
lim_sup95_b_conj=reta_mq$coefficients[2]+epb*qt(0.9875,24) # limite superior do IC conjunto de 95% para beta
lim_inf95_a_conj=reta_mq$coefficients[1]-epa*qt(0.9875,24) # limite inferior do IC conjunto de 95% para intercepto
lim_sup95_a_conj=reta_mq$coefficients[1]+epa*qt(0.9875,24) # limite superior do IC conjunto de 95% para intercepto

poly_mq = lm(ps_sistolica~poly(peso, degree = 2, raw = TRUE), data = dados) # ajuste da regressão quadrática aos dados
# construção de gráficos da regressão quadrática
plot(dados$peso,dados$ps_sistolica,main="Polinômio de 2° grau ajustado",xlab="Peso",ylab="PS Sistólica")
x <- with(dados, seq(min(peso), max(peso), length.out=2000))
y <- predict(poly_mq, newdata = data.frame(peso = x))
lines(x, y, col = "red")

qmres_p=sum(poly_mq$residuals^2)/24 # quadrado médio dos resíduos da regressão quadrática
s_p=sqrt(qmres_p) # (s)

# construção de gráficos de resíduos da regrssão quadrática
residuo_padronizado_poly=(poly_mq$residuals-mean(poly_mq$residuals))/s_p
plot(poly_mq$fitted,residuo_padronizado_poly,main="Valores ajustados versus Resíduos padronizados (regressão quadrática)",xlab="Valores ajustados",ylab="Residuos padronizados",ylim=c(-2.5,2.5))
abline(h=0,lty=2, col="blue")
