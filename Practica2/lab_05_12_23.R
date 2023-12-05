search()
muestra
clasificacion=rpart(C.G~T+L+P, data=muestra, method = "class", minsplit=1)

regresion=lm(D~R, data=planetas)

summary(regresion)

(res=summary(regresion)$residuals)

(sr=sqrt(sum(res^2)/4))

for(i in 1:length(res))
{if (res[i]>3*sr)
{print("el suceso"); print(res[i]); print("es un suceso anómalo outlier")}}

(muestra=read.table("RD.txt"))

(dfr=lm(muestra$D~muestra$R))

(res=summary(dfr)$residuals)

(sr=sqrt(sum(res^2)/7))

for(i in 1:length(res))
{if (res[i]>2*sr)
{print("el suceso"); print(res[i]); print("es un suceso anómalo outlier")}}
