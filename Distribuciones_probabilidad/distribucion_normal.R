#DISTRIBUCION NORMAL

data<-read.csv("d:/geomar/modulos/programacion/R/statistical_hydrology/data.csv")

a<-data[['registro']]
Q<-data[['caudal']]

xm<-mean(Q) #promedio
dst<-sd(Q) #desviación standard
cv<-dst/xm #coef. de variación

v_N<-c(xm,dst,cv)

TR<-c(2,5,10,20,50,100,200,500,1000)

#valores obtenidos de la tabla de frecuencias de weibull
#NOTA: ACOPLAR 'factores_frecuencia_weibull' A 'distribucion_<title>'
Px<-c(Px22[50],Px22[80],Px22[90],Px22[95],Px22[98],Px22[99],Px33[199],Px44[499],Px55[999])
vae<-c(k2[50],k2[80],k2[90],k2[95],k2[98],k2[99],k2[99],k3[199],k4[499],k5[999])

Q_n<-rep(0,length(vae))

for (i in 1:length(vae)){
  Q_n[i]<-xm+vae[i]*dst
}

#tabla de factor de frecuencia para prob. 50%,80%,90% y 100%
#coef. de variación (cv) que varian de 0.05 hasta 1.00
cvn<-seq(0.05,1,by=0.05)

k_n<-matrix(0,nrow = length(cvn), ncol = length(vae))

for (i in 1:length(cvn)){
  for (j in 1:length(vae)){
    k_n[i,j]<-(exp((log(1+cvn[i]^2))^0.5*vae[j]-.5*(log(1+cvn[i]^2)))-1)/cvn[i]
  }
}
