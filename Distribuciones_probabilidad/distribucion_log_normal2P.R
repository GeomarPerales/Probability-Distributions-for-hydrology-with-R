#LOG NORMAL 2P

data<-read.csv("d:/geomar/modulos/programacion/R/statistical_hydrology/data.csv")

a<-data[['registro']]
Q<-data[['caudal']]

xm<-mean(Q) #promedio
dst<-sd(Q) #desviación standard
cv<-dst/xm #coef. de variación

lnQ=log(Q) #log natural de caudales
cvQ=dst/xm #coef. de variación

uy1<-0.5*log(xm^2/(1+cvQ^2))
dy1<-sqrt(log(1+cvQ^2))

v_ln2p<-c(cvQ,uy1,dy1)

TR<-c(2,5,10,20,50,100,200,500,1000)

#valores obtenidos de la tabla de frecuencias de weibull
#NOTA: ACOPLAR 'factores_frecuencia_weibull' A 'distribucion_<title>'
Px<-c(Px22[50],Px22[80],Px22[90],Px22[95],Px22[98],Px22[99],Px33[199],Px44[499],Px55[999])
vae<-c(k2[50],k2[80],k2[90],k2[95],k2[98],k2[99],k2[99],k3[199],k4[499],k5[999])

#matrix ceros
Q_ln2t<-rep(0,length(vae))
cv2p<-rep(0,length(vae))
Q_ln2k<-rep(0,length(vae))

#usando vae t
for (i in 1:length(vae)){
  Q_ln2t[i]<-exp(uy1+vae[i]*dy1)
}

#usando k
for (i in 1:length(vae)){
  cv2p[i]<-(exp((log(1+cvQ^2))^0.5*vae[i]-0.5*(log(1+cvQ^2)))-1)/cvQ
}

for (i in 1:length(vae)){
  Q_ln2k[i]<-xm+cv2p[i]*dst
}
