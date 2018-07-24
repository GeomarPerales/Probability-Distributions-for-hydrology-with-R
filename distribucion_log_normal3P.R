#LOG NORMAL 3P
sg<-0

#coef. asimetria
for (i in 1:length(Q)){
  sg<-sg+(Q(i)-xm)^3/length(Q)
}

g<-length(Q)^2*sg/(length(Q)-1)/(length(Q)-2)/dst^3
cs<-g

W<-(-g+sqrt(g^2+4))*0.5
Z2<-(1-W^(2/3))/W^(1/3)
dy2<-(log(Z2^2+1))^(1/2)
uy2<-log(dst/Z2)-0.5*log(Z2^2+1)
xo<-xm-dst/Z2

v_ln3p<-c(cs,W,Z2,dy2,uy2,xo)

TR<-c(2,5,10,20,50,100,200,500,1000)

#valores obtenidos de la tabla de frecuencias de weibull
#NOTA: ACOPLAR 'factores_frecuencia_weibull' A 'distribucion_<title>'
Px<-c(Px22[50],Px22[80],Px22[90],Px22[95],Px22[98],Px22[99],Px33[199],Px44[499],Px55[999])
vae<-c(k2[50],k2[80],k2[90],k2[95],k2[98],k2[99],k2[99],k3[199],k4[499],k5[999])

#matriz ceros
Q_ln3t<-rep(0,length(vae))
cv3p<-rep(0,length(vae))
Q_ln3k<-rep(0,length(vae))

#usando vae t
for (i in 1:length(vae)){
  Q_ln3t[i]<-xo+exp(uy2+vae[i]*dy2)
}

#usando k
for (i in 1:length(vae)){
  cv3p[i]<-(exp((log(1+Z2^2))^0.5*vae[i]-0.5*(log(1+Z2^2)))-1)/Z2
}

for (i in 1:length(vae)){
  Q_ln3k[i]<-xm+cv3p[i]*dst
}