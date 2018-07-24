#hidrología estadistica

rm(list=ls())
cd<-setwd('d:/geomar/modulos/programacion/R')

#TABLA F(Z)
#coeficientes

ao<-2.490895
a1<-1.466003
a2<--0.024343
a3<-0.178257

#tabla de ordenadas
#horizontal
th<-seq(0,0.09,by=0.01)

#vertical
tv<-seq(0,3.9,by=0.1)

#matriz ceros
Fz<-matrix(0,length(tv),length(th))

#funcion
#Fz=(ao+a1*t^2+a2*t^4+a3*t^6)^(-1)
for (i in 1:length(tv)){
  for (j in 1:length(th)){
    Fz[i,j]<-((ao+a1*(tv[i]+th[j])^2+a2*(tv[i]+th[j])^4+a3*(tv[i]+th[j])^6)^(-1))
  }
}

#ABRAMOWITZ Y STEGUN
#CALCULO DE t
#coeficientes
as_a3<-0.33267

#matrix ceros
t_as<-matrix(0,length(tv),length(th))

#funcion
for (i in 1:length(tv)){
  for (j in 1:length(th)){
    t_as[i,j]<-1/(1+as_a3*(tv[i]+th[j]))
  }
}

#calculo de Fz
#coeficientes
ao_as<-0.43618
a1_as<-0.12017
a2_as<-0.9373

#vertical para AS
tv_as<-seq(0,3.1,by=0.1)

#matriz ceros
Fz_as<-matrix(0,length(tv_as),length(th))

#Fz
for (i in 1:length(tv_as)){
  for (j in 1:length(th)){
    Fz_as[i,j]<-1-Fz[i,j]*(ao_as*t_as[i,j]-a1_as*t_as[i,j]^2+a2_as*t_as[i,j]^3)
  }
}

#Masting
#calculo de t
#coeficientes
bo_ma<-0.232164

#matriz ceros
t_ma<-matrix(0,length(tv),length(th))

#funcion
for (i in 1:length(tv)){
  for (j in 1:length(th)){
    t_ma[i,j]<-1/(1+bo_ma*(tv[i]+th[j]))
  }
}

#calculo de Fz
#coeficientes
b1_ma<-0.31938
b2_ma<--0.35656
b3_ma<-1.78148
b4_ma<--1.82126
b5_ma<-1.33027

#matriz ceros
Fz_ma<-matrix(0,length(tv_as),length(th))
P_ma<-matrix(0,length(tv_as),length(th))

#funcion
for (i in 1:length(tv_as)){
  for (j in 1:length(th)){
    P_ma[i,j]=b1_ma*t_ma[i,j]+b2_ma*t_ma[i,j]^2+b3_ma*t_ma[i,j]^3+b4_ma*t_ma[i,j]^4+b5_ma*t_ma[i,j]^5
    Fz_ma[i,j]=1-Fz[i,j]*P_ma[i,j]
  }
}

#factor de frecuencia usando weibull
#coeficientes
co<-2.515517
c1<-0.802853
c2<-0.010328

d1<-1.432788
d2<-0.189269
d3<-0.001308

#muestra 1:59
S1<-seq(1,59,by=1)

#vectores ceros
Px1<-rep(0,length(S1))
Px11<-rep(0,length(S1))
TM1<-rep(0,length(S1))
W1<-rep(0,length(S1))
k1<-rep(0,length(S1))

for (i in 1:length(S1)){
  Px1[i]=i/(length(S1)+1)
  Px11[i]=i/(length(S1)+1)
  TM1[i]=1/(1-Px1[i])
  if (Px1[i]>=0.5){
    Px1[i]=1-Px1[i]
  }
  W1[i]=sqrt(log(1/Px1[i]^2))
  k1[i]=W1[i]-(co+c1*W1[i]+c2*W1[i]^2)/(1+d1*W1[i]+d2*W1[i]^2+d3*W1[i]^3)
}

#muestra 1:99
S2<-seq(1,99,by=1)

#vectores ceros
Px2<-rep(0,length(S2))
Px22<-rep(0,length(S2))
TM2<-rep(0,length(S2))
W2<-rep(0,length(S2))
k2<-rep(0,length(S2))

for (i in 1:length(S2)){
  Px2[i]=i/(length(S2)+1)
  Px22[i]=i/(length(S2)+1)
  TM2[i]=1/(1-Px2[i])
  if (Px2[i]>=0.5){
    Px2[i]=1-Px2[i]
  }
  W2[i]=sqrt(log(1/Px2[i]^2))
  k2[i]=W2[i]-(co+c1*W2[i]+c2*W2[i]^2)/(1+d1*W2[i]+d2*W2[i]^2+d3*W2[i]^3)
}

#muestra 1:199
S3<-seq(1,199,by=1)

#vectores ceros
Px3<-rep(0,length(S3))
Px33<-rep(0,length(S3))
TM3<-rep(0,length(S3))
W3<-rep(0,length(S3))
k3<-rep(0,length(S3))

for (i in 1:length(S3)){
  Px3[i]=i/(length(S3)+1)
  Px33[i]=i/(length(S3)+1)
  TM3[i]=1/(1-Px3[i])
  if (Px3[i]>=0.5){
    Px3[i]=1-Px3[i]
  }
  W3[i]=sqrt(log(1/Px3[i]^2))
  k3[i]=W3[i]-(co+c1*W3[i]+c2*W3[i]^2)/(1+d1*W3[i]+d2*W3[i]^2+d3*W3[i]^3)
}

#muestra 1:499
S4<-seq(1,499,by=1)

#vectores ceros
Px4<-rep(0,length(S4))
Px44<-rep(0,length(S4))
TM4<-rep(0,length(S4))
W4<-rep(0,length(S4))
k4<-rep(0,length(S4))

for (i in 1:length(S4)){
  Px4[i]=i/(length(S4)+1)
  Px44[i]=i/(length(S4)+1)
  TM4[i]=1/(1-Px4[i])
  if (Px4[i]>=0.5){
    Px4[i]=1-Px4[i]
  }
  W4[i]=sqrt(log(1/Px4[i]^2))
  k4[i]=W4[i]-(co+c1*W4[i]+c2*W4[i]^2)/(1+d1*W4[i]+d2*W4[i]^2+d3*W4[i]^3)
}

#muestra 1:999
S5<-seq(1,999,by=1)

#vectores ceros
Px5<-rep(0,length(S5))
Px55<-rep(0,length(S5))
TM5<-rep(0,length(S5))
W5<-rep(0,length(S5))
k5<-rep(0,length(S5))

for (i in 1:length(S5)){
  Px5[i]=i/(length(S5)+1)
  Px55[i]=i/(length(S5)+1)
  TM5[i]=1/(1-Px5[i])
  if (Px5[i]>=0.5){
    Px5[i]=1-Px5[i]
  }
  W5[i]=sqrt(log(1/Px5[i]^2))
  k5[i]=W5[i]-(co+c1*W5[i]+c2*W5[i]^2)/(1+d1*W5[i]+d2*W5[i]^2+d3*W5[i]^3)
}

#DISTRIBUCION NORMAL

#data<-scan("d:/geomar/modulos/programacion/R/statistical_hydrology/data.xlsx")
data<-read.csv("d:/geomar/modulos/programacion/R/statistical_hydrology/data.csv")

a<-data[['registro']]
Q<-data[['caudal']]

xm<-mean(Q) #promedio
dst<-sd(Q) #desviación standard
cv<-dst/xm #coef. de variación

v_N<-c(xm,dst,cv)

TR<-c(2,5,10,20,50,100,200,500,1000)

Px<-c(Px22[50],Px22[80],Px22[90],Px22[95],Px22[98],Px22[99],Px33[199],Px44[499],Px55[999])
vae<-c(k2[50],k2[80],k2[90],k2[95],k2[98],k2[99],k3[199],k4[499],k5[999])

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

#LOG NORMAL 2P

lnQ=log(Q) #log natural de caudales
cvQ=dst/xm #coef. de variación

uy1<-0.5*log(xm^2/(1+cvQ^2))
dy1<-sqrt(log(1+cvQ^2))

v_ln2p<-c(cvQ,uy1,dy1)

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

#LOG NORMAL 3P
sg<-0

#coef. asimetria
for (i in 1:length(Q)){
  sg<-sg+(Q[i]-xm)^3/length(Q)
}

g<-length(Q)^2*sg/(length(Q)-1)/(length(Q)-2)/dst^3
cs<-g

W<-(-g+sqrt(g^2+4))*0.5
Z2<-(1-W^(2/3))/W^(1/3)
dy2<-(log(Z2^2+1))^(1/2)
uy2<-log(dst/Z2)-0.5*log(Z2^2+1)
xo<-xm-dst/Z2

v_ln3p<-c(cs,W,Z2,dy2,uy2,xo)

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

#TABLA DE FACTOR DE FRECUENCIA GUMBEL
#para prob. de 50%,80%,90% y 95% con incremento de 5 unid.
Tm<-seq(10,100,by=5)

A<-matrix(0,nrow = length(Tm), ncol = 100)
n<-length(Tm)

#generación de los series
for (i in 1:length(Tm)){
  n[i]<-length(1:Tm[i])
  for (j in 1:length(1:Tm[i])){
   A[i,j]<--log(-log((n[i]+1-j)/(n[i]+1)))
  }
}

#vector zeros para la media y desv. standar poblacional
M<-rep(0,length(Tm))
DS<-rep(0,length(Tm))

for (i in 1:length(Tm)){
  M[i]<-mean(A[i,1:Tm[i]])
  DS[i]<-sd(A[i,1:Tm[i]])*sqrt((length(1:Tm[i])-1)/length(1:Tm[i]))
}

#vector de zeros
f_g<-rep(0,length(TR))

for (i in 1:length(vae)){
  f_g[i]<--log(-log((TR[i]-1)/TR[i]))
}

#vector de zeros
Yt<-matrix(0,nrow = length(Tm), length(f_g))

for (i in 1:length(Tm)){
  for (j in 1:length(f_g)){
    Yt[i,j]<-(M[i]-f_g[j])/DS[i]
  }
}

#DISTRIBUCION GUMBEL
a1<-1.2825/dst #alfa
u<-xm-0.45*dst #u

#vectores zero
Y_gt<-rep(0,length(Px))
Q_gt<-rep(0,length(Px))

#mediante t
for (i in 1:length(Px)){
    Y_gt[i]<--log(-log(Px[i]))
    Q_gt[i]<-Y_gt[i]/a1+u
}

#vector zeros
d_g<-rep(0,length(Q))

#mediante k
for (i in 1:length(vae)){
  d_g[i]<--log(-log((length(Q)+1-i)/(length(Q)+1)))
}

x_g<-mean(d_g)
dst_g<-sd(d_g)*sqrt((length(d_g)-1)/length(d_g))

v_g<-c(x_g,dst_g)

#zeros
k_g<-rep(0,length(vae))
Q_gk<-rep(0,length(vae))

for (i in 1:length(vae)){
  k_g[i]<-(Y_gt[i]-x_g)/dst_g
  Q_gk[i]<-xm+k_g[i]*dst
}

#TABLA DE FRECUENCIA PEARSON
#prob. y coef. de sesgo de 0.0 a 2.0 con incremento de 0.1
cs_p<-seq(0,2,by=0.1)

#vectores en zero
gc_p<-rep(0,length(cs_p))
p1<-matrix(0,length(cs_p),length(vae))
p2<-matrix(0,length(cs_p),length(vae))
k_p<-matrix(0,length(cs_p),length(vae))

for (i in 1:length(cs_p)){
  for (j in 1:length(vae)){
    p1[i,j]<-vae[j]+(vae[j]^2-1)*gc_p[i]+(vae[j]^3-6*vae[j])*gc_p[i]^2/3
    p2[i,j]<--(vae[j]^2-1)*gc_p[i]^3+vae[j]*gc_p[i]^4+gc_p[i]^5/3 
    k_p[i,j]<-p1[i,j]-p2[i,j]
  }
}

#DISTRIBUCION PEARSON
N<-length(Q)
gc<-cs/sqrt(N*(N-1))/(N-2)*(1+8.5/N)
be<-(2/gc)^2
al<-dst/sqrt(be)
y<-xm-dst*sqrt(be)
  
v_p<-c(gc,be,al,y)

#mediante t
#zeros
Q_pt<-rep(0,length(vae))

for (i in 1:length(vae)){
  Q_pt[i]<--al*be*(1-1/9/be+vae[i]*sqrt(1/9/be)^3+y)
}

#mediante k
#zeros
p11<-rep(0,length(vae))
p22<-rep(0,length(vae))
k_p<-rep(0,length(vae))

gc1<-gc/6
  
for (i in 1:length(cs_p)){
    p11[i]<-vae[i]+(vae[i]^2-1)*gc1+(vae[i]^3-6*vae[i])*gc1^2/3
    p22[i]<--(vae[i]^2-1)*gc1^3+vae[i]*gc1^4+gc1^5/3 
    k_p[i]<-p11[i]+p22[i]
  }

Q_pk<-rep(0,length(vae))

for (i in 1:length(vae)){
  Q_pk[i]<-xm+k_p[i]*dst
}

#DISTRIBUCION LOG PEARSON
xm_lp<-mean(lnQ)
ds_lp<-sd(lnQ)
cv_lp<-xm_lp/ds_lp

v_lp<-c(xm_lp,ds_lp,cv_lp)

sg_lp<-0
for (i in 1:length(Q)){
  sg_lp<-sg_lp+(lnQ[i]-xm_lp)^3/length(Q)
}

g_lp<-length(Q)^2*sg_lp/(length(Q)-1)/(length(Q)-2)/ds_lp^3

cs_lp<-g_lp #coef. asimetria segun fisher

gc_lp<-cs_lp/(sqrt(N*(N-1))/(N-2)*(1+8.5/N))
be_lp<-(2/gc_lp)^2 #beta 
sc<-ds_lp*sqrt(N/(N-1))
al_lp<-sc/sqrt(be_lp) #alfa
y_lp<-xm_lp-al_lp*be_lp

v_lp2<-c(cs_lp,gc_lp,be_lp,sc,al_lp,y_lp)

#variable t
#zeros
Q_lpt<-rep(0,length(vae))

for (i in 1:length(vae)){
  Q_lpt[i]<-exp(al_lp*be_lp*(1-1/9/be_lp+vae[i]*(1/9/be_lp)^0.5)^3+y_lp)
}

#zeros
p111<-rep(0,length(vae))
p222<-rep(0,length(vae))
k_lp<-rep(0,length(vae)) 

gc1<-gc_lp/6

for (i in 1:length(vae)){
  p111[i]<-vae[i]+(vae[i]^2-1)*gc1+(vae[i]^3-6*vae[i])*gc1^2/3
  p222[i]<--(vae[i]^2-1)*gc1^3+vae[i]*gc1^4+gc1^5/3 
  k_lp[i]<-p111[i]+p222[i]
}

#variable k
#zeros
Q_lpk<-rep(0,length(vae))

for (i in 1:length(vae)){
  Q_lpk[i]<-exp(xm_lp+k_lp[i]*ds_lp)
}

