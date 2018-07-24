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
