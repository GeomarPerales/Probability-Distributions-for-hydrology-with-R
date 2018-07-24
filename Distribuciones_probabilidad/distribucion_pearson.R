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
  p22[i]<--(vae[j]^2-1)*gc1^3+vae[i]*gc1^4+gc1^5/3 
  k_p[i]<-p11[i]+p22[i]
}

Q_pk<-rep(0,length(vae))

for (i in 1:length(vae)){
  Q_pk[i]<-xm+k_p[i]*dst
}


