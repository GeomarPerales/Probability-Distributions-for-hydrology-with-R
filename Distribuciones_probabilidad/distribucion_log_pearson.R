
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
