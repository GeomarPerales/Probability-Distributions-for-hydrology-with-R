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
Q_g<-rep(0,length(vae))

for (i in 1:length(vae)){
  k_g[i]<-(Y_gt[i]-x_g)/dst_g
  Q_g[i]<-rep(0,length(vae))
}