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
M<-rep(0,length(vae))
DS<-rep(0,length(vae))

for (i in 1:length(vae)){
  M[i]<-mean(A[i,1:Tm[i]])
  DS[i]<-sd(A[i,1:Tm[i]])*(length(DS)-1)/length(DS)
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