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
