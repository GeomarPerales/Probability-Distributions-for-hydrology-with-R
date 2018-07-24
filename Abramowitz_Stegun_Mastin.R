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

#MASTING
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