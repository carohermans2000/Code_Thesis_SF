#RETURN PERIOD INVESTIGATION

years = 2023-1986
n = length(pflux)
ny = n/years

u=seq(0.0002,0.001,5*10^(-6))
C = vector('numeric',length(u))
Ninv = vector('numeric',length(u))
Ninvo = vector('numeric',length(u))
Ninvb = vector('numeric',length(u))
N = vector('numeric',length(u))

#####################################
nflux = length(fluxies)
nyflux = nflux/years

Cflux = vector('numeric',length(u))
Ninvflux = vector('numeric',length(u))
Nflux = vector('numeric',length(u))

#######################################
#Carrington event
for(i in 1:length(u)){
    C[i] = length(pflux[pflux>u[i]])/length(pflux)
    Ninv[i] = ny*C[i]*(1+shape[i]*((0.0045-u[i])/scale[i]))^(-1/shape[i])
    N[i] = 1/Ninv[i]
}
for(i in 1:length(u)){
  Cflux[i] = length(fluxies[fluxies>u[i]])/length(fluxies)
  Ninvflux[i] = nyflux*Cflux[i]*(1+shapefluxies[i]*((0.0045-u[i])/scalefluxies[i]))^(-1/shapefluxies[i])
  Nflux[i] = 1/Ninvflux[i]
}


plot(u,N,ylim = c(0,850), xlim = c(0.0002,0.00055),
     main = 'Return period of Carrington level', type = 'l',
    cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,xlab = 'Threshold u', ylab = 'Return period in years')
lines(u,Nflux,col = 'blue', lty = 2)
legend("topleft", legend = c("Original Plutino", "Adapted Plutino"), 
       col = c("black", "blue"), lty = c(1, 2), cex = 1.5)

abline(h = 100, col = 'grey', lty = 3)
abline(h = 200, col = 'grey', lty = 3)
abline(h = 300, col = 'grey', lty = 3)

plot(u,N,ylim = c(0,1000), xlim = c(0.00042,0.0005),
     main = 'Return period of Carrington level', type = 'l',
     cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,xlab = 'Threshold u', ylab = 'Return period in years')
lines(u,Nflux,col = 'blue', lty = 2)
legend("topleft", legend = c("Original Plutino", "Adapted Plutino"), 
       col = c("black", "blue"), lty = c(1, 2), cex = 1.5)

abline(h = 100, col = 'grey', lty = 3)
abline(h = 200, col = 'grey', lty = 3)
abline(v = 0.00045)
abline(v = 0.00048)

#####################################
#X25 event
for(i in 1:length(u)){
  C[i] = length(pflux[pflux>u[i]])/length(pflux)
  Ninv[i] = ny*C[i]*(1+shape[i]*((0.0025-u[i])/scale[i]))^(-1/shape[i])
  N[i] = 1/Ninv[i]
}
for(i in 1:length(u)){
  Cflux[i] = length(fluxies[fluxies>u[i]])/length(fluxies)
  Ninvflux[i] = nyflux*Cflux[i]*(1+shapefluxies[i]*((0.0025-u[i])/scalefluxies[i]))^(-1/shapefluxies[i])
  Nflux[i] = 1/Ninvflux[i]
}


plot(u,N,ylim = c(0,60),
     main = 'Return period of X25', type = 'l',
     cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,xlab = 'Threshold u', ylab = 'Return period in years')
lines(u,Nflux,col = 'blue', lty = 2)
legend("topleft", legend = c("Original Plutino", "Adapted Plutino"), 
       col = c("black", "blue"), lty = c(1, 2), cex = 1.5)

abline(h = 40, col = 'grey', lty = 3)
abline(h = 15, col = 'grey', lty = 3)
abline(v = 0.0005, col = 'grey', lty = 3)

u[61]
max(N[51:61])

#####################################
#X15 event
for(i in 1:length(u)){
  C[i] = length(pflux[pflux>u[i]])/length(pflux)
  Ninv[i] = ny*C[i]*(1+shape[i]*((0.0015-u[i])/scale[i]))^(-1/shape[i])
  N[i] = 1/Ninv[i]
}
for(i in 1:length(u)){
  Cflux[i] = length(fluxies[fluxies>u[i]])/length(fluxies)
  Ninvflux[i] = nyflux*Cflux[i]*(1+shapefluxies[i]*((0.0015-u[i])/scalefluxies[i]))^(-1/shapefluxies[i])
  Nflux[i] = 1/Ninvflux[i]
}


plot(u,N,ylim = c(0,5),
     main = 'Return period of X15', type = 'l',
     cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,xlab = 'Threshold u', ylab = 'Return period in years')
lines(u,Nflux,col = 'blue', lty = 2)
legend("topright", legend = c("Original Plutino", "Adapted Plutino"), 
       col = c("black", "blue"), lty = c(1, 2), cex = 1.5)

abline(h = 3.4, col = 'grey', lty = 3)
abline(h = 3.8, col = 'grey', lty = 3)
abline(h = 2.5, col = 'grey', lty = 3)
abline(h = 2.9, col = 'grey', lty = 3)
abline(v = 0.0005, col = 'grey', lty = 3)


###################################################
##############################################

u1=seq(0.0004,0.00049,10^(-6))

#Plutino
scale1 = vector('numeric',length = length(u1))
shape1 = vector('numeric',length = length(u1))
scalefluxies1 = vector('numeric',length = length(u1))
shapefluxies1 = vector('numeric',length = length(u1))

for(i in 1:length(u1)){
  print(i)
  gp_fit = fevd(pflux, threshold = u1[i],type = 'GP', method = 'MLE')
  scale1[i] = unname(gp_fit$results$par[1])
  shape1[i] = unname(gp_fit$results$par[2])
  gp_fit1 = fevd(fluxies, threshold = u1[i], type = 'GP', method = 'MLE',time.units = '6086.189/year',)
  scalefluxies1[i] = unname(gp_fit1$results$par[1])
  shapefluxies1[i] = unname(gp_fit1$results$par[2])
  
}

scalester1 = scale1-shape1*u1
scalesterfluxies1 = scalefluxies1-shapefluxies1*u1

C1 = vector('numeric',length(u1))
Ninv1 = vector('numeric',length(u1))
Ninvo1 = vector('numeric',length(u1))
Ninvb1 = vector('numeric',length(u1))
N1 = vector('numeric',length(u1))


nflux1 = length(fluxies)
nyflux1 = nflux/years

Cflux1 = vector('numeric',length(u1))
Ninvflux1 = vector('numeric',length(u1))
Nflux1 = vector('numeric',length(u1))


#Carrington event
for(i in 1:length(u1)){
  C1[i] = length(pflux[pflux>u1[i]])/length(pflux)
  Ninv1[i] = ny*C1[i]*(1+shape1[i]*((0.0045-u1[i])/scale1[i]))^(-1/shape1[i])
  N1[i] = 1/Ninv1[i]
}
for(i in 1:length(u1)){
  Cflux1[i] = length(fluxies[fluxies>u1[i]])/length(fluxies)
  Ninvflux1[i] = nyflux*Cflux1[i]*(1+shapefluxies1[i]*((0.0045-u1[i])/scalefluxies1[i]))^(-1/shapefluxies1[i])
  Nflux1[i] = 1/Ninvflux1[i]
}


plot(u1,scale1, main = 'Scale parameter for different values of the threshold',
     cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,xlab = 'Threshold choice u', 
     ylab = 'Scale parameter',type = 'l', col = 'black',ylim = c(0.0003,0.0006))
lines(u,scalefluxies, col = 'blue', type = 'l',lty = 2)
legend("bottomright", legend = c("Original Plutino", "Adapted Plutino"), 
       col = c("black", "blue"), lty = c(1, 2), cex = 1.5)


plot(u1,shape1, main = 'Shape parameter for different values of the threshold',
     cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7, xlab = 'Threshold choice u', 
     ylab = 'Shape parameter', type = 'l',ylim = c(-0.1,0.3))
lines(u,shapefluxies, col = 'blue', type = 'l',lty = 2)
legend("topright", legend = c("Original Plutino", "Adapted Plutino"), 
       col = c("black", "blue"), lty = c(1, 2), cex = 1.5)


plot(u1,scalester1, main = 'Scale* parameter for different values of the threshold',
     cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7, xlab = 'Threshold choice u', 
     ylab = 'Scale* parameter', type = 'l', ylim = c(0.0003,0.0007))
lines(u,scalesterfluxies, col = 'blue', type = 'l',lty = 2)
legend("bottomright", legend = c("Original Plutino", "Adapted Plutino"), 
       col = c("black", "blue"), lty = c(1, 2), cex = 1.5)



plot(u1,N1,ylim = c(50,250), xlim = c(0.00043,0.00046),
     main = 'Return period of Carrington level', type = 'l',
     cex.main = 2.2, cex.lab = 1.5, 
     cex.axis = 1.7,xlab = 'Threshold u', 
     ylab = 'Return period in years')
lines(u1,Nflux1,col = 'blue', lty = 2)
legend("bottomright", legend = c("Original Plutino", "Adapted Plutino"), 
       col = c("black", "blue"), lty = c(1, 2), cex = 1.5)

abline(h = 100, col = 'grey', lty = 3)
abline(h = 200, col = 'grey', lty = 3)
abline(v = 0.00045,col= 'red')
abline(v = 0.000485, col='red')

