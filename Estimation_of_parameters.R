library('POT')
library('mvPot')
library('extRemes')

data = read.csv("C:\\Users\\caroh\\Downloads\\flare_catalog_plutino_2023_04__1986_01.csv")
pflux = data$peak_flux

#Plutino
u=seq(0.0002,0.001,5*10^(-6))
scale = vector('numeric',length = length(u))
shape = vector('numeric',length = length(u))
for(i in 1:length(u)){
  print(i)
  gp_fit = fevd(pflux, threshold = u[i],type = 'GP', method = 'MLE')
  scale[i] = unname(gp_fit$results$par[1])
  shape[i] = unname(gp_fit$results$par[2])
}

scalester = scale-shape*u

bound=u
for(i in 1:length(u)){
  if (shape[i]>0){
    bound[i] = 0
  }else{
    bound[i] = u[i]-scale[i]/shape[i]
  }
}

#Adapted Plutino

scalefluxies = vector('numeric',length = length(u))
shapefluxies = vector('numeric',length = length(u))
for(i in 1:length(u)){
  print(i)
  gp_fit = fevd(fluxies, threshold = u[i], type = 'GP', method = 'MLE',time.units = '6086.189/year',)
  scalefluxies[i] = unname(gp_fit$results$par[1])
  shapefluxies[i] = unname(gp_fit$results$par[2])
}

scalesterfluxies = scalefluxies-shapefluxies*u

boundflux = u
for(i in 1:length(u)){
  if (shapefluxies[i]>0){
    boundflux[i] = 0
  }else{
    boundflux[i] = u[i]-scalefluxies[i]/shapefluxies[i]
  }
}

##############################################
##############################################

plot(u,scale, main = 'Scale parameter for different values of the threshold',
     cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,xlab = 'Threshold choice u', 
     ylab = 'Scale parameter',type = 'l', col = 'black')
lines(u,scalefluxies, col = 'blue', type = 'l',lty = 2)
legend("bottomright", legend = c("Original Plutino", "Adapted Plutino"), 
       col = c("black", "blue"), lty = c(1, 2), cex = 1.5)


plot(u,shape, main = 'Shape parameter for different values of the threshold',
     cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7, xlab = 'Threshold choice u', 
     ylab = 'Shape parameter', type = 'l')
lines(u,shapefluxies, col = 'blue', type = 'l',lty = 2)
legend("topright", legend = c("Original Plutino", "Adapted Plutino"), 
       col = c("black", "blue"), lty = c(1, 2), cex = 1.5)
abline(h = 0, lty = 'dotted', col = 'red')
abline(v = 0.0004, lty = 'dotted')
abline(v = 0.00045, lty = 'dotted')
abline(v = 0.0005, lty = 'dotted')
abline(v = 0.0008, lty = 'dotted')

plot(u,scalester, main = 'Scale* parameter for different values of the threshold',
     cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7, xlab = 'Threshold choice u', 
     ylab = 'Scale* parameter', type = 'l')
lines(u,scalesterfluxies, col = 'blue', type = 'l',lty = 2)
legend("bottomright", legend = c("Original Plutino", "Adapted Plutino"), 
       col = c("black", "blue"), lty = c(1, 2), cex = 1.5)
abline(v = 0.0004, lty = 'dotted')
abline(v = 0.00045, lty = 'dotted')
abline(v = 0.0005, lty = 'dotted')
abline(v = 0.0008, lty = 'dotted')

#############################################


plot(u,bound,ylim = c(0,0.008),
     main = 'Upper bound of estimation caused by negative shape parameter'
    , cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7, xlab = 'Threshold choice u', 
    ylab = 'Upperbound value', type = 'l')
lines(u,boundflux, col ='blue', type = 'l', lty =2)
legend("bottomright", legend = c("Original Plutino", "Adapted Plutino"), 
       col = c("black", "blue"), lty = c(1, 2), cex = 1.5)
abline(h = 0.0025, lty = 'dotted', col = 'red')
abline(h = 0.0035, lty = 'dotted', col = 'red')
abline(h = 0.0045, lty = 'dotted', col = 'red')


u[59]
shape[59]
u[shape<0]
shape[u==0.00049]


#####Compare 
deltascale = scale - scalefluxies
deltashape = shape - shapefluxies
deltaster = scalester - scalesterfluxies
plot(u, deltascale, main = 'Comparison of Scale parameters', 
     cex.main = 2, cex.lab = 1.2, cex.axis = 1.2,xlab = 'Threshold choice u', 
     ylab = 'Difference in scale parameter')
plot(u, deltashape , main = 'Comparison of Shape parameters', 
     cex.main = 2, cex.lab = 1.2, cex.axis = 1.2,xlab = 'Threshold choice u', 
     ylab = 'Difference in shape parameter')
plot(u, deltaster, main = 'Comparison of Scale* parameters', 
     cex.main = 2, cex.lab = 1.2, cex.axis = 1.2,xlab = 'Threshold choice u', 
     ylab = 'Difference in scale* parameter')


