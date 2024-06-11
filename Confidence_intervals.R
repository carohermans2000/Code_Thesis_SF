library('extRemes')
u=seq(0.0002,0.001,5*10^(-6))
scaleo = vector('numeric',length = length(u))
shapeo= vector('numeric',length = length(u))
scaleb = vector('numeric',length = length(u))
shape = vector('numeric',length = length(u))
scale = vector('numeric',length = length(u))
shapeb = vector('numeric',length = length(u))
for(i in 1:length(u)){
  print(i)
  gp_fit = fevd(pflux, threshold = u[i],type = 'GP', method = 'MLE')
  scale[i] = unname(gp_fit$results$par[1])
  shape[i] = unname(gp_fit$results$par[2])
  scaleo[i] = ci(gp_fit, type = 'parameter',alpha = 0.05,)[1,1]
  scaleb[i] = ci(gp_fit, type = 'parameter',alpha = 0.05,)[1,3]
  shapeo[i] = ci(gp_fit, type = 'parameter',alpha = 0.05,)[2,1]
  shapeb[i] = ci(gp_fit, type = 'parameter',alpha = 0.05,)[2,3]
  
}

scalester=scale-shape*u
scalesterb=scaleb-shapeb*u
scalestero=scaleo-shapeo*u

plot(u,scale, type = 'l', main = 'Scale parameter with confidence interval',
     xlab = 'Threshold u', ylab = 'Scale', cex.main = 2.2,
     cex.lab = 1.5, cex.axis = 1.7,xlim = c(0.00035,0.0008))
lines(u,scaleo, col = 'lightgreen')
lines(u,scaleb, col = 'lightgreen')

plot(u,scalester, type = 'l', main = 'Scale* parameter with confidence interval',
     xlab = 'Threshold u', ylab = 'Scale*', cex.main = 2.2,
     cex.lab = 1.5, cex.axis = 1.7)
lines(u,scalesterb, col = 'blue',lty =2)
lines(u,scalestero, col = 'blue',lty =2)

plot(u,shape, type = 'l', main = 'Shape parameter with confidence interval',
     xlab = 'Threshold u', ylab = 'Shape', cex.main = 2.2,
     cex.lab = 1.5, cex.axis = 1.7)
lines(u,shapeo, col = 'blue',lty =2)
lines(u,shapeb, col = 'blue',lty =2)
abline(v = 0.00047)
abline(v = 0.0005)

h = shapeb-shapeo
plot(u,h)


