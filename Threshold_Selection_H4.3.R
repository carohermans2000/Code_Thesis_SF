library('mvPot')
library('POT')
library(extRemes)
library('zoom')

data = read.csv("C:\\Users\\caroh\\Downloads\\flare_catalog_plutino_2023_04__1986_01.csv")
pflux = data$peak_flux


#MRL plot Plutino 
par(mar = c(5,5,5,5))
u=seq(0,max(pflux),10^(-6))
x1=vector('numeric', length(u))
for(i in 1:length(x1)){ 
  threshold.exceedances = pflux[pflux>u[i]] 
  x1[i]=mean(threshold.exceedances-u[i]) }
plot(x1~u,type='l', main='MRL plot of Plutino dataset',ylab='Mean Excess', 
     cex.lab = 1.7, cex.main = 2.2, cex.axis = 1.5,
     xlab = 'Threshold')
abline(v = 0.00035, col = 'orange')
abline(v = 0.00048, col = 'blue')
abline(v = 0.0005, col = 'red')
abline(v = 0.0008, col = 'green')


#MRL plot adapted Plutino
u=seq(0,max(fluxies),10^(-6))
x2=vector('numeric', length(u))
for(i in 1:length(x2))
{ threshold.exceedances = fluxies[fluxies>u[i]] 
x2[i]=mean(threshold.exceedances-u[i]) }
plot(x2~u,type='l', main='MRL plot Adapted Plutino',ylab='Mean Excess', 
     cex.lab = 1.2, cex.main = 2, cex.axis = 1.2,
     xlab = 'Threshold')
abline(v = 0.00035, col = 'darkorange',)
abline(v = 0.00048, col = 'darkblue')
abline(v = 0.0005, col = 'darkred')
abline(v = 0.0008, col = 'darkgreen')

#TOGETHER ON ONE PLOT 
plot(x1~u,type='l', main='MRL plot',ylab='Mean Excess', 
     cex.lab = 1.7, cex.main = 2.2, cex.axis = 1.5,
     xlab = 'Threshold')
lines(u,x2, type = 'l', lty = 3, col = 'palevioletred')
abline(v = 0.00035, col = 'orange')
abline(v = 0.00048, col = 'blue')
abline(v = 0.0005, col = 'red')
abline(v = 0.0008, col = 'green')

legend("topright", legend = c("Original Plutino", "Adapted Plutino"), 
       col = c("black", "palevioletred"), lty = 1, cex = 1.5 )
#############
#NUMBER OF EVENTS PER THRESHOLD 


u=seq(0,max(pflux),10^(-5))
number = seq(0,max(pflux),10^(-5))
for(i in 1:length(u)){
  number[i] = length(pflux[pflux>u[i]])
}

plot(u,number,ylim= c(0,200),cex.lab = 1.2, cex.main = 2, 
     main = 'Number of events above a threshold choice',
     cex.axis = 1.2, xlab = 'Threshold', ylab = 'Number of events')


numberfluxies = seq(0,max(pflux),10^(-5))
for(i in 1:length(u)){
  numberfluxies[i] = length(fluxies[fluxies>u[i]])
}

par(mar = c(5, 6, 4, 2) + 0.1)  # Adjusting margins: bottom, left, top, right

# Initial plot
plot(u, numberfluxies, ylim= c(0,200), cex.lab = 1.5, cex.main = 2.2, 
     main = 'Number of events above a threshold choice',
     cex.axis = 1.7, xlab = 'Threshold', ylab = 'Number of events ', col = 'red', type = 'l')

# Adding the second plot
lines(u, number, col = 'blue', type = 'l')
legend("topright", legend=c("Adapted Plutino", "Original Plutino"), col=c("red", "blue"), lty=1, cex=1.2)
abline(v = 0.0005, lty = 'dotted', col = 'grey')
abline(h = 60,lty = 'dotted', col = 'pink')
abline(h = 65,lty = 'dotted', col = 'lightblue')
abline(v = 0.00035, lty = 'dotted', col = 'grey')
abline(h = 105,lty = 'dotted', col = 'pink')
abline(h = 110,lty = 'dotted', col = 'lightblue')
abline(v = 0.0008, lty = 'dotted', col = 'grey')
abline(h = 40,lty = 'dotted', col = 'lightblue')
abline(h = 37,lty = 'dotted', col = 'pink')

