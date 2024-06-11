library(ismev)
data(rain)

p = max(rain, na.rm = TRUE)
p
u=seq(0,p,0.8)
x=vector('numeric', length(u))
for(i in 1:length(x)){
  i
  threshold.exceedances=rain[rain>u[i]] 
x[i]=mean(threshold.exceedances-u[i]) }
plot(x~u,type='l', main='MRL plot',ylab='mean excess', cex.main = 2, cex.lab = 1.2, cex.axis = 1.2, 
     xlab = 'Threshold choice u')
abline(v = 30, col = 'red')
abline(v = 60, col = 'blue')

library(extRemes)
gp_fit <- fevd(rain, threshold = 30, type = "GP")
plot(gp_fit)#ziet er gelijk uit 

threshold = 30
C = length(rain[rain>threshold])/length(rain)
C #komt zelfde uit 
scalepara = unname(gp_fit$results$par[1])
scalepara
shapepara = unname(gp_fit$results$par[2])
shapepara
#komen beide weer gelijk uit 

N = 100
ny = 365
zn = threshold + scalepara/shapepara*((N*ny*C)^shapepara -1)
zn #komt ook exact hetzelfde uit! 
