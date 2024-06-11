#fitting 
library(extRemes)
library('latex2exp')

data = read.csv("C:\\Users\\caroh\\Downloads\\flare_catalog_plutino_2023_04__1986_01.csv")
pflux = data$peak_flux

u1 = 0.00035
u2 = 0.0004
u3 = 0.00045
u4 = 0.000485
u5 = 0.0005
u6 = 0.00055
u7 = 0.0006
u8 = 0.0008

year = 2023-1986
year
ny = length(pflux)/year
ny

gp_fit1 = fevd(pflux, threshold = u1, type = "GP", span = year,time.units = '9699.595/year',method = 'MLE')
gp_fit2 = fevd(pflux, threshold = u2, type = "GP", span = year,time.units = '9699.595/year',method = 'MLE')
gp_fit3 = fevd(pflux, threshold = u3, type = "GP", span = year,time.units = '9699.595/year',method = 'MLE')
gp_fit4 = fevd(pflux, threshold = u4, type = "GP", span = year,time.units = '9699.595/year',method = 'MLE')
gp_fit5 = fevd(pflux, threshold = u5, type = "GP", span = year,time.units = '9699.595/year',method = 'MLE')
gp_fit6 = fevd(pflux, threshold = u6, type = "GP", span = year,time.units = '9699.595/year',method = 'MLE')
gp_fit7 = fevd(pflux, threshold = u7, type = "GP", span = year,time.units = '9699.595/year',method = 'MLE')
gp_fit8 = fevd(pflux, threshold = u8, type = "GP", span = year,time.units = '9699.595/year',method = 'MLE')

plot(gp_fit1, 'qq', main = TeX(r'(Quantile plot for $u_1$ = 0.00035)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)#
plot(gp_fit2, 'qq', main = TeX(r'(Quantile plot for $u_2$ = 0.0004)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)
plot(gp_fit3, 'qq', main = TeX(r'(Quantile plot for $u_3$ = 0.00045)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)
plot(gp_fit4, 'qq', main = TeX(r'(Quantile plot for $u_4$ = 0.000485)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)#
plot(gp_fit5, 'qq', main = TeX(r'(Quantile plot for $u_5$ = 0.0005)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)
plot(gp_fit6, 'qq', main = TeX(r'(Quantile plot for $u_6$ = 0.00055)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)
plot(gp_fit7, 'qq', main = TeX(r'(Quantile plot for $u_7$ = 0.0006)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)#
plot(gp_fit8, 'qq', main = TeX(r'(Quantile plot for $u_8$ = 0.0008)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)

plot(gp_fit1,'probprob',main = TeX(r'(Probability plot for $u_1$ = 0.00035)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)#
plot(gp_fit2,'probprob',main = TeX(r'(Probability plot for $u_2$ = 0.0004)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)#
plot(gp_fit3,'probprob',main = TeX(r'(Probability plot for $u_3$ = 0.00045)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)#
plot(gp_fit4,'probprob',main = TeX(r'(Probability plot for $u_4$ = 0.000485)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)#
plot(gp_fit5,'probprob',main = TeX(r'(Probability plot for $u_5$ = 0.0005)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)#
plot(gp_fit6,'probprob',main = TeX(r'(Probability plot for $u_6$ = 0.00055)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)#
plot(gp_fit7,'probprob',main = TeX(r'(Probability plot for $u_7$ = 0.0006)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)#
plot(gp_fit8,'probprob',main = TeX(r'(Probability plot for $u_8$ = 0.0008)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)#


plot(gp_fit1, 'rl',main = TeX(r'(Return Level plot for $u_1$ = 0.00035)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)#
abline(h = 0.0045,col= 'orange', lty = 'dotted')
abline(v = 52, col = 'orange', lty = 'dotted')
abline(h = 0.0025, col = 'blue', lty = 'dotted')
abline(v = 11,col = 'blue', lty = 'dotted')
text(52,0.0045, labels = 'X45', col = 'darkorange', adj = 0.5)
text(11,0.0025, labels = 'X25', col = 'darkblue', adj = 0.5)


plot(gp_fit2, 'rl', main = TeX(r'(Return Level plot for $u_2$ = 0.0004)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)
abline(h = 0.0045,col= 'orange', lty = 'dotted')
abline(v = 132, col = 'orange',  lty = 'dotted')
abline(h = 0.0025, col = 'blue', lty = 'dotted')
abline(v = 15,col = 'blue', lty = 'dotted')
text(132,0.0045, labels = 'X45', col = 'darkorange', adj = 0.5)
text(15,0.0025, labels = 'X25', col = 'darkblue', adj = 0.5)


plot(gp_fit3, 'rl', main = TeX(r'(Return Level plot for $u_3$ = 0.00045)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)
abline(h = 0.0045,col= 'orange',lty = 'dotted')
abline(v = 450, col = 'orange',lty = 'dotted')
abline(h = 0.0025, col = 'blue', lty = 'dotted')
abline(v = 18,col = 'blue', lty = 'dotted')
text(450,0.0045, labels = 'X45', col = 'darkorange', adj = 0.5)
text(18,0.0025, labels = 'X25', col = 'darkblue', adj = 0.5)

plot(gp_fit4, 'rl', main = TeX(r'(Return Level plot for $u_4$ = 0.000485)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)
abline(h = 0.0045,col= 'orange',lty = 'dotted')
abline(v = 630, col = 'orange',lty = 'dotted')
abline(h = 0.0025, col = 'blue', lty = 'dotted')
abline(v = 18.5,col = 'blue', lty = 'dotted')
text(630,0.0045, labels = 'X45', col = 'darkorange', adj = 0.5)
text(18.5,0.0025, labels = 'X20', col = 'darkblue', adj = 0.5)


plot(gp_fit5, 'rl',main = TeX(r'(Return Level plot for $u_5$ = 0.0005)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)
abline(h = 0.0045,col= 'orange',lty = 'dotted')
abline(h = 0.0025, col = 'blue', lty = 'dotted')
abline(v = 32,col = 'blue', lty = 'dotted')
text(32,0.0025, labels = 'X20', col = 'darkblue', adj = 0.5)

plot(gp_fit6, 'rl', main = TeX(r'(Return Level plot for $u_6$ = 0.00055)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)
abline(h = 0.0045,col= 'orange', lty = 'dotted')
abline(h = 0.0025, col = 'blue', lty = 'dotted')
abline(v = 35,col = 'blue', lty = 'dotted')
text(35,0.0025, labels = 'X20', col = 'darkblue', adj = 0.5)


plot(gp_fit7, 'rl',main = TeX(r'(Return Level plot for $u_7$ = 0.0006)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)
abline(h = 0.0045,col= 'orange',lty = 'dotted')
abline(h = 0.0025, col = 'blue', lty = 'dotted')
abline(v = 35,col = 'blue', lty = 'dotted')
text(35,0.0025, labels = 'X20', col = 'darkblue', adj = 0.5)


plot(gp_fit8, 'rl',main = TeX(r'(Return Level plot for $u_8$ = 0.0008)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)
abline(h = 0.0045,col= 'orange',lty = 'dotted')
abline(h = 0.0025, col = 'blue', lty = 'dotted')
abline(v = 37,col = 'blue', lty = 'dotted')
text(37,0.0025, labels = 'X25', col = 'darkblue', adj = 0.5)

plot(gp_fit1, 'density', main = 'Density function of u1')
plot(gp_fit2, 'density', main = 'Density function of u2')
plot(gp_fit3, 'density', main = 'Density function of u3')
plot(gp_fit4, 'density', main = 'Density function of u4')
plot(gp_fit5, 'density', main = 'Density function of u5')
plot(gp_fit6, 'density', main = 'Density function of u6')
plot(gp_fit7, 'density', main = 'Density function of u7')
plot(gp_fit8, 'density', main = 'Density function of u8')
