#fitting 
library(extRemes)


u1 = 0.00035
u2 = 0.0004
u3 = 0.00045
u4 = 0.000485
u4a = 0.00048
u4b = 0.000475
u5 = 0.0005
u6 = 0.00055
u7 = 0.0006
u8 = 0.0008

year = 2023-1986
year
nyflux = length(fluxies)/year
nyflux

gp_fit1f = fevd(fluxies, threshold = u1, type = "GP", span = year,time.units = '6086.189/year', method = 'MLE')
gp_fit2f = fevd(fluxies, threshold = u2, type = "GP", span = year,time.units = '6086.189/year', method = 'MLE')
gp_fit3f = fevd(fluxies, threshold = u3, type = "GP", span = year,time.units = '6086.189/year', method = 'MLE')
gp_fit4f = fevd(fluxies, threshold = u4, type = "GP", span = year,time.units = '6086.189/year', method = 'MLE')
gp_fit5f = fevd(fluxies, threshold = u5, type = "GP", span = year,time.units = '6086.189/year', method = 'MLE')
gp_fit6f = fevd(fluxies, threshold = u6, type = "GP", span = year,time.units = '6086.189/year', method = 'MLE')
gp_fit7f = fevd(fluxies, threshold = u7, type = "GP", span = year,time.units = '6086.189/year', method = 'MLE')
gp_fit8f = fevd(fluxies, threshold = u8, type = "GP", span = year,time.units = '6086.189/year', method = 'MLE')

plot(gp_fit1f, 'qq', main = TeX(r'(Quantile plot for $u_1$ = 0.00035)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)#
plot(gp_fit2f, 'qq', main = TeX(r'(Quantile plot for $u_2$ = 0.0004)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)
plot(gp_fit3f, 'qq', main = TeX(r'(Quantile plot for $u_3$ = 0.00045)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)
plot(gp_fit4f, 'qq', main = TeX(r'(Quantile plot for $u_4$ = 0.000485)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)#
plot(gp_fit5f, 'qq', main = TeX(r'(Quantile plot for $u_5$ = 0.0005)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)
plot(gp_fit6f, 'qq', main = TeX(r'(Quantile plot for $u_6$ = 0.00055)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)
plot(gp_fit7f, 'qq', main = TeX(r'(Quantile plot for $u_7$ = 0.0006)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)#
plot(gp_fit8f, 'qq', main = TeX(r'(Quantile plot for $u_8$ = 0.0008)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)

plot(gp_fit1f,'probprob',main = TeX(r'(Probability plot for $u_1$ = 0.00035)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)#
plot(gp_fit2f,'probprob',main = TeX(r'(Probability plot for $u_2$ = 0.0004)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)#
plot(gp_fit3f,'probprob',main = TeX(r'(Probability plot for $u_3$ = 0.00045)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)#
plot(gp_fit4f,'probprob',main = TeX(r'(Probability plot for $u_4$ = 0.000485)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)#
plot(gp_fit5f,'probprob',main = TeX(r'(Probability plot for $u_5$ = 0.0005)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)#
plot(gp_fit6f,'probprob',main = TeX(r'(Probability plot for $u_6$ = 0.00055)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)#
plot(gp_fit7f,'probprob',main = TeX(r'(Probability plot for $u_7$ = 0.0006)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)#
plot(gp_fit8f,'probprob',main = TeX(r'(Probability plot for $u_8$ = 0.0008)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)#


plot(gp_fit1f, 'rl',main = TeX(r'(Return Level plot for $u_1$ = 0.00035)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)#
abline(h = 0.0045,col= 'orange', lty = 'dotted')
abline(v = 54, col = 'orange', lty = 'dotted')
abline(h = 0.0025, col = 'blue', lty = 'dotted')
abline(v = 12,col = 'blue', lty = 'dotted')
text(54,0.0045, labels = 'X45', col = 'darkorange', adj = 0.5)
text(12,0.0025, labels = 'X25', col = 'darkblue', adj = 0.5)


plot(gp_fit2f, 'rl', main = TeX(r'(Return Level plot for $u_2$ = 0.0004)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)
abline(h = 0.0045,col= 'orange', lty = 'dotted')
abline(v = 120, col = 'orange',  lty = 'dotted')
abline(h = 0.0025, col = 'blue', lty = 'dotted')
abline(v = 16,col = 'blue', lty = 'dotted')
text(120,0.0045, labels = 'X45', col = 'darkorange', adj = 0.5)
text(16,0.0025, labels = 'X25', col = 'darkblue', adj = 0.5)


plot(gp_fit3f, 'rl', main = TeX(r'(Return Level plot for $u_3$ = 0.00045)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)
abline(h = 0.0045,col= 'orange',lty = 'dotted')
abline(v = 220, col = 'orange',lty = 'dotted')
abline(h = 0.0025, col = 'blue', lty = 'dotted')
abline(v = 18,col = 'blue', lty = 'dotted')
text(220,0.0045, labels = 'X45', col = 'darkorange', adj = 0.5)
text(18,0.0025, labels = 'X25', col = 'darkblue', adj = 0.5)

plot(gp_fit4f, 'rl', main = TeX(r'(Return Level plot for $u_4$ = 0.000485)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)
abline(h = 0.0045,col= 'orange',lty = 'dotted')
abline(h = 0.0025, col = 'blue', lty = 'dotted')
abline(v = 22,col = 'blue', lty = 'dotted')
text(22.5,0.0025, labels = 'X25', col = 'darkblue', adj = 0.5)


plot(gp_fit5f, 'rl',main = TeX(r'(Return Level plot for $u_5$ = 0.0005)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)
abline(h = 0.0045,col= 'orange',lty = 'dotted')
abline(h = 0.0025, col = 'blue', lty = 'dotted')
abline(v = 31,col = 'blue', lty = 'dotted')
text(31,0.0025, labels = 'X25', col = 'darkblue', adj = 0.5)

plot(gp_fit6f, 'rl', main = TeX(r'(Return Level plot for $u_6$ = 0.00055)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)
abline(h = 0.0045,col= 'orange', lty = 'dotted')
abline(h = 0.0025, col = 'blue', lty = 'dotted')
abline(v = 32,col = 'blue', lty = 'dotted')
text(32,0.0025, labels = 'X25', col = 'darkblue', adj = 0.5)


plot(gp_fit7f, 'rl',main = TeX(r'(Return Level plot for $u_7$ = 0.0006)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)
abline(h = 0.0045,col= 'orange',lty = 'dotted')
abline(h = 0.0025, col = 'blue', lty = 'dotted')
abline(v = 33,col = 'blue', lty = 'dotted')
text(33,0.0025, labels = 'X25', col = 'darkblue', adj = 0.5)


plot(gp_fit8f, 'rl',main = TeX(r'(Return Level plot for $u_8$ = 0.0008)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)
abline(h = 0.0045,col= 'orange',lty = 'dotted')
abline(h = 0.0025, col = 'blue', lty = 'dotted')
abline(v = 37,col = 'blue', lty = 'dotted')
text(37,0.0025, labels = 'X25', col = 'darkblue', adj = 0.5)



gp_fit4a = fevd(fluxies, threshold = u4a, type = "GP", span = year,time.units = '6086.189/year', method = 'MLE')
plot(gp_fit4a, 'qq', main = TeX(r'(Quantile plot for $u_{4a}$ = 0.00048)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)#
plot(gp_fit4a,'probprob',main = TeX(r'(Probability plot for $u_{4a}$ = 0.00048)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)#
plot(gp_fit4, 'rl', main = TeX(r'(Return Level plot for $u_{4a}$ = 0.00048)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)
abline(h = 0.0045,col= 'orange',lty = 'dotted')
abline(h = 0.0025, col = 'blue', lty = 'dotted')
abline(v = 380,col = 'orange', lty = 'dotted')
abline(v = 19.5,col = 'blue', lty = 'dotted')
text(20,0.0025, labels = 'X25', col = 'darkblue', adj = 0.5)
text(380,0.0045, labels = 'X45', col = 'darkorange', adj = 0.5)


gp_fit4b = fevd(fluxies, threshold = 0.00048, type = "GP", span = year,time.units = '6086.189/year', method = 'MLE')
plot(gp_fit4b, 'qq', main = TeX(r'(Quantile plot for $u_4b$ = 0.000475)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)#
plot(gp_fit4b,'probprob',main = TeX(r'(Probability plot for $u_4b$ = 0.000475)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)#
plot(gp_fit4b, 'rl', main = TeX(r'(Return Level plot for $u_{4b}$ = 0.00048)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)
abline(h = 0.0045,col= 'orange',lty = 'dotted')
abline(h = 0.0025, col = 'blue', lty = 'dotted')
abline(v = 500,col = 'orange', lty = 'dotted')
abline(v = 21,col = 'blue', lty = 'dotted')
text(22,0.0025, labels = 'X25', col = 'darkblue', adj = 0.5)
text(500,0.0045, labels = 'X45', col = 'darkorange', adj = 0.5)


plot(gp_fit4, 'rl', main = TeX(r'(Return Level plot for $u_{4a}$ = 0.00048)'),cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)
abline(v = 703.7671,col = 'orange', lty = 'dotted')

