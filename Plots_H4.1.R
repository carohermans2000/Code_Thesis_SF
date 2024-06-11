#Plots van events 

data = read.csv("C:\\Users\\caroh\\Downloads\\flare_catalog_plutino_2023_04__1986_01.csv")
pflux = data$peak_flux
dates = data$tstart
dates = as.POSIXlt(dates,tz=Sys.timezone())

#for adapted plutino
dates1 = as.POSIXlt(dates1,tz=Sys.timezone())

#General 
par(mar = c(5, 6, 4, 2) + 0.1)  # Adjusting margins: bottom, left, top, right
plot(dates,pflux, type = 'l', main = 'X-ray flux of solar flare events in PLutino dataset',
cex.lab = 1.7, cex.main = 2.2, cex.axis = 1.5, xlab = 'Time', ylab = 'X-ray flux level')
abline(v = 1996)


plot(dates1,fluxies, type = 'l', main = 'X-ray flux of solar flare events in Adapted PLutino dataset',
     cex.lab = 1.7, cex.main = 2.2, cex.axis = 1.2, xlab = 'Time', ylab = 'X-ray flux level')


#Halloweenstorms
dates2 = dates1[215000:225000]
flux1 = pflux[215000:225000]
plot(dates2,flux1,ylim = c(0,0.00005), 
     main = 'X-ray flux levels in period of Halloweenstorms',
     xlab = 'Time', ylab = 'X-ray flux',  cex.lab = 1.7, cex.main = 2.2, cex.axis = 1.5)


