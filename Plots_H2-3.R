library('evd')
###################
#H2

# Define the range of x values
x <- seq(-3,5, length.out = 100)

# Calculate the y values for each distribution
y_gumbel <- dgumbel(x, scale = 1, loc = 0)
y_weibull <- dweibull(x, shape = 1, scale = 1)
y_frechet <- dfrechet(x, shape = 1, scale = 1,loc = 0)

# Plot the first curve (Gumbel) and set up the plot
plot(x, y_gumbel, type = "l", col = "#CC00FF", ylim = c(0, max(y_gumbel, y_weibull, y_frechet)), 
     ylab = "Density", xlab = "x", main = 'Density functions for shape = 1, scale = 1'
     ,cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)

# Add the Weibull curve
lines(x, y_weibull, col = "#0033FF")

# Add the Frechet curve
lines(x, y_frechet, col = '#FF6600')

legend("topright", legend = c("Gumbel", "Weibull", "Frechet"), 
       col = c("#CC00FF", "#0033FF", "#FF6600"), lty = 1, cex = 1.5 )
####################################################################################
# Create a sequence of x values
x <- seq(-5, 5, length.out = 100)

# Define the scale parameter
scale <- 1

shape_values = c(-0.5,0,0.5)
par(mar = c(5,5,5,2))
# Initialize a plot
plot(x, dgev(x, scale = scale, shape = -0.5), 
     type = "l", col = "#CC00FF", ylim = c(0, 0.8), 
     ylab = "Density", xlab = "x", 
     main = "GEV density functions for different shape parameters",
     ,cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7,)

# Add lines for other shape parameter values
  lines(x, dgev(x, scale = 1, shape = 0), col = "#0033FF")
  lines(x, dgev(x, scale = 1, shape = 0.5), col = "#FF6600")

# Add legend
legend("topright", legend = paste("Shape =", shape_values), 
       col = c("#CC00FF", "#0033FF","#FF6600"), lty = 1, cex = 1.5)
####################################################################################

# Create a sequence of x values
x <- seq(-5, 5, length.out = 100)

# Define the scale parameter
scale <- 1

# Define the shape parameter values
shape_values <- c(1, 5, 20)

par(mar = c(5, 5, 5, 2))  # Set plot margins

# Initialize a plot
plot(x, dgpd(x, scale = scale, shape = 1),
     type = "l", col = "#CC00FF", ylim = c(0, 0.9), xlim = c(-.1,5),
     ylab = "Density", xlab = "x", 
     main = "GP density functions for different shape parameters",
     cex.main = 2.2, cex.lab = 1.5, cex.axis = 1.7)

# Add lines for other shape parameter values
lines(x, dgpd(x, scale = scale, shape = 5), col = "#0033FF")
lines(x, dgpd(x, scale = scale, shape = 20), col = "#FF6600")

# Add legend
legend("topright", legend = paste("Shape =", shape_values), 
       col = c("#CC00FF", "#0033FF", "#FF6600"), lty = 1, cex = 1.5)

#################################################################################3
#H3
counts = table(categ)
par(mar = c(5, 10, 4, 2) + 0.1)
barplot(counts, las = 2, col = "blue", main = "Histogram of solar flare occurrences", 
        xlab = "Classes", ylab = "", ylim = c(0, max(counts) + 10000),cex.main = 2.2, cex.names = 1.9, cex.lab = 2,cex.axis = 1.8,)

text(x = seq_along(counts), y = counts, label = counts, pos = 3, cex = 1.2, col = "black")
mtext("Frequency", side = 2, line = 7, at = max(counts)/2, las = 0, cex = 1.5)


