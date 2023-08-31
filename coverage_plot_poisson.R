layout(matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), ncol = 3))
margin <- c(5, 4, 4, 2)  # Adjust margins

# Define x values
x <- c(-log(9), -log(7), -log(5), -log(3), 0, log(3), log(5), log(7), log(9))

# Define the data for different scenarios
# Define the data for different scenarios
cov_data <- list(
     cov.200.k7 = c(0.954,0.96,0.965,0.969,0.96,0.948,0.932,0.927,0.92 ) * 100,
     cov.400.k7 = c(0.961,0.954,0.967,0.964,0.95,0.914,0.917,0.901,0.908 ) * 100,
     cov.800.k7=c(0.956,0.97,0.96,0.967,0.958,0.927,0.923,0.909,0.922 )*100,

     cov.200.k5=c(0.959,0.943,0.955,0.967,0.965,0.955,0.933,0.918,0.906 )*100,
     cov.400.k5<-c(0.955,0.96,0.966,0.963,0.958,0.927,0.928,0.935,0.89 )*100,
     cov.800.k5<-c(0.956,0.965,0.961,0.976,0.942,0.931,0.91,0.923,0.902 )*100,

     cov.200.k3<-c(0.961,0.954,0.957,0.967,0.962,0.948,0.936,0.93,0.912)*100,
     cov.400.k3<-c(0.947,0.956,0.955,0.966,0.971,0.953,0.938,0.901,0.904 )*100,
     cov.800.k3<-c(0.951,0.954,0.959,0.962,0.957,0.955,0.929,0.913,0.895)*100
)

cov_firth_data <- list(
     cov.200.firth.k7=c(0.862,0.896,0.908,0.924,0.941,0.944,0.935,0.909,0.904 )* 100,
     cov.400.firth.k7=c(0.901,0.915,0.928,0.933,0.957,0.938,0.924,0.905,0.911 ) * 100,
     cov.800.firth.k7=c(0.933,0.936,0.933,0.945,0.948,0.937,0.922,0.91,0.902  )*100,

     cov.200.firth.k5=c(0.955,0.96,0.968,0.965,0.967,0.936,0.942,0.923,0.91)*100,
     cov.400.firth.k5<-c(0.906,0.914,0.931,0.932,0.95,0.945,0.91,0.943,0.916 )*100,
     cov.800.firth.k5<-c(0.925,0.919,0.944,0.934,0.939,0.941,0.923,0.911,0.909 )*100,

     cov.200.firth.k3<-c(0.96,0.959,0.953,0.963,0.966,0.947,0.929,0.923,0.897 )*100,
     cov.400.firth.k3<-c(0.96,0.954,0.964,0.972,0.961,0.938,0.915,0.909,0.913  )*100,
     cov.800.firth.k3<-c(0.962,0.965,0.962,0.963,0.949,0.93,0.908,0.908,0.9 )*100
)

cov_boots_data <- list(
     cov.200.boots.k7=c(0.82,0.84,0.87,0.912,0.925,0.93,0.934,0.93,0.932 )* 100,
     cov.400.boots.k7=c(0.84,0.875,0.88,0.91,0.925,0.92,0.918,0.921,0.928 ) * 100,
     cov.800.boots.k7=c(0.87,0.903,0.91,0.922,0.93,0.928,0.93,0.931,0.93  )*100,
     
     cov.200.boots.k5=c(0.835,0.857,0.876,0.899,0.914,0.922,0.923,0.924,0.924)*100,
     cov.400.boots.k5<-c(0.879,0.89,0.906,0.911,0.915,0.917,0.925,0.928,0.931 )*100,
     cov.800.boots.k5<-c(0.905,0.913,0.918,0.924,0.93,0.931,0.937,0.935,0.936 )*100,
     
     cov.200.boots.k3<-c(0.842,0.863,0.891,0.903,0.91,0.914,0.918,0.92,0.923 )*100,
     cov.400.boots.k3<-c(0.887,0.907,0.917,0.923,0.927,0.926,0.934,0.937,0.941  )*100,
     cov.800.boots.k3<-c(0.908,0.913,0.92,0.923,0.926,0.931,0.935,0.938,0.94 )*100
     
)



# Define main titles for each scenario
main_titles <- c(
     "Poisson regression model, k=7", "Poisson regression model, k=7",
     "Poisson regression model, k=7",
     "Poisson regression model, k=5", "Poisson regression model, k=5",
     "Poisson regression model, k=5",
     "Poisson regression model, k=3", "Poisson regression model, k=3",
     "Poisson regression model, k=3"
)

# Set custom colors
ml_color<-"#E67E22"
firth_color<-"#2ECC71"
boots_color<-"#3498DB"


# Set the layout for a 3x3 grid
par(mfrow = c(3, 3))

# Custom x-axis labels
custom_labels <- c(-log(9), -log(7), -log(5), -log(3), 0, log(3), log(5), log(7), log(9))
# Define n values
n_values <- c(200, 400, 800,200, 400, 800,200, 400, 800)
# Loop through the scenarios
for (i in 1:9) {
     # Create a new plot
     plot(x, cov_data[[i]], xaxt = 'n',type = "b", pch = 18,
          ylab = expression(paste("Coverage (", beta, ")")), xlab = "True beta", ylim = c(80, 100),
          main = main_titles[i])

     # Add lines
     lines(x, cov_firth_data[[i]], type = "b", col = firth_color, pch = 18)
     lines(x, cov_data[[i]], type = "b", col = ml_color, pch = 18)
     lines(x, cov_boots_data[[i]], type = "b", col = boots_color, pch = 18)
     abline(h = 95, lty = 3, col = "black")
     abline(h = 92.5, lty = 3, col = "black")
     abline(h = 97.5, lty = 3, col = "black")


     # Add x-axis labels without ticks or labels
     axis(side = 1, at = custom_labels,  labels = round(custom_labels,2), tcl = 0.2)
     # Add n value in the bottom left corner
     mtext(paste("n=", n_values[i]), side = 1, at = -log(5), line = -1, cex = 0.8)
     # Adjust the margin
     par(mar = margin)
}

# Reset the layout to default
par(mfrow = c(1, 1))

# Create the common legend below the 3x3 grid
legend("bottom", legend = c("ML-Wald", "Firth-wald", "Firth-Bootstrap"),
       col = c(ml_color, firth_color, boots_color), lty = c(1, 1, 1), cex = 0.7, horiz = TRUE, bty = "n")
