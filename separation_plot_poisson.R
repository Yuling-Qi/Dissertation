library(ggplot2)
library(gridExtra)

# Function to format y-axis labels
formatYLabels <- function(x) {
     sprintf("%.1f", x)
}

# Create a custom plot function
custom_plot_ggplot <- function(x, y, title) {
     p <- ggplot() +
          geom_line(aes(x, y), color = "black", size = 1) +
          geom_point(aes(x, y), color = "black", shape = 18, size = 3) +
          labs(x = "True beta", y = expression(paste("Separation % (", beta, ")"))) +
          scale_x_continuous(labels = scales::number_format(accuracy = 0.01)) +
          scale_y_continuous(limits = c(0, 1),
                             labels = formatYLabels) +
          theme_minimal() +
          theme(legend.position = "top", legend.key.width = unit(0.5, "cm")) +
          ggtitle(title)
     return(p)
}

# The number of Separation
x <- c(-log(9), -log(7), -log(5), -log(3), 0, log(3), log(5), log(7), log(9))

# n=200, k=7
sep.200.k7 <- c(711,661,581,372,67,0,0,0,0) / 1000

# n=400, k=7
sep.400.k7 <- c(503,445,323,159,3,0,0,0,0 ) / 1000

# n=800, k=7
sep.800.k7 <- c(279,183,94,22,0,0,0,0,0 ) / 1000



# n=200, k=5
sep.200.k5 <- c(718,693,553,398,64,3,0,0,0 ) / 1000

# n=400, k=5
sep.400.k5 <- c(528,444,311,162,6,0,0,0,0) / 1000

# n=800, k=5
sep.800.k5 <- c(259,188,103,21,0,0,0,0,0 ) / 1000


# n=200, k=3
sep.200.k3 <- c(715,649,552,370,64,0,1,0,0 ) / 1000

# n=400, k=3
sep.400.k3 <- c(504,421,277,135,4,0,0,0,0) / 1000

# n=800, k=3
sep.800.k3 <- c(265,186,88,19,0,0,0,0,0 ) / 1000



# Create the individual plots using custom_plot_ggplot
plot_1 <- custom_plot_ggplot(x, sep.200.k7, "Poisson, k=7, n=200")
plot_2 <- custom_plot_ggplot(x, sep.400.k7, "Poisson, k=7, n=400")
plot_3 <- custom_plot_ggplot(x, sep.800.k7, "Poisson, k=7, n=800")

plot_4 <- custom_plot_ggplot(x, sep.200.k5, "Poisson, k=5, n=200")
plot_5 <- custom_plot_ggplot(x, sep.400.k5, "Poisson, k=5, n=400")
plot_6 <- custom_plot_ggplot(x, sep.800.k5, "Poisson, k=5, n=800")

plot_7 <- custom_plot_ggplot(x, sep.200.k3, "Poisson, k=3, n=200")
plot_8 <- custom_plot_ggplot(x, sep.400.k3, "Poisson, k=3, n=400")
plot_9 <- custom_plot_ggplot(x, sep.800.k3, "Poisson, k=3, n=800")

# Arrange the plots using grid.arrange
grid_arrange <- grid.arrange(plot_1, plot_2, plot_3, plot_4, plot_5, plot_6,
                             plot_7, plot_8, plot_9, ncol = 3)

# Display the combined plot
print(grid_arrange)
