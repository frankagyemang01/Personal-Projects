# Define radius
r <- 2

# Initialize vector to store volumes
V <- numeric(20)

# Compute the volume for p = 1, ..., 20
for (p in 1:20) {
  V[p] <- (pi^(p/2) * r^p) / gamma(p/2 + 1)
}

# Create a plot
plot(1:20, V, type = "b", pch = 19, col = "blue",
     xlab = "Dimension (p)", ylab = "Volume (V_p)",
     main = "Volume of a p-dimensional Sphere with Radius = 2")

# Add a grid for clarity
grid()

