## AGRO 896
sim_data <- read.csv("AGRO896.CSV")
sim_data$Unit <- NULL
# Number of samples
n_samples <- 10
sample_size <- 20

# Store results
results <- data.frame(
  Sample = integer(),
  White = integer(),
  Black = integer()
)

# Loop over samples
for (i in 1:n_samples) {
  
  # Draw a random sample (without replacement)
  samp <- sample(sim_data$Color, size = sample_size, replace = FALSE)
  
  # Count frequencies
  counts <- table(samp)
  
  # Ensure both colors are recorded (even if one is missing)
  white_count <- ifelse("White" %in% names(counts), counts["White"], 0)
  black_count <- ifelse("Black" %in% names(counts), counts["Black"], 0)
  
  # Store results
  results <- rbind(results, data.frame(
    Sample = i,
    White = white_count,
    Black = black_count
  ))
}

# View results
print(results)

## Getting observed frequencies at each sample

# Add proportion columns
results$White_prop <- results$White / (results$White + results$Black)
results$Black_prop <- results$Black / (results$White + results$Black)

# View updated results
print(results)

## Creating a new column for Expected frequency
results$Exp_freq <- 
  

