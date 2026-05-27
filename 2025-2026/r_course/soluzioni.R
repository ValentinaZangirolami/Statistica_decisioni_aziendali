# 1. Calculate perimeter and area of a circle with radius 4
radius <- 4
perimeter <- 2 * pi * radius
area <- pi * radius ** 2
cat("Perimeter:", perimeter, "Area:", area)

# 2. Install the library dplyr and show the datasets available
install.packages("dplyr")
library(dplyr)
data(package='dplyr')

# 3. Read the help page of factorial and choose
?factorial
?choose

# 4. Number of ways to arrange 7 out of 20 books
choose(20, 7) * factorial(7)

# 5. Round the number n = 3.78957 to two decimal places
n <- 3.78957
round(n, 2)

# 6. Visualize all objects saved in R space and then remove them
ls()
rm(list=ls())

# 7. Calculate absolute and relative frequencies
animals <- c("Cat", "Dog", "Bird", "Bird", "Cat", "Cat", "Dog")
freq <- table(animals)
rel_freq <- prop.table(freq)
max_freq <- max(freq)
max_animal <- names(which(freq == max_freq))
cat("Maximum frequency:", max_freq, "Animal:", max_animal)

# 8. Define vector z and select elements
x <- c("Red", "Green", "Yellow")
y <- c("Tomato", "Grass", "Sun")
z <- paste(x, y)
selected <- z[startsWith(z, "R") & endsWith(z, "o")]

# 9. Matrix operations
A <- matrix(10:24, nrow=5, ncol=3, byrow=FALSE)
A_subset <- A[1:3, 1:2]
row_sums <- rowSums(A)
col_sums <- colSums(A)
A_transpose <- t(A)
B <- matrix(1:6, nrow=3, ncol=2)
AB_product <- A %*% B
sd_rows <- apply(B, 1, sd)
sd_cols <- apply(B, 2, sd)

# 10. Function for vector statistics
vector_stats <- function(vec) {
  list(
    mean = mean(vec),
    variance = var(vec),
    minimum = min(vec),
    maximum = max(vec)
  )
}


# 11. Skewness function
skewness <- function(data) {
  x_bar <- mean(data)
  q2 <- median(data)
  s <- sd(data)
  I <- 3 * (x_bar - q2) / s
  if (abs(I) > 0.1) {
    return("The data is significantly skewed")
  } else {
    return("The data is not significantly skewed")
  }
}