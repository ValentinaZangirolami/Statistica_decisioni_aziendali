# Introduction to R
# Valentina Zangirolami

# R resources
# To install R: https://cran.r-project.org/
# To install R studio (R is required): https://posit.co/downloads/
# Documentation: https://cran.r-project.org/manuals.html
# Cheat sheets: https://support-rstudio-com.netlify.app/resources/cheatsheets/

# Basic commands - Help
help.start()  # shows the main links for programming in R
help(mean)    # or ?mean shows the description of the function "mean"
help.search("mean")  # to search a string on function's documentation

# Symbols for maths operation
2 + 3   # addition
5 - 2   # difference
3 * 4   # product with numbers
10 / 2  # division with numbers
10 %/% 3 # integer division
2 ** 3  # exponentiation
10 %% 3 # division remainder

# Logic operators
5 != 3  # not equal
(5 > 3) & (2 < 4) # AND
(5 > 3) | (2 > 4) # OR

# Data Types
numeric_var <- 2.5    # Numeric
integer_var <- 18L    # Integer
logical_var <- TRUE   # Logical
complex_var <- 2i     # Complex
character_var <- "Cherry" # Character
raw_var <- charToRaw("hello") # Raw

# Special data types
null_object <- NULL   # empty object
missing <- NA         # missing value

# Variables and Basic Arithmetic
sum_n <- 2 + 2.5      # Sum of two numbers
cat("The sum of the two numbers is", sum_n)

# Division between two numbers with scientific notation
x <- 1
y <- 2.73e4
print(x/y)

# Math functions
z <- exp(3)           # Exp function
y <- log(1)           # Log function
print(z - y)
pi                    # Pi greco
sin(0); cos(pi)       # Trigonometric functions
log(x=4, base=2)      # Log base 2

# It's your turn! - Solutions

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

# Vectors
x <- c(23.4, 6, 7.1, 6.9, 34.7) # Create a vector
length(x)       # Length of the vector
x[1]           # First element
x[c(2,3)]      # Second and third elements
x[c(1:3)]      # First three elements

# Sequences
n <- 10
seq(1, n)              # Sequence from 1 to 10
seq(1, n, by = 2)      # Sequence with step 2
seq(1, n, length.out = 5) # Sequence with 5 elements
rep(n, times=5)        # Repeat n 5 times
rep(1:4, each=5)       # Repeat each element 5 times
rep(1:4, times=5)      # Repeat the sequence 5 times

# Math operations on vectors
x <- c(x, -6.8)        # Add a new value
sort(x)                # Sort the vector
floor(x)               # Integer part
sign(x)                # Sign of elements
any(x > 4)             # Check if any element > 4

# Useful functions
abs(x)                 # Absolute value
sum(x)                 # Sum
mean(x)                # Mean
sd(x)                  # Standard deviation
median(x)              # Median
prod(x)                # Product
cumsum(x)              # Cumulative sum
min(x); max(x)         # Minimum and maximum
range(x)               # Range
head(x); tail(x)       # Head and tail
summary(x)             # Summary statistics

# Vectors of characters
days_list <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
factor(c("Monday", "Sunday"), levels = days_list)

# Frequency table
table(c("A", "B", "B"))

# Matrix
matrix(1:10, nrow = 2, ncol = 5)
matrix(letters[1:10], nrow = 2, ncol = 5, byrow = TRUE)

# Matrix operations
x <- matrix(1:4, nrow=2)
y <- matrix(c(5,10,33,2), ncol=2)
x %*% y               # Matrix multiplication
t(x)                 # Transpose
det(x)               # Determinant
solve(x)             # Inverse
crossprod(x,y)       # t(x) %*% y
solve(x,x)           # solve(x) %*% x

# Matrix binding
cbind(x,y)           # Column binding
rbind(x,y)           # Row binding

# It's your turn! - Solutions

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

# Lists
my_list <- list(FALSE, 0L, pi, "ABC")
x <- list(1, "A", matrix(1:4, 2, 2))
x[2]        # Extract as list
x[[2]]      # Extract element
unlist(x)   # Convert to vector

# Probability functions
rnorm(n=5, mean=0, sd=1)  # Random normal numbers
pnorm(0, mean=0, sd=1)    # P(X <= 0) for X ~ N(0,1)

# R Exercises
# 11. Calculate the 0.7 quantile of X ~ N(5, 5)
qnorm(0.7, mean=5, sd=sqrt(5))

# 12. Simulate Gamma(1, 2) and show summary
set.seed(123)
data <- rgamma(500, shape=1, rate=2)
summary(data)

# Control structures
# If/Else
x <- 5
if (x > 3) {
  print("x is greater than 3")
} else {
  print("x is not greater than 3")
}

# For loop
set.seed(123)
x <- rpois(10, lambda=10)
x <- sort(x)
even_x <- c()
odd_x <- c()
for (i in seq_along(x)) {
  if(x[i] %% 2 == 0){
    even_x <- c(even_x, x[i])
  } else {
    odd_x <- c(odd_x, x[i])
  }
}

# Functions
fahrenheit_to_celsius <- function(temp_F) {
  (temp_F - 32) * 5 / 9
}
fahrenheit_to_celsius(55)

# It's your turn! - Solutions

# 13. Function for vector statistics
vector_stats <- function(vec) {
  list(
    mean = mean(vec),
    variance = var(vec),
    minimum = min(vec),
    maximum = max(vec)
  )
}

# 14. Function for number sequence
num_seq <- function(n) {
  sapply(2:(n+1), function(i) i^2 * (2*i - 1))
}

# 15. Skewness function
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

# Dataframe
x <- data.frame(
  hair_color = c("Blonde", "Blonde", "Black", "Brown"),
  gender = c("F", "M", "M", "F")
)

# Apply functions
salaries <- c(80000, 62000, 113000, 68000, 75000, 79000, 112000, 118000, 65000, 117000)
jobs <- factor(c('DS', 'DA', 'DE', 'DA', 'DS', 'DS', 'DE', 'DE', 'DA', 'DE'))
tapply(salaries, jobs, mean)

# Example of sapply
add_one <- function(x) x + 1
my_vector <- c(1, 2, 3)
sapply(my_vector, add_one)