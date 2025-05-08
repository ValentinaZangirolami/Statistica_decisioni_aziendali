################################################################################
#                  Solution of exercises - Introduction to R                   #
#                                  12/05/2025                                  #
################################################################################
#                           Valentina Zangirolami                              #
################################################################################

#EXERCISE 1

#Given a circle with a radius equal to 4. Calculate perimeter and area of the circle using R.

r <- 4
perimeter <- 2*pi*r
area <- pi*r^2

cat("The perimeter and the area of the circle is equal to", perimeter, "and", area)

#EXERCISE 2

#Install the library dplyr and show the datasets available.

#install.packages("dplyr")
library(dplyr)
data()

#EXERCISE 3

#Try to read the help page of factorial and choose.

?factorial
?choose

#same as
help(choose)

#EXERCISE 4

#In how many ways can you arrange 7 out of 20 books on a bookshelf? Show the result in R. 

choose(20, 7)

#EXERCISE 5

#Round the number n=3.78957 to two decimal places.

n <- 3.78957 
round(n, 2)

#EXERCISE 6

#Visualize all objects saved in R space and then remove them.

ls()
rm(list=ls())

#EXERCISE 7

#Calculate absolute and relative frequencies of vector ("Cat","Dog",  "Bird", "Bird", "Cat","Cat","Dog"). 
#Print the maximum absolute frequency and the correspondent animal.

animal <- c("Cat","Dog", "Bird", "Bird","Cat","Cat","Dog")

#absolute frequencies
freq_abs <- table(animal)
freq_abs

#relative frequencies
freq_rel <- table(animal)/length(animal)
freq_rel

cat("The animal with maximum frequency is:",names(freq_abs)[which(freq_abs==max(freq_abs))], "and the frequency is", max(freq_abs))

rm(list=ls())

#EXERCISE 8

#Define a vector called z by "pasting" the vectors x=("Red", "Green", "Yellow") and y=("Tomato", "Grass", "Sun") 
#and select only those elements of z that begin with "r" and end in "o".

x <- c("Red", "Green", "Yellow") 
y <- c("Tomato", "Grass", "Sun")

z <- paste(x,y)

selected_word <- startsWith(z, 'R') & endsWith(z, 'o')
print(z[selected_word])

rm(list=ls())

#EXERCISE 9

#Create a matrix A containing the integers from 10 to 24 having 5 rows and 3 columns 
#where A must be filled by column. Show the firsts 3 rows of the firsts two columns. 

A <- matrix(10:24, nrow=5, ncol=3)

#Calculate the sum of A by row and column (Hint: rowSums(), colSums()). 

sum_by_row <- rowSums(A)
sum_by_row
sum_by_col <- colSums(A)
sum_by_col

#Calculate the transpose of A. 

A_transpose <- t(A)
A_transpose

#Define a new matrix B of appropriate size to calculate the AB product (and calculate that product). 

B <- matrix(1:6, nrow=3, ncol=2)
B

### product

A %*% B

#Calculate the standard deviation of B by row and then by column (Hint: apply())

B_row <- apply(B, 1, sd)
B_row
B_col <- apply(B, 2, sd)
B_col

rm(list=ls())

#EXERCISE 10

#Given a matrix X 2x2 containing the integers from 1 to 4. 
#Try to apply svd() function to X and then reconstruct the input matrix X with the outputs of SVD.

X <- matrix(1:4, nrow=2)
X

#SVD
x_svd <- svd(X)

# Input matrix
D <- diag(x_svd$d)
X_input <- x_svd$u %*% D %*% t(x_svd$v)

X_input

rm(list=ls())

#EXERCISE 11

#Let X ~ N(5,5). Calculate the 0.7 quantile of X.

a <-qnorm(0.7, 5, 5)

pnorm(a, 5, 5)

#EXERCISE 12

#Let Y ~ Gamma(1,2). Simulate the extraction of n=500 random numbers from Y and 
#save them into a object called "data". Show the summary statistics about it. 
#(To guarantee the reproducibility you need to use set.seed())

n <- 500
set.seed(123) # reproducibility

data <- rgamma(n, shape=1, scale=2)

summary(data)

# EXERCISE 13

#Try to define a function f which, given a numerical vector as input, returns a list containing the mean, 
#variance, minimum and maximum of that vector. Assign an appropriate name to the elements of the list.

my_vec <- c(5, 20, 33, 1)
fun_stat <- function(x){ list(min(x), var(x), max(x), mean(x))}

fun_stat(my_vec)

# EXERCISE 14

#Write an R function, num\_seq(n), that returns a vector that has the form {2^2 x 3,3^2 x5,4^2 x7,5^2x9, ...}$
#and has exactly n terms.

n <- 20

num_seq = function(n) {
  squares = (2:(n+1))^2
  odds = 2*(1:n)+1
  return(squares * odds)
}

num_seq(n)

# EXERCISE 15

#Assume that a data set is stored in the vector data in R. 
#Write a function skewness(data) that calculates the skewness index of the data set 
#and returns either "The data is significantly skewed" or "The data is not significantly skewed" 
#as appropriate.

x <- c(2, 34, 7, 15, 67, 82, 13, 24, 88, 99, 70, 69, 77)
y <- rnorm(20) #we know in advance that's a symmetric distribution


skewness=function(data) {
  index=3*(mean(data)-median(data))/sd(data)
  return(ifelse(abs(index)>1,"The data is significantly skewed",
                "The data is not significantly skewed"))
}

#first vector x
skewness(x) #skewed

#vector y
skewness(y) # not skewed
