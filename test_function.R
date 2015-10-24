inverse_matrix <- function(size) {
     x <- matrix()
     length(x) <- size^2
     dim(x) <- c(size,size)
     x
}