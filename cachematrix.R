## This program determine the inverse matrix from an input matrix that is assumed to be a
## square and invertible matrix.

## The makeCacheMatrix function is used initialize the inverse matrix and define the functions
## that are used to compute the inverse matrix. 
makeCacheMatrix <- function(input_matrix = matrix()) 
{

     ## First let's initialize a NA matrix in the local environment.
     inverse_matrix <- matrix()
     
     ## This function is used to assign a new matrix to the inverse matrix
     ## that was initialized in the local environment and to update its values.
     function_set <- function(new_matrix) 
     {
          input_matrix <<- new_matrix
          inverse_matrix <<- matrix()
     }
     
     ## This function returns input_matrix that will be used compute the inverse matrix.
     function_get <- function() 
     {
          input_matrix
     }
     
     ## This function computes the inverse matrix using the solve function.
     function_setinverse <- function(solve) 
     {
          inverse_matrix <<- solve
     }
     
     ## This function returns the inverse matrix.
     function_getinverse <- function() 
     {
          inverse_matrix
     }
     
     ## The function list.
     list(set = function_set, get = function_get,
          setinverse = function_setinverse,
          getinverse = function_getinverse)
}

## cacheSolve provides the previously created inverse_matrix to this function.
## This function checks if the inverse matrix has been previously computed.
## If not, cacheSolve computes the inverse matrix and returns it.
cacheSolve <- function(input_matrix, ...) 
{
     
     ## Get the (potentially cached) inverse matrix from the created inverse_matrix defined in the 
     ## makeCacheMatrix function. The inverse_matrix object is not the same as the one defined in makeCacheMatrix. 
     inverse_matrix <- input_matrix$getinverse()
     
     ## Check if the inverse matrix has already been computed by using the complete.cases to check for
     ## NA values. If NA values exist, then the inverse has not been computed. To avoid length>1 warning
     ## messages from logical array created in the complete.cases function, I use the first value from 
     ## the complete.cases array to test if the inverse matrix has been computed.
     if(complete.cases(inverse_matrix)[1]) 
     {
          message("getting cached inverse matrix")
          ## Return the cached inverse matrix.
          return(inverse_matrix)
     }
     
     ## If the inverse matrix has not been previously computed, then we need to compute it here.
     ## Copy the input matrix to a new matrix called data. This new matrix will be used to
     ## calculate the inverse.
     data <- input_matrix$get()
     ## Calculate the inverse matrix and store it in the inverse_matrix object.
     inverse_matrix <- solve(data, ...)
     ## Using the setinverse function, store the inverse matrix in the inverse_matrix 
     ## created in the makeCacheMatrix function.
     input_matrix$setinverse(inverse_matrix)
     ## Return the inverse_matrix.
     inverse_matrix
}