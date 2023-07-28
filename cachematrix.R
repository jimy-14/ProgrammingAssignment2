## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- NULL  #object with null value for inverse matrix
  set <- function(aMatrix) #set function with aMatrix argument
    { 
  matrix1 <<- aMatrix # assigning aMatrix argument value to matrix1 defined in parent enviornment
  inverse_matrix <<- NULL #for new value of aMatrix assign NULL to inverse_matrix
  }
  get <- function() matrix1 #defining the getter for matrix matrix1
  
  setInverse <- function(inverse_of_matrix) inverse_matrix <<- inverse_of_matrix # setter for the inverse inverse_matrix
  
  getInverse <- function() inverse_matrix # getting inverse matrix 
  
  list(set = set, get = get, #list of functions being returned by the makeCacheMatrix()
       setInverse = setInverse,
       getInverse = getInverse)
  
  
}
  


## matrix solving the inverse of the makeCacheMatrix() object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_mat <- makeCacheMatrix_object$getInverse() #calling getInverse()
  if(!is.null(inv_mat)) { # if inverse exist, return it
    message("getting cached data")
    return(inv_mat)
  }
  data <- makeCacheMatrix_object$get() #if inverse does not exist ,get matrix
  matrix_inverse_object <- solve(data, ...) #find inverse of matrix
  makeCacheMatrix_object$setInverse(matrix_inverse_object ) #set inverse of matrix
  matrix_inverse_object  #return matrix
}
