
# My functions create a special type of matrix that through the appropriate use 
# of lexical scoping and the creation and application of 4 functions can save valuable 
# computational time by not calculating again unnecessarily the inverse of a matrix that 
# was already calculated.


# This function allows one to set a matrix whose inverse can be calculated and called 
# in an efficient way. More specifically, it creates four functions that are used to
# set and call a matrix, as well as to set and call the inverse of this matrix. 

makeCacheMatrix <- function(x = matrix()) {
  
    inv <- NULL
    setmatrix <- function(z) {
      x <<- z
      inv <<- NULL
    }
    getmatrix <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(setmatrix = setmatrix, getmatrix = getmatrix,
         setinverse = setinverse,
         getinverse = getinverse)
  
}


# This function simply calls one of the functions defined in the special matrix 
# to get the inverse of the matrix. Then, it will ouput this value, if it exists. Otherwise,
# it will calculate the inverse of the matrix with the solve function after it uses 
# another function of the special matrix (x$getmatrix). 

cacheSolve <- function(x, ...) {
        
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$getmatrix()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
  
}
