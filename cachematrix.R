
# My functions create a special type of matrix that through the appropriate use 
# of lexical scoping can save valuable computational time by not calculating unnecessarily 
# the inverse of a matrix that was already calculated. 


# This function receives an argument to be converted into a matrix. This matrix is then going 
# to be assigned to a variable inside a subsequent function and through the operator <<- 
# is identifiable in the environment of the makeCacheMatrix function. After this, this function
# creates a number of subsequent functions that allow the call of the given matrix and the input 
# and call of the inverse of a matrix. 

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
# to get the inverse of a matrix. Then, it will ouput this value, if it exists. Otherwise,
# it will calculate the inverse of the matrix

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
