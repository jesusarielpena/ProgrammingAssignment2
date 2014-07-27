## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix--> return some functions in order to set
#Set and get the value of the matrix
#Set and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  # inv to save the cached inverse matrix
  inv <- NULL
  
  # set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # get the value of the matrix
  get <- function() x
  
  # set the value of the inverse
  setinv <- function(inverse) inv <<- inverse
  # get the value of the inverse
  getinv <- function() inv
  
  # Return the matrix with the defined functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
# use cacheSolve to get the inverse of the matrix.If it is already calculated returns
# returns the cached inverse.
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  # If the inverse is already calculated, return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # The inverse is not yet calculated calculate it
  data <- x$get()
  inv <- solve(data, ...)
  
  # Cache the inverse
  x$setinv(inv)
  
  # Return it
  inv
}

# Example  calculate a matrix 2x2 with four random numbers, invert the matrix and save the Cache, the second
#time the cachesolve will give the cached data "getting cached data"
x <- matrix(rnorm(4), nrow = 2)          
cx <- makeCacheMatrix(x)                 
cx$get()                                  
cacheSolve(cx)                           
cacheSolve(cx)   
