## This function creates a special "matrix" object that can cache its inverse.
## The function returns a list that stores get, set, getinverse, setinverse functions 

makeCacheMatrix <- function(x = matrix()) {
  # variable to store inverse value
  inverse <- NULL 
  
  # get and set functions for matrix
  set <- function(y)
  {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  
  # get and set functions for inverse matrix
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  
  # setting output of the function
  list(set=set, get=get, 
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  # First step is to try to get the inverse matrix from cache.
  # If it is not null, we happily ask to return the value of the cached inverse matrix as an output.

  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  # In case there's nothing to return from previous statement, it calculates the inverse
  
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
  
}
