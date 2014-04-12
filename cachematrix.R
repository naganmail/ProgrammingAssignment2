## This set of functions solves quadratic matrices using simple caching

## makeCacheMatrix - constructs cached matrix object
##                  'x' is a quadratic matrix
makeCacheMatrix <- function(x = matrix()) {
  # initialize an empty cache
  inverse <- NULL
  
  # define data access functions (getters and setters)
    # updates the stored matrix and resets the cache
    set <- function(y) {
      x <<- y
      inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse

  # make function names available for execution
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by 
##             makeCacheMatrix above. If the inverse has already been calculated 
##             (and the matrix has not changed), then the cachesolve retrieves 
##             the inverse from the cache.
##            'x' cached matrix object
cacheSolve <- function(x, ...) {
  # get current inverse matrix value
  inverse <- x$getinverse()
  
  # once the inverse matrix is in cache just return its content
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  # if cache is empty get the matrix data, solve it for inverse, store in cache 
  # and return the result 
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
