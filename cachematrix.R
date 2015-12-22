# testable at command prompt as
# a <- makeCacheMatrix() 
# a$set(matrix(c(2,4,3,1,5,7,9,10,11),nrow=3,ncol=3))  
# cacheSolve(a) ==> This is not from cache
# cacheSolve(a) ==> This returns from cache

makeCacheMatrix <- function(x = matrix()) {
  # initialize to NULL
  cache <- NULL
  
  # create the matrix in the working environment
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  
  # get the value of the matrix
  get <- function() x
  # invert the matrix and store in cache
  setMatrix <- function(inverse) cache <<- inverse
  # get the inverted matrix from cache
  getInverse <- function() cache
  
  # return the created functions to the working environment
  list(set = set, get = get,
       setMatrix = setMatrix,
       getInverse = getInverse)
}

## CacheSolve function

cacheSolve <- function(x, ...) {
  ## attempt to get the inverse of the matrix stored in cache
  cache <- x$getInverse()
  
  # return inverted matrix from cache if it exists
  # else create the matrix in working environment
  if (!is.null(cache)) {
    message("getting cached data")
    
    # display matrix in console
    return(cache)
  }
  
  # create matrix since it does not exist
  matrix <- x$get()
  
  # matrix needs to be square
  
  tryCatch( {
    # set and return inverse of matrix
    cache <- solve(matrix, ...)
  },
  error = function(e) {
    message("Runtime Error:")
    message(e)
    
    return(NA)
  },
  warning = function(e) {
    message("Run Time Warning:")
    message(e)
    
    return(NA)
  },
  finally = {
    # set inverted matrix in cache
    x$setMatrix(cache)
  } )
  
  # display matrix in console
  return (cache)
}