## These functions allow a user to create an extended matrix object that is capable of
## caching the inverse operation

## this function returns a matrix wrappered as an object with extension methods.  

makeCacheMatrix <- function(x = matrix()) {
  cachedMatrixInverse <- NULL
  set <- function(newMatrix) {
      x <<- newMatrix
      matrix <- NULL
  }
  get <- function() x
  setInverse <- function(matrixInverse) cachedMatrixInverse <<- matrixInverse 
  getInverse <- function() cachedMatrixInverse 
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function determines if an inverse exixts and if it does, returns it.  
## otherwise, it solves the inverse, caches it and returns it

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
      message("getting cached data")
      return(inverse)
    }
    inverse <- solve(x$get())
    x$setInverse(inverse)
    inverse
}
