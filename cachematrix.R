## The functions below make use of caching the inverse of a matrix
## rather than getting R to compute it every time, which would be both time- and resourse consuming.

## The first function creates a special object that caches the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## The inverse of the special matrix object from the previous function is created by this function. 
## If the matrix has not changed and the inverse already exists then it is retreaved from cache.

cacheSolve <- function(x, ...) {
  
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
  
  
}
