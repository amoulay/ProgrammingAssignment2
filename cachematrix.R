## A pair of functions that cache the inverse of a matrix rather than compute it repeatedly

## The following function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize the inverse property
  i <- NULL
  
  ## Sets the value of the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## Gets the value of the matrix
  get <- function() x    
  
  ## Sets the value of the inverse of the matrix
  setinverse <- function(inverse) i <<- inverse
  
  ## Gets the value of the inverse of the matrix
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Compute and cache the inverse of a matrix. Rather than computing the inverse for the same matrix, 
## the function retrieves it from the cache.
cacheSolve <- function(x, ...) {
  
  ## Returns a matrix that is the inverse of 'x'
  i <- x$getinverse()
  
  ## Retrieves the inverse if it's already computed
  if (!is.null(i)) {
    message("retrieving the inverse from cache")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

