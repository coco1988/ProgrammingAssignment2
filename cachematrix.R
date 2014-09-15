## Matrix inversion is usually a costly computation and their may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## These two functions are used to create a special object that stores a numeric
## matrix and cache's its inverse.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  # initialize the stored inverse value to NULL
  m <- NULL
  
  # set value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL # resetting matrix to NULL
  }
  
  # get value of matrix
  get <- function() x
  
  # set inverse of matrix by setMatrix
  setMatrix <- function(cacheSolve) m <<- cacheSolve
  
  # get inverse of matrix by getMatrix
  getMatrix <- function() m
  
  # returning a list
  # containing all functions defined from above
  list(  
        set = set,
        get = get,
         
        setMatrix = setMatrix,
        getMatrix = getMatrix
      )
}



## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse from
## the cache.

## Returning a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    
    # get matrix
    m <- x$getMatrix()
    
    # check if matrix not equal NULL
    if(!is.null(m)) {
      # if yes, return cached matrix
      message("getting cached matrix")
      return(m)
    }
    
    # else, get the inverse from matrix
    matrix <- x$get()
    
    # solve the inverse
    m <- solve(matrix, ...)
    
    # cache the inverse of matrix
    x$setMatrix(m)
    
    # finally, return the inversed matrix
    m
}


