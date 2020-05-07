## A function that are used to create a special object that stores a numiric
## vector and cache's it's inverse.

## This function creates a special ""matrix object that can cashe its inverse

makeCachueMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## this function computes the inverse of special "matrix" returned by makeCachueMatrix
## matrix 

cacheSolve <- function(x, ...) {
  
  z <- x$getinverse()
  
  if (!is.null(z)) {
    
    message("getting cached data")
    
    return(z)
    
  }
  matrix <- x$get()
  
  z <- solve(matrix,...)
  
  x$setinverse(z)
  
  z
  
}
