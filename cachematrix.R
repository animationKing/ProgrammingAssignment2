## The following contains a pair of functions 
## that cache the inverse of a matrix

## This function creates a special "matrix" object 
## that can cache its inverse.

makeCachematrix <- function(x = matrix()) {
 
    matrix <- NULL
    set <- function(y) 
  {
    x <<- y
    matrix <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) matrix <<- solve
  getinverse <- function() matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special 
## "matrix" returned by makeCachematrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve retrieve 
## the inverse from the cache.

## !!we assume that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
       
  matrix <- x$getinverse()
  if(!is.null(matrix)) {
    message("getting cached data")
    return(matrix)
  }
  data <- x$get()
  matrix <- solve(data, ...)
  x$setinverse(matrix)
  matrix
}
