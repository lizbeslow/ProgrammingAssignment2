## makeCacheMatrix: creates a special "matrix" object that can cache its inverse
## cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix

## This function stores a list of functions that are used to cache the 
## inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #m will be the inverse of matrix x
  set <- function(y) { 
    x <<- y #changes the matrix stored in the main function to y
    m <<- NULL #restores the value of m to NULL when the old m is no longer needed
  }
  get <- function() x #returns matrix x stored in the main function
  setinv <- function(inv) m <<- inv #sets inverse of the matrix to m
  getinv <- function() m #returns inv of matrix x (m) stored in the main function
  list(set = set, get =get,
       setinv = setinv,
       getinv = getinv)
}


## This function returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  ## if m is cached, then just return m
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## otherwise, calculate m and return m
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}
