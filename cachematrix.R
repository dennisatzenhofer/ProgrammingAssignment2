## makeCacheMatrix and cacheSolve can be used to cache the inverse of a matrix and return it.

## This is function should cache in the inverse of a matrix.

makeCacheMatrix <- function (x = as.matrix())  {
    m <<- x 
    inv <<- solve(m)
    list(matrix = m, inverse = inv) # creating an object containing the original matrix and the inverse
  }
  
## cacheSolve makes use of the inverse that was cached in by the function makeCacheMatrix. It returns the cached inverse or calculates the inverse
## if the object does not contain cached data.

cacheSolve <- function(x) {
## By checking whether x is atomic, one can control whether the object is a regular matrix. If so, one first needs to run the makeCacheMatrix, in order to
## get an object that can be used for the cacheSolve function. 
  if (is.atomic(x)) {
    message("first run function 'makeCacheMatrix'")
  }
  else {
  m <- x$inverse
  if(!is.null(m)) {                     ## using cached inverse
    message("getting cached data")
    return(m)
  }
  raw_matrix <- x$matrix
  inv <- solve(raw_matrix)              ## calculating inverse if no cached data available
  x$inverse <- inv
  inv
  }
}
