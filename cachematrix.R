
## The function, makeCacheMatrix creates a special "matrix", which is really a matrix containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the solve(inverse)
##get the value of the solve(inverse)
##sending a matrix as an input parameter
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #Returns the value of x
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  #List of values being returned
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The following function calculates the invers of the "matrix" created with function makeCacheMatrix
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  #Check if the data is already cached
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
