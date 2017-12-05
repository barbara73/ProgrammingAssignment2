## This is an object which stores the inverted matrix.
## There are 4 functions inside (get, set, getsolve, getmean).
## Only change mean to solve:

makeCacheMatrix <- function(x = numeric()) {
  # set m to NULL
  m <- NULL
  # set the parameters and reset M to NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # get parameters
  get <- function() x
  # store m into cache
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  # store in a list s.t. one can call function by name
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## This function inverts a matrix. 
## It takes as input the object makeCachematrix(x).
## Output is the inverse of a matrix.
## I only changed mean to solve:

cacheSolve <- function(x, ...) {
  # get the inverted matrix if it is in cahce
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # if not, claculate the inverse of the matrix
  # by getting the input matrix from object
  data <- x$get()
  m <- solve(data, ...)
  # save inverse matrix in cache by using setsolve
  x$setsolve(m)
  # return inverted matrix
  m
}
