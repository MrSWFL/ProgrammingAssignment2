## Data Science Course "R Programming" assignment 2.
## Learn how to cache large matrices and the results of calculations
## using them. This can potentially save large amounts of computation
## time by only having to perform thae calcs once, and retrieving the 
## already calculated results in subsequent time.
## makeCacheMatrix creates a "special" matrix which will contain the matrix and it's
## inverse (after being calculated once).


makeCacheMatrix <- function(x = matrix()) {
  ## Create a "special" matrix to hold the original matrix and it's inverse.
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Returns the inverse matrix using the cached copy (if it exists), else claculate the
## inverse and save it for future use.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matr <- x$get()
  m <- solve(matr, ...)
  x$setinverse(m)
  m
}