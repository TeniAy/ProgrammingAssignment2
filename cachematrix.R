## My function calculates the inverse of a matrix if the cached value is null (i.e.if m is null)
## If there is a cached value for the inverse, my function gets the value from the list created in makeCacheMatrix

## This function creates a list which included getting & setting the matrix object and the inverse value

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the matrix if there is no value in the cache(i.e. if m is null)

cacheSolve <- function(x, ...) {
   m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m    
}
