## Functions to compute and cache inverse matrix computation

## Create a list of functions to get, set the input matrix, and 
##    get, set its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(pinv) inv <<- pinv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Caclulate the inverse of a matrix. If the inverse already exists, 
##    return the previously calculated value.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

## Sample test
X <- makeCacheMatrix( matrix(data=rnorm(1000000,100.0, 20.0),nrow=1000,ncol=1000))
system.time( {R <- cacheSolve(X)} )
system.time( {R <- cacheSolve(X)} )


