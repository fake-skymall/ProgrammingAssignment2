##Learning how to cache a matrix & assign to the parent environment
##set the value of the matrix
##get the value of the matrix
##set the value of the inv
##get the value of the inv

makeCacheMatrix <- function(x = matrix()) {

  m<-NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Populate or retrieve inverse of matrix x

cacheSolve <- function(x, ...) {
  
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  #the part that inverts the matrix
  m <- solve(data)
  x$setinv(m)
  m
}
