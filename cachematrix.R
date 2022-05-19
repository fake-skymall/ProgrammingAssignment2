## Put comments here that give an overall description of what your
## functions do

##create NULL matrix m of matching x dim
##set the value of the matrix
##get the value of the matrix
##set the value of the inv
##get the value of the inv

makeCacheMatrix <- function(x = matrix()) {
  #dim<-dim(x)
  #m<-matrix(,xdim[1],xdim[2])
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
## Return a matrix that is the inverse of 'x'
  
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
