## Caching the Inverse of a Matrix

## makeCacheMatrix: This function creates a special "matrix" object

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y){
    x<<- y
    m<<- NULL
    
  }
  
  get <- function() x
  setinv<- function(inverse) m <<- inverse
  getinv<- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve: This function computes the inverse of the special "matrix"

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m      
  
  ## Return a matrix that is the inverse of 'x'
}