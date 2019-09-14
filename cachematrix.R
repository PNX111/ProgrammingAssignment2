## Below function will create cache mtrix and 
## functions do

## Function will cache Matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function()x
  setI1 <- function(inverse) m <<- inverse
  getI1 <- function() m 
  list(set = set, get = get, 
       setI1 = setI1, 
       getI1 = getI1)
}


## Return matrix^-1

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'  
  m <- x$getI1()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setI1(m)
  
  m
  
}
