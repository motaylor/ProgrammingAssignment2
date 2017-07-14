

## makeCache takes the inverse of the input matrix value

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
  x <<- y
  m<<- NULL
  }
  get<- function() x
  setinverse<- function(inverse)m <<- inverse 
  getinverse <- function() m
  list(set=set,get=get,
       setinverse= setinverse,
       getinverse=getinverse)
  }



## computes inverse
##if inverse has already been calculated, retrieves from cache

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m<- inverse(data,...)
  x$setinverse(m)
  m
}
  