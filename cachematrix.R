## Allysson Jill T. Miralles
## R Programming - Week 3


## The two functions are designed to cache matrix inversion


##makeCacheMatrix
## This function creates a special matrix object 
## and cache its corresponding inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


##cacheSolve 
## This function calculates the matrix produced from the makeCachematrix
## However, if the matrix has been calculated, this function will retrieve the inverse
## from the cache

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("obtaining cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
