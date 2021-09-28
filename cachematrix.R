## R Programming - Week 3

## The two function below is designed for cache matrix inversion.

## The function below creates a matrix object and cache its corresponding inverse.

makeCachematrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
    }
  get <- function() x
  setinv <- function(inverse) inv <<-inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  }

## The function below calculates the matrix produced from the function above.
## If the matrix produced has already been calculated, this function will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("obtaining cached from the data")
    return(inv)
    }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  }

##Done
