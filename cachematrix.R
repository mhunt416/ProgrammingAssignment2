## This function will allow us to avoid recomputing the inverse of a matrix if that
## calculation has already been done.

## This function creates a list of four functions that will
## 1) Set the value of the matrix
## 2) Get the value of the matrix
## 3) Set the value of the inverse
## 4) Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates the inverse of a matrix.  If it has already been calculated,
## it will return that cached value, otherwise it calculates the inverse and stores it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
      message("getting cached inverse")
      return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
