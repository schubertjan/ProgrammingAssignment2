## The first function creates a list of functions that are used to calculate 
## matrix inversion and cache it. The second function takes the object created 
## by the first function and checks if the inverse of a matrix exists within
## this object. If it does, it returns the cached inverse of the matrix
## if it doesn't it will calculate the inversion.


## The function sets a value of the matrix and the inverse matrix
## it then gets the values of the original matrix
## sets the value of the inverse matrix
## and gets the value of the inverse matrix



makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
      x <<- y
      i <<- NULL
  }
  get <- function() x
  setInv <- function(solve) i <<- solve
  getInv <- function() i
  list(set = set, get = get,
       setInv = setInv, getInv = getInv)
}


## The function first gets the value if the inverse matrix
## and evaluates if the value is null (not cached). If not null (cached)
## it returns the inverse matrix. If null (not cached) it will calculate
## the inversion of the matrix

cacheSolve <- function(x, ...) {
        i <- x$getInv() 
        if(!is.null(i)) {
          message("getting cached data")
          return(i)
        }
        data <- x$get()
        i <- solve(data,...)
        x$setInv(i)
        i
}
