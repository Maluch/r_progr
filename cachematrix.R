
## Put comments here that give an overall description of what your
## functions do


## makeCacheMatrix
##
## Creates a special matrix object containing matrix itself and additionaly its inverse.
## In fact both objects are global.
## The matrix object contains access functions, both `get` and `set` allowing reading and writing matrix and its inverse.
##
## Input: x -- matrix
## Output: a list with four access functions
##
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get    <- function() x
  setinv <- function(inv_) inv <<- inv_
  getinv <- function() inv
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


##
## cacheMatrix
##
## Calculates inverse using a special matrix object created by `makeCacheMatrix` function.
## If the inverse was previously calculated then the calculation is not repeated.
## 
## Input: x -- cache matrix object
## Output: inverse of the given matrix
##
## Details: if the inverse was previously calculated then the user is informed about it.
##
cacheSolve <- function(x, ...) {
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



## testing the functions

m <- matrix(1:4, 2, 2) # example matrix
mCache <- makeCacheMatrix(m) # creating cache matrix object


mInv.1 <- cacheSolve(mCache) # not cached inverse
m %*% mInv.1 # should be I

mInv.2 <- cacheSolve(mCache) # cached inverse
# to be sure they are equal
identical(mInv.1, mInv.2)
