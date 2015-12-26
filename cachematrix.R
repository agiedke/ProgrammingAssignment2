## The two functions below (makeCacheMatrix() and cacheSolve()) use the same 
## approach as the example given by the Coursera team (makeVector() and 
## cachemean()) and apply it to caching/calculating the inverse of a matrix
## instead of caching/calculating the mean of a vector.


## The first function makeCacheMatrix() takes a matrix as an input and returns 
## a list of four functions: 
## 1) set() changes the matrix x
## 2) get() returns the matrix x
## 3) setinverse() assigns the result of the solve-function to the object inv,
## which is returned in the second function.
## 4) getinverse() returns the inverse (if it was calculated already, e.g. if 
## the function is used in a loop)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The second function cacheSolve() takes the result of makeCacheMatrix and
## returns either a) the calculated inverse of the matrix or b) the message
## "getting cached data" plus the inverse which has already been calculated
## before.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
