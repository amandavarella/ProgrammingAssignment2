## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function receives a native R matrix and
## creates a cache matrix, a special matrix whose inverse can be cached
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

## The cacheSolve function receives a cacheMatrix object and calculates its inverse
## If the inverse had never been calculed, the cacheSolve function returns the inverse
## using the function solve and stores in an internal variable
## When the function is called, and the inverse had already been calculed, the function
## just returns the value previously calculed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
