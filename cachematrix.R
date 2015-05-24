## The two functions below create a special object that stores a matrix and its inverse.

## The function makeCacheMatrix creates a list of four functions: set, get, setinv, and getinv, which do the following
# 1. set: sets the value of a matrix
# 2. get: gets the value of the matrix
# 3. setinv: sets the inverse of the matrix
# 4. getinv: gets the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## The function cacheSolve returns the inverse of the matrix created by  makeCacheMatrix.
# If the inverse was previously calculated, it returns the cached value. 
# Otherwise, it calculates the inverse and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  dimension = sqrt(length(data))
  dim(data)=c(dimension,dimension)
  inv <- solve(data)
  x$setinv(inv)
  inv
}
