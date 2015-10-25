## Put comments here that give an overall description of what your
## functions do

#The following two function are used to cache the inverse of a matrix.

# The first function, makeCacheMatrix, creates a special "matrix", which is really a list containing a function to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix

## Write a short comment describing this function

#makeCacheMatrix is a function that creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<-NULL
  }
  get <- function()x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

#cacheSolve returns the inverse of the matrix.
#It first checks to see if the inverse has already been calculated.
#If so, it gets the result and skips the computation.
#If not, it computes the inverse, sets the value in the cache
#via the setinverse function.

cacheSolve <- function(x, ...) {
  inv <-x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
