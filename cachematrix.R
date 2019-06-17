## This pair of functions will  cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  x_inv<- NULL
  set <- function(y) {      ## this set function is used to create the matrix and restart the inverse matrix x_inv each time a new matrix is passed
    x <<- y
    x_inv <<- NULL
  }
  get <- function() x        ## just to obtain the matrix which will be used to calculate the inverse matrix
  setinverse <- function(inverse) x_inv <<- inverse   ## to store in cache the previosuly inverse matrix calculated
  getinverse <- function() x_inv   ##to obtain the cache-stored inverse matrix saving some high-costed calculations
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
##retrieve the inverse from the cache. We will assume that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  x_inv <- x$getinverse()
  if(!is.null(x_inv)) {
    message("getting cached data")
    return(x_inv)
  }
  data <- x$get()
  x_inv <- solve(data, ...)
  x$setinverse(x_inv)
  x_inv

}
