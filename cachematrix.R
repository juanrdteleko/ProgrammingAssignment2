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
  getinverse <- function() x_inv   ##to obtain the cache-stored inverse matrix saving some high-costly calculations
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
##retrieve the inverse from the cache. We will assume that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  x_inv <- x$getinverse()   ##it x_inv will be the inverse matrix previosly calculated or NULL in case the matrix has changed
  if(!is.null(x_inv)) {
    message("getting cached data")    ##if not NULL it will retrieve the inverse matrix from cache
    return(x_inv)
  }
  data <- x$get()                     ##if NULL it means that new matrix has been provided and we need to calculate the inverse matrix
  x_inv <- solve(data, ...)           ##here we calculate the new inverse matrix
  x$setinverse(x_inv)                 ##and here we store the inverse matrix in cache to use it later in case we ask for it again
  x_inv

}
