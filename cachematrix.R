## The below two functions create a special object that stores a matrix and 
## computes and caches its inverse

##The first function, `makeCacheMatrix` creates a special "matrix", which is
##really a list containing functions to set the value of the matrix, get the value of the matrix, 
##set the value of the inverse of the matrix, and get the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    ##create the special object with the functions called by cacheSolve
  m <- NULL
  set <- function(y) {
    ##set the value of the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x    ##get the value of the matrix
  setinverse <- function(solve) m <<- solve    ##set the value of the inverse
  getinverse <- function() m  ##get the value of the inverse

  list(set = set, get = get,    ##set the list of functions
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix"
##created with the above function. It first checks to see if the
##inverse has already been calculated. If so, it `get`s the inverse from the
##cache and skips the computation. Otherwise, it calculates the inverse of
##the matrix and sets the value of the inverse in the cache via the `setinverse`
##function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' (the special "matrix")
  m <- x$getinverse()    ##gets the value of the inverse stored in the cache
  if(!is.null(m)) {    ##checks to see if the inverse has been cached already
    message("getting cached data")    ##prints a message if the inverse has been cached
    return(m)    ##returns the cached inverse
  }
  data <- x$get()    ##gets the matrix
  m <- solve(data, ...)    ##finds the inverse of the matrix
  x$setinverse(m)    ##caches the inverse of the matrix
  m    ##returns the inverse of the matrix

}
