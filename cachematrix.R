## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
## It basically creates an object where the matrix that is passed in is stored
## The basic get and set functions retrieve (get) and store (set) the stored matrix
## The getInverse and setInverse functions retrieve and store the Inverse of the matrix
## Note the function believes that whatever you pass it is the inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## stores the inverse of the matrix
  invs <- NULL
  
  ## allows you to change the matrix that is being stored
  set <- function(y) {
    x <<- y
    invs <<- NULL
  }
  
  ## returns the matrix that is stored. Added {} for clarity
  get <- function() { x } 
  
  ## stores the value passed, meant to be the inverse of the matrix
  setInverse <- function(inverse) {
    invs <<- inverse
  }
  
  ## returns the inverse of the matrix
  getInverse <- function() invs
  
  ## exposes the functions as a list
  list(
       set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse
      )
}


## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
## If the inverse has already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Check to see if the inverse is already there
    invM <- x$getInverse()
    if(!is.null(invM)) {
        message("getting cached data")
        return(invM)
    }
  
  ## Create a matrix that is the inverse of 'x'
    origM <- x$get()
    invM <- solve(origM, ...)
    x$setInverse(invM)
    invM
}
