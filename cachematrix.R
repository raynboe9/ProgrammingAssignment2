## Put comments here that give an overall description of what your
## functions do

## This function will create a matrix, find its inverse and store it in cache.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL   ##setting up the arguments for the matrix.
  set <- function(y) {   
    x <<- y
    s<<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve  ## this solves for the inverse of the entered matrix.
  getsolve <- function() s
  list(set = set, get = get,   ##storing the inverse in cache.
       setsolve = setsolve,
       getsolve = getsolve)
}


## If a matrix's inverse is stored in cache, this will report what's in cache. 
## If not, it will calculate the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {   ##if there is an inverse matrix in cache, retrieve it.
    message("getting cached data")
    return(s)       ##return the inverse matrix.
  }
  data <- x$get()
  s <- solve(data, ...) ##else solve for the inverse.
  x$setsolve(s)
  s
}
