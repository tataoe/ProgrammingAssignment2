## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix- This function calculates the inverse of any square matrix 
## in lexical environment
## cacheSolve- This function is used to cache the caluclated inverse of the square matrix
## in lexical environment


## Write a short comment describing this function
## This function creates a special vector and multiple other functions such as
## set -  set the value of the matrix
## get - read the value of the matrix
## setinv - set the calculated inverse of matrix
## getinv- read the calculated inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Write a short comment describing this function
## This function calculates the inverse of the square matrix.
## At the same time it checks if the inverse is already calulated to save time from 
## cache values and avoid same computation
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
