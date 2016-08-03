## the makeCacheMatrix function will set and get a cached data for 
## an inversed matrix

## This function will prepare a special matrix to store the 
## inverse cached value which will be retrieved when requested.

makeCacheMatrix <- function(a = matrix()) {
  i <- NULL
  set <- function(b){
    a <<- b
    i <<- NULL
  }
  get <- function() a
  setinv <- function(solve)  i <<- solve
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The cacheInv function will compute the inverse of a given 
## matrix 'a'

cacheSolve <- function(a, ...) {
  ## Return a matrix that is the inverse of matrix 'a'
  i <- a$getinv()
  if(!is.null(i)){
    message("Getting cached data")
    return(i)
  }
  data <- a$get()
  i <- solve(data, ...)
  a$setinv(i)
  i
}

# Last Edited 03-08-2016
