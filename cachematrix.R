##
## Assignment: Caching the Inverse of a Matrix
##

## Create "storage" function which can cache matrix (use 'set' and 'get' functions to access it)
## and its inverse ('set_inv_matrix' and 'get_inv_matrix' functions)

makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL
  set <- function(y) {
    x <<- y
    invm <<- NULL
  }
  get <- function() x
  
  set_inv_matrix <- function(i) {
    invm <<- i
  }
  get_inv_matrix <- function() invm
  
  list(set = set, get = get, set_inv_matrix = set_inv_matrix, get_inv_matrix = get_inv_matrix)
}


## Return an inverse of a matrix. If the inverse was previously calculated the cached value will be returned.
##
## x  makeCacheMatrix function to access matrix and its inverse

cacheSolve <- function(x, ...) {
  invm <- x$get_inv_matrix()
  if (!is.null(invm)) {
    message("Getting cached inverted matrix")
    return(invm)
  }
  
  data = x$get()
  
  invm <- solve(data)
  x$set_inv_matrix(invm)
  invm
}

