## This function creates a special "matrix" object that can cache its inverse. 
## The cached inverse matrix is set to NULL  

makeCacheMatrix <- function(m = matrix()) {
  inv_m <- NULL
  
  ## inits the object with a new matrix
  ## resets cached data 
  ## i.e. tippical setter method
  set <- function(y) {
    m <<- y
    inv_m <<- NULL
  }
  
  ## returns the matrix
  get <- function() {
    m
  }
  
  #calculates matrix inversion if it is not calculated yet. If one is calculated the cached results are returned 
  get_inverse <- function(){
    if(!is.null(inv_m)) {
      message("get cached result")
      inv_m
    } else{
      message("calculate result")
      inv_m <<- solve(m)
      inv_m
    }
  }
  list(set = set, get = get, get_inverse = get_inverse)
  
}


## Returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  x$get_inverse()
}
  