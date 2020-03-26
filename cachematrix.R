# These functions take advantage of the scoping in R to eliminate unnecessary computations.
# The functions conduct a computation (invert matrix) once, then store the result in memory for future use.
# This eliminates the need to recompute the inverse of a matrix at a future time.

## The makeCacheMatrix function takes as input the matrix to be inverted (we assume it's invertable) and
## creates an environment to store the original matrix as well as future inverse computation

makeCacheMatrix <- function(x = matrix()) {
  invMat <- NULL # set the initial value of the invMat
  setMat <- function(y){
    # function to store the given matrix in the parent
    x <<- y
    invMat <<- NULL
  }
  getMat <- function () x # function to give the matrix back
  setInv <- function (mat) invMat <<- mat # function to set the value of the invMat in the parent
  getInv <- function () invMat # function to give the inverse back
  list(setMat = setMat, getMat = getMat,
       setInv = setInv, getInv = getInv)
  # return the functions for use  
}


## The cacheSolve function takes as input the list created by the makeCacheMatrix function and
## calculate the inverse matrix, if necessary, or retrieve the cached previously calculated inverse
## and returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invMat <- x$getInv() # check to see if the inverse exists
  if(!is.null(invMat)){ # if it does return the cached inverse
    message('Cached Inverse')
    return(invMat)
  }
  data <- x$getMat() # get the matrix back 
  invMat <- solve(data) # calculate the inverse
  x$setInv(invMat) # store the inverse in the cache
  invMat # return the calculation
}
