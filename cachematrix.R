## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function accepts a square matrix (x) and establishes global variables:
# minv -- the matrix inverse 
# morg -- a copy of the original matrix for detecting any changes or a new matrix
# ml   -- a list of processing functions for cacheSolve (see next function)
# This function is called ONCE!
makeCacheMatrix <- function(x = numeric()) {  # Input is a square matrix
     minv   <<- NULL  # global inverse not calculated; initialized to NULL
     morg   <<- x     # global original matrix; copy matrix given
     set    <- function() {morg <<- y; minv <<- NULL }
     get    <- function() x  # return data
     setinv <- function(minv) minv <<-minv # set global inverse
     getinv <- function() minv  # return cache inverse
     # set up a global list of processing functions for use in cacheSolve
     ml     <<-list(set = set, get = get,setinv = setinv,getinv = getinv)
     return()
} # *** end function makeCacheMatrix ***
# --------------------------------------
# This function is called following makeCacheMatrix and utilizes the global
# variables it established (see above function).
# Input: a square matrix
# Checks if the inverse needs to be calculated, it does so and returns it.
# It also checks to see if the original matrix has been changed, or a new 
# matrix is being given, it also then calcuates the inverse.
# If the matrix is the same, and inverese exists, it returns the global inverse
# without having to do a new calculation.
cacheSolve <- function(x, ...) {  # Input is a square matrix
     ## Return a value that is the inverse of 'x'
     minv <<- ml$getinv()  # retrieve the global inverse
     if((is.null(minv)|  # check if the inverse is null or ...
        (sum(morg==x)!=prod(dim(x))))) # the matrix has changed ...
          {data <- x              # get the matrix
           minv <<- solve(data)   # calculate the global inverse (note: <<-)
           ml$setinv(minv)        # set the global inverse
           morg <<- x  }          # reset the global original matrix
     else
          # no calculation needed, return cached global inverse
          {message("getting cached data")} 
     return(minv)  # return the cached or global inverse
} # *** end function cacheSolve ***
# ---------------------------------
