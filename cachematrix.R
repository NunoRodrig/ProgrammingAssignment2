############################################################################################################
## Lib Func:            Funtions for handling Matrix for Caching and Inversion
##
## Definded Functions: 
##                      makeCacheMatrix (Matrix)
##                      cacheSolve (CachedMatrix)
## Lib Func Context:
##                      These functions here built to handle to compute a matrix inversion fast calculation.
##                      This mecanism is split in two functions:
##                        The first one, is for create a special main matrix with the list of several functions; 
##                        The second one, is to apply the inversion to the stored matrix; 
##
############################################################################################################


############################################################################################################
## "makeCacheVector" creates a cached matrix object, which is really a list containing a function to: 
## set the value of the matrix 
## get the value of the matrix 
## set the value of the inverse matrix through the solve R command 
## get the value of the inverse matrix through the solve R command 

##  "makeCacheVector" should be used in conjunction with the "cacheSolve" function.
##  A matrix is given as input.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                                 ## Initiate m
  
  set <- function(y) {                      ## Define set function for input data
    x <<- y
    
    m <<- NULL
  }

  get <- function() x                       ## Define get function for input data

  setinv <- function(solve) m <<- solve     ## Define set function for result
  
  getinv <- function() m                    ## Define get function for result
  
  list(set = set, get = get,                ## Make cached function matrix object
       setinv = setinv,
       getinv = getinv)
}



############################################################################################################

## "cacheSolve" calculate the inverse of the special "matrix" created with the above function makeCacheMatrix.
## However, it first checks to see if the inverse has already been calculated.  
## If so, it gets the inverse from the cache and skips the computation.  
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the 'setInverse' function. 

cacheSolve <- function(x, ...) {
    
  m <- x$getinv()                           ## Get inverted from cache matrix  
  
  if(!is.null(m)) {                         ## Check if value is calculated
    message("getting cached data")  
    return(m)                               ## return cached value and exit
  }
  
  dataget <- x$get()                           ## Get input data from cache
  
  m <- solve(dataget, ...)                     ## Compute invertion
  
  x$setinv(m)                               ## Return a matrix that is the inverse of 'x'
}
