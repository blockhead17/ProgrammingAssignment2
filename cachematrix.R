## The two primary functions defined below (makeCacheMatrix and cacheSolve) 
## define functions that are used to set and retreive an invertible matrix
## provided by the user as well as the solution (inversion) for that matrix.
## When the solution for the matrix is needed, the functions determine if 
## the solution is stored in the cache, returning the cached solution when
## available or generating the new solution and caching for future use when
## it is not available.

## makeCacheMatrix is a function that defines four other functions
## (set, get, setinv, getinv).  The argument for the main function (x) is 
## presumed to be an invertible matrix.  Calling this function returns
## a list of the four functions defined within.

makeCacheMatrix <- function(x = matrix()) {
      ## Initialize the inverse matrix at the outset of makeCacheMatrix call
      inverseMatrix <- NULL
      
      ## When the set function is called, its argument (y) replaces the matrix stored
      ## within the main function makeCacheMatrix. By doing so, any inverse matrix
      ## previously stored must be re-initialized.
      set <- function(y) {
            x <<- y
            inverseMatrix <<- NULL
      }
      
      ## When the get function is called, it returns the matrix x stored within
      ## the main function makeCacheMatrix
      get <- function() x
      
      ## When the setinv function is called, its argument (z) replaces the inverse
      ## matrix
      setinv <- function(z) inverseMatrix <<- z
     
      ## When the getinv function is called, it returns the inverse of matrix x
      getinv <- function() inverseMatrix
      
      ## Store the four functions in the body of makeCacheMatrix as a list
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve is a function that determines if the solution is stored in the cache.
## The cached solution is displayed when it is available using the getinv function.
## If not available, the solve function is used to generate a new solution, display it
## and cache it for future use.  The argument for the main function (w) is 
## presumed to be an invertible matrix. 

cacheSolve <- function(w, ...) {
      ## Call the getinv function (defined above) and determine of a cached solution
      ## if available for the argument matrix (w).  Assign the results of this
      ##function call to a variable (inverseMatrix).
      inverseMatrix <- w$getinv()
      
      ## If the result of this of the getinv call is not null, there was a cached
      ## solution available.  Let the user know that the cached solution is being 
      ## used and return it.
      if(!is.null(inverseMatrix)) {
            message("Getting cached data...")
            return(inverseMatrix) ## If return is called, function is done.
      } 
      
      ## If the condition above is not true (i.e. there is no cached solution), 
      ## then do the following:
      else {
      
      ## Call the get function (defined above) so the new matrix passed
      ## in the argument (w) is assigned to a variable (newMatrix)
      newMatrix <- w$get()
      
      ## Solve the matrix assigned to the variable just created.
      inverseMatrix <- solve(newMatrix, ...)
      
      ## Cache the solution just generated for future use.
      w$setinv(inverseMatrix)
      
      ## Return the solution.
      inverseMatrix
      }
}
