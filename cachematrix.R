## creates and assigns functions usuable with $

makeCacheMatrix <- function(x = matrix()) {
  function(x = matrix()) {
    m <- NULL # creates cache
    set <- function(y) {
      x <<- y
      m <<- NULL   #function used to set new input and reset cache for result of 
      # the new input
    }
    get <- function() x #function that provides new input to be used by 
    # leaving x out of get environment returns parent environment
    
    setmatrix <- function(mtrx) m <<- mtrx #function that caches output from 
    #cacheSolve
    
    getmatrix <- function() m   # finds value for m 
    list(set = set, get = get,
         setmatrix = setmatrix,  #names all the functions for use with $
         getmatrix = getmatrix)
  }
  
}


# takes listed makeCachedMatrix and uses it's funct's to provide a result

cacheSolve <- function(x, ...) { 
    m <- x$getmatrix()
    if (!is.null(m)){ #using function to check if a value exists + retrieving it
      message("retrieving your matrix")
      return(m)
    }
    data <- x$get()
    m <- solve(data,...)  #using function to get provided input and make output
    x$setmatrix(m)
    m
} # Returns the inverse matrix of the inputted matrix 

