#creates functions and allows user to put them in a list to be called later

makeCachedMatrix <- function(x = matrix()) {
  cacheMakeMatrix <- function(x = matrix()) {
    m <- NULL # creates cache
    set <- function(y) {
      x <<- y
      m <<- NULL   #function used to set new input and reset cache for result of 
      # the new input
    }
    get <- function() x # gives user the current input and allows for use of it
    
    setmatrix <- function(mtrx) m <<- mtrx #function that caches output from cacheSolve
    
    getmatrix <- function() m   # finds value for m 
    list(set = set, get = get,
         setmatrix = setmatrix,  #names all the functions for use with $
         getmatrix = getmatrix)
  }
  
}


# takes listed makecachedMatrix and uses it's funct's to provide a result

cacheSolve <- function(x, ...) { 
    m <- x$getmatrix()
    if (!is.null(m)){ #using function to check if a value exists + retrieving it
      message("retrieving your matrix")
      return(m)
    }
    data <- x$get()
    m <- solve(data,...)  #uses $get function to get provided input and stores result
    x$setmatrix(m)
    m
} # in this case returns the inverse matrix of the inputted matrix 

