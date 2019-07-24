## Put comments here that give an overall description of what your
## functions do

## It basically stores the inverse of matrix in cache so that it doesnt have to be calculated
## for the same matrix again and again. If a different matrix is entered, it automatically 
## calulates its inverse as well.

## Write a short comment describing this function

##    Basically just modified the make_vector only. After intialization of the variables,
##    used the "set" function to reinitialize ensuring that m is null and the value of x
##    is stored. Moving on function "get" takes the same value as 
##    entered in the formals. "setsolve" is used to store the inverse value in the parent
##    environment to access. "getsolve" simply retrieves the data 
##    m as defined previously in set solve. 

##    Thankyou for your time but please let me know if I could have made it better by integrating
##    the comments inside the code line wise or this will suffice.

makeCacheMatrix <- function(x = matrix()) {
  
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
  
}


## Write a short comment describing this function

## Firstly the function tries to retrieve the data of m from the previous function, if it 
## finds it, it gives the message and retrieves data from m stored in cache. If it doesnt,
## it sets it to new variable data and calculates the inverse of it through solve and returns
## the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    m <- x$getsolve()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
  
}
