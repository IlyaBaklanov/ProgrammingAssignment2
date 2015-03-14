## Programming Assignment 2 (peer assessment): Lexical Scoping
## Due on Sun 22 Mar


## These two functions allow us to define matrix and to calculate 
## its inversion. 

## The first function creates a list of functions which we will use later:
## $set - set the matrix
## $get - get the matrix
## $getsolve - set the value of the matrix inversion
## $setsolve - get the value of the matrix inversion

makeCacheMatrix <- function(x = matrix()) {
     s <- NULL
     set <- function(y) {
          x <<- y
          s <<- NULL
     }
     get <- function() x
     setsolve <- function(solve) s <<- solve
     getsolve <- function() s
     list(set = set, 
          get = get,
          setsolve = setsolve,
          getsolve = getsolve)
}

## The second function check whether inversion of matrix was already calculated. 
## If yes, the value is just gotten from cache and message "Getting cached data!" 
## returned. If no, inversion of matrix is calculated.

cachesolve <- function(x, ...) {
     s <- x$getsolve()
     if(!is.null(s)) {
          message("Getting cached data!")
          return(s)
     }
     data <- x$get()
     s <- solve(data, ...)
     x$setsolve(s)
     s
}


##One can use this to check how the functions work:

##First run:
##v1 <- matrix(1:4, nrow = 2, ncol = 2)
##v1c <- makeVector(v1)
##cachesolve(v1c)

##Second run:
##cachesolve(v1c)