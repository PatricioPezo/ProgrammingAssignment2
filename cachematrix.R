## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function allows to create a list containing a function to
#1.- Set the value of the matrix
#2.- Get the value of the matrix
#3.- Set the value of inverse of the matrix
#4.- Get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
#This function returns the inverse of the matrix. It calculates if there was previously calculated, and if so, it gets the result
#and skip the computation. If it is not the case, computes the inverse, sets the value in the cache via set_inverse function

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("calculating cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}


#Example of the function
# x <- matrix(c(1, -1/8, -1/8, 1), nrow = 2, ncol = 2)
# m <- makeCacheMatrix(x)
# m$get()
#       [,1]   [,2]
#[1,]  1.000 -0.125
#[2,] -0.125  1.000

#No cache in the first run
# cacheSolve(m)
#       [,1]      [,2]
#[1,] 1.0158730 0.1269841
#[2,] 0.1269841 1.0158730

#Returning cache in the second run
# cacheSolve(m)
#calculating cached data.
#          [,1]      [,2]
#[1,] 1.0158730 0.1269841
#[2,] 0.1269841 1.0158730
