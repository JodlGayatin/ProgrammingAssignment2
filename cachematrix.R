## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a cache matrix based on an input provided.
 makeCacheMatrix <- function(x = matrix()) {
            inv <- NULL
            set <- function(y) {
                        x <<- y
                        inv <<- NULL
                        }
            get <- function() x
            setinverse <- function(inverse) inv <<- inverse
            getinverse <- function() inv
            list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
            }
## This function gets the inverse of the matrix. The assumption is the matrix is a square matrix and is invertible
cacheSolve <- function(x, ...) {
            inv <- x$getinverse()
            if(!is.null(inv)) {
                        message("getting cached data.")
                        return(inv)
                        }
            input <- x$get()
            inv <- solve(input)
            x$setinverse(inv)
            inv
            
            }
## To check we create a 3x3 matrix
## > x <- rbind ( c(1,2,3), c(5,9,13), c(-2,0,7))
## > x
##     [,1] [,2] [,3]
## [1,]    1    2    3
## [2,]    5    9   13
## [3,]   -2    0    7
## > m<-makeCacheMatrix(x)
## We look for the inverse. The cache matrix does not have the data yet
## > cacheSolve(m)
##      [,1] [,2] [,3]
## [1,] -12.6  2.8  0.2
## [2,]  12.2 -2.6 -0.4
## [3,]  -3.6  0.8  0.2
## We look for the inverse again. This time the inverse has already been cached.
## > cacheSolve(m)
## getting cached data.
##      [,1] [,2] [,3]
## [1,] -12.6  2.8  0.2
## [2,]  12.2 -2.6 -0.4
## [3,]  -3.6  0.8  0.2
## To check if indeed the correct inverse is given we do a confirmatory test. 
## A matrix multiplied by its inverese will return an identity matrix
## > x%*%cacheSolve(m)
## getting cached data.
##              [,1]         [,2] [,3]
## [1,]  1.000000e+00 0.000000e+00    0
## [2,] -7.105427e-15 1.000000e+00    0
## [3,]  0.000000e+00 8.881784e-16    1
## As the original was messy we do it again rounding it off to decimal places
## > round(x%*%cacheSolve(m),2)
## getting cached data.
##     [,1] [,2] [,3]
## [1,]    1    0    0
## [2,]    0    1    0
## [3,]    0    0    1


