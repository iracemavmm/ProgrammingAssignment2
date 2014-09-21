ProgrammingAssignment2
======================

# Assignment programming 2 ########

makeCacheMatrix <- function(x = numeric()) { cache <- NULL

    ## Store a matrix

    setmatrix <- function(y) {
            x <<- y

    ## Since the matrix is assigned a new value, flush the cache

            cache <<- NULL
    }

    ## Returns the stored matrix

    getmatrix <- function() {
            x
    }

    ## Caches the inverse of a matrix/the given argument

    cacheinverse <- function(solve) {
            cache <<- solve
    }

    ## Gets the cached value of the inverse 

    getinverse <- function() {
            cache
    }

    ## Returns a list where the elements of the list is a function

    list(setmatrix = setmatrix, getmatrix = getmatrix, cacheinverse = cacheinverse, getinverse = getinverse)
}

cacheSolve <- function(y, ...) {

    ## Gets the cached value

    inverse <- y$getinverse()

    ## If a cached value exists return it 

    if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
    }

    ## cc gets the matrix-caclulate the inverse and store it in the cache

    data <- y$getmatrix()
    inverse <- solve(data)
    y$cacheinverse(inverse)
    inverse 

    ## Finally, returns the inverse of the matrix
}

valuematrix <- makeCacheMatrix( matrix(c(1,4,12,26), nrow = 2, ncol = 2) ); 

summary(valuematrix);

valuematrix$getmatrix(); 

cacheSolve(valuematrix)

##the 2nd time we run the function,we get the cached value

cacheSolve(valuematrix)

##Some commands

## solve

#solve- This generic function solves the equation a %*% x = b for x, where b
#can be either a vector or a matrix.
#solve(a, b, tol, LINPACK = FALSE, ...) 
# a a square numeric or complex matrix containing the coefficients of the
# linear system. Logical matrices are coerced to numeric. #
# b a numeric or complex vector or matrix giving the right-hand side(s) of
#the linear system. If missing, b is taken to be an identity matrix and
#solve will return the inverse of a.
#tol the tolerance for detecting linear dependencies in the columns of a.
#The default is .Machine$double.eps. Not currently used with complex matrices
#a.
# LINPACK logical. Defunct and ignored (with a warning for a true value).
#  ... further arguments passed to or from other methods

## Matrix 2x2-numbers in the colunm one are 1 and 4,in the colunm two,12 and26
##summary
# summary- is a generic function used to produce result summaries of the
#results of various model fitting functions. The function invokes particular
#methods which depend on the class of the first argument.


#Results obtained
#Length Class Mode
#setMatrix 1 -none- function
#getMatrix 1 -none- function
#cacheInverse 1 -none- function
#getInverse 1 -none- function


#Results obtained
#     [,1] [,2]
#
#[1,] 1 12 
#[2,] 4 26

#Results obtained
#[,1] [,2]
#[1,] -1.1818182 0.54545455
#[2,] 0.1818182 -0.04545455
#Results obtained
#getting cached data
#[,1] [,2]
#[1,] -1.1818182 0.54545455
#[2,] 0.1818182 -0.04545455
