# Assignació 2 del curs de R-programming Coursera.
# 
# Inclou les següents funcions:
# makeCacheMatrix: This function creates a special "matrix" object 
#                   that can cache its inverse.
# cacheSolve: This function computes the inverse of the special "matrix" 
#               returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


## Write a short comment describing this function
##############
# Funció makeCacheMatrix
##############
#' Creates an object that can store the computed inverse matrix.
#' 
#' \code{makeCacheMatrix} Creates an object that can store the computed inverse matrix
#' 
#' The function creates an object with 4 "methods" that could be used to compute
#' and store an inverse matrix.
#' Aquesta funció afegeix zeros a l'esquerra d'una cadena fins que aquesta té 
#' el nombre indicat de zeros a l'esquerra
#' 
#' @param x Matrix
#' 
#' @return makeCacheMatrix "object". function with internal data and other functions
#' @export
#' 
#' @examples
#' matriu <- matrix( c(2,1,1,2), 2,2)
#' Cmatriu <- makeCacheMatrix(matriu)
#' inversa <- cacheSolve(Cmatriu)


makeCacheMatrix <- function(x = matrix()) 
{
# To function name makeCacheMatrix we assign the following code where x is the argument passed.
# When we call set we will be called with a variable that will make reference to an object
# like this, so we will assign new value (y) to matrix.
    m <- NULL
    set <- function(y) 
        {
            x <<- y
            m <<- NULL
        }
    get <- function() x
    set.inverse <- function(solve) m <<- solve
    get.inverse <- function() m
    list(set = set, get = get,
         set.inverse = set.inverse, get.inverse = get.inverse)
}



##############
# Funció cacheSolve
##############
#' Computes the inverse matrix.
#' 
#' \code{makeCacheMatrix} Computes the inverse matrix.
#' 
#' The function uses the function makeCacheMatrix to store the matrix to resolve
#' If makeCacheMAtrix has the computation stored, returns the stored value
#' If not, this function computes it and stores in makeCacheMatrix cache
#' 
#' @param x makeCacheMatrix
#' 
#' @return Inverse matrix.
#' 

cacheSolve <- function(x, ...) 
    ## Return a matrix that is the inverse of 'x'
{
    m <- x$get.inverse()
    if(!is.null(m)) 
    {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$set.inverse(m)
    m
}    
