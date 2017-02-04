##
## This R script contains two functions:
##
##     1- makeCacheMatrix : create a "special" matrix object that can cache a matrix and its inverse
##     2- cacheSolve      : calculate inverse of a matrix, store it in a makeCacheMatrix object, and 
##                          returns it. Can retrieve it from a makeCacheMatrix object's cache in a subsequent call                    
##
## Author: bdiocsera
##
## Rev. 0 (02/03/2017)

#
# Create a matrix object with a cache
#
# Usage: - cacheMatObj <- makeCacheMatrix()     : create an empty cacheMatObj that can be set with cacheMatObj$set(matA)
#        - cacheMatObj <- makeCacheMatrix(matA) : initialize cacheMatObj by storing matrix matA in its cache
#        - cacheMatObj$set(matA)                : store matrix matA in cache of cacheMatObj
#        - cacheMatObj$get()                    : retrieve matrix matA from cacheMatObj cache
#        - cacheMatObj$setinv(matB)             : store matrix matB (inverse of matA) in cacheMatObj cache
#        - cacheMatObj$getinv()                 : retrieve matrix matB (inverse of matA) from cacheMatObj cache
#
makeCacheMatrix <- function(mtx = matrix()) {
    mtx_inv <- NULL
    
    set <- function(mty) { # store matrix in cache
        mtx <<- mty
        mtx_inv <<- NULL
    }
    
    get <- function() mtx # retrieve matrix from cache
    
    setinv <- function(solved) mtx_inv <<- solved # store inverse matrix in cache
    
    getinv <- function() mtx_inv # retrieve inverse matrix from cache
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

#
# Return a matrix that is the inverse of 'mtx'
#
# Usage: - matB <- cacheSolve(cacheMatObj, ...) 
#          where cacheMatObj is an object defined by makeCacheMatrix
#                        ... elipsis for optional arguments for solve
#
cacheSolve <- function(mtx, ...) {
    mtx_inv <- mtx$getinv() # retrieve inverse from cache
    
    # check if mtx_inv is not empty (because it was calculated before)
    if(!is.null(mtx_inv)) {
        message("Obtaining inverse from cache")
        return(mtx_inv)
    }
    
    # If inverse hasn't been calculated: 1st retrieve matrix from cache, 2nd calculate inverse
    matx_data <- mtx$get()
    if(anyNA(matx_data)) {
        stop("Matrix was not set")
        
    } else {
        mtx_inv <- solve(matx_data, ...) # solve for the inverse 
        mtx$setinv(mtx_inv)         # store inverse in cache
        mtx_inv                     # return inverse
    }
}
