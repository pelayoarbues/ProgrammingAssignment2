## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Programming Assignment 2: Lexical Scoping
##

## The "makeCacheMatrix" function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## The "cacheSolve" function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        if (nrow(x$get()) != ncol(x$get())) {
                message("No possible to invert a rectangular matrix")
                return()
        }
        ## Return a matrix that is the inverse of x
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)    
        x$setinv(m)
        m
}


# 
# #*******************************CODE CHECK
# # 1. Run both functions on Rstudio console
# # 
# # 2. Define a 2x2 matrix called M
# M <- matrix(1:4, ncol=2, nrow=2)
# # 
# # 3. Store the value of running makeCacheMatrix(M) which is a list.
# cacheM <- makeCacheMatrix(M)
# # 
# # 4. Compute, cache, and return the inverse of matrix M
# cacheSolve(cacheM)
# # 
# # [,1] [,2]
# # [1,]   -2  1.5
# # [2,]    1 -0.5
# # 
# # 5. Show the original matrix stored
# cacheM$get() 
# # 
# # [,1] [,2]
# # [1,]    1    3
# # [2,]    2    4
# # 
# # 6. Show the inverse of the matrix stored
# cacheM$getinv()
# # 
# # [,1] [,2]
# # [1,]   -2  1.5
# # [2,]    1 -0.5
# # 
# # 7. Show the inverse of M called from cache
# cacheSolve(cacheM)  
# # 
# # getting cached data
# # [,1] [,2]
# # [1,]   -2  1.5
# # [2,]    1 -0.5