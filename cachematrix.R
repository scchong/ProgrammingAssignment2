## This function creates a special "matrix" object that can cache its inverse.
## It returns a list containing functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of its inverse
## 4. get the value of its inverse 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, 
             get = get, 
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve should retrieve the inverse from 
## the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        
        ## if the inverse has already been calculated, get from cach
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## otherwise calculates the inverse 
        mat <- x$get()
        inv <- solve(mat, ...)
        
        ## sets the value of the inverse in the cache
        x$setinv(mat)
        
        inv
}
