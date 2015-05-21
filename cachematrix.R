## The function makeCacheMatrix will cache the inverse of a matrix
## 
## example
## a <— makeCacheMatrix(matrix(1:4, 2, 2))
## cacheSolve(a)
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## Create a list containing a function to:
##   - set the value of the matrix
##   - get the value of the matrix
##   - set the value of the inverse matrix
##   - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## If the inverse of the matrix is stored in the cache
## this function will retrieve the value of the inverse
## and print it

cacheSolve <- function(x = matrix()) {
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix)
        x$setinverse(inv)
        inv
}
