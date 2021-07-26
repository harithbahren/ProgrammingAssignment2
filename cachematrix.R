# makeCacheMatrix is a function which creates a special "matrix" object that can 
# cache its inverse for the input (which is an invertible square matrix)

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        #set the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        # get the value of the matrix
        get <- function() {x}
        # set the value of the inverse
        setinverse <- function(inverse) {inv <<- inverse}
        # get the value of the inverse
        getinverse <- function() {inv}
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# cacheSolve is a function which computes the inverse of the special "matrix" 
# returned by makeCacheMatrix. If the inverse has already been calculated , 
# then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        # if inverse of matrix is cached, inverse is obtained from cached data
        if(!is.null(inv)) {
                message("Getting cached data")
                return(inv)
        }
        # if cache is empty, inverse is calculated and set through setinverse
        data <- x$get()
        inv <- solve(data,...) 
        x$setinverse(inv)
        x
}
# ----- testing the functions -----
# m <- matrix(1:4, 2, 2)
# m1 <- makeCacheMatrix(m)
# m1$get()
#      [,1] [,2]
#[1,]    1    3
#[2,]    2    4
# m1$getinverse()
# NULL
# cacheSolve(m1)
# m1$getinverse()
#      [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
# cacheSolve(m1)
# Getting cached data
#      [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5