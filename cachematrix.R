## These functions cache the value of the inverse of a matrix
## so that when we need it again we can look it up in the cache
## rather than needing to compute it again.

## This function creates a list of the functions that we want
## to apply to the matrix that we input into the makeCacheMatrix function
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL 
        set <- function(y) {
                x <<- y 
                m <<- NULL 
        }
        get <- function() x 
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}


## This is the function that looks in the cache to see if the inverse
## has already been calcuated. If the inverse has been calculated it 
## is returned to the console. If the inverse has not been calcuated
## it is calcuated and added to the cache. 
cacheSolve <- function(x, ...) {
                m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

## The below is an example that I used to test the code using
## matrix multiplication to verify that the answer is correct. 
a <- makeCacheMatrix(matrix(1:4,2))
b <- cacheSolve(a)
a$get() %*% b

