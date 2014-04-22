## makeCacheMatrix takes a square, invertable matrix as an input, stores it in a hidden cache 
## and returns a list of four functions to access the hidden cache.
##      get: retrieves the hidden matrix
##      set: stores a new matrix in the hidden cache
##      setinverse: computes the inverse to the matrix and stores it in the hidden cache
##      getinverse: retrieves the computed inverse from the hidden cache

makeCacheMatrix <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        set(x)
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve utilizes the function list returned from makeCacheMatrix as an input and 
## returns the inverse of the cached matrix. If the inverse has already been computed 
## it simply retrieves the answer from the cache, otherwise it uses the Solve function 
## to compute it and store it in the cache before returning it.

cacheSolve <- function(x, ...){
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m
}
