## Since some computations are resource hungry therefore in order to not to redo computations that have already
## been done, the following type of functions are devised.

## This function takes a matrix as an input and stores it in the cache. It also acts as a handle for the 
## cacheSolve function when the setInv function is used.

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInv <- function(solve) m <<- solve
        getInv <- function() m
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## This function checks the cache and if the solve matrix exists. If it exists then the same is returned otherwise 
## it is calculated and then returned.

cacheSolve <- function(x, ...) {
                
        m <- x$getInv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setInv(m)
        m
}
