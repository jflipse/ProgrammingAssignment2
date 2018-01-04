makeCacheMatrix <- function(x = matrix()) {
        
        # 
        # This function creates a special "matrix" object that can cache its inverse.  It creates a
        # square matrix, inverts it, then writes it to global cache for later retrieval and use.
        #
        # JFlipse - 3 Jan 2018
        #
        
        # Set the matrix to be inverted, default to NULL
        iMatrix <- NULL
        set <- function(y) {
                x <<- y
                iMatrix <<- NULL
        }
        
        # Get the matrix
        get <- function() x
        
        # Set the inverted matrix
        #        setinv <- function(inverse) iMatrix <<- inverse
        setinv <- function(solve) iMatrix <<- solve
        
        # Get the inverted matrix
        getinv <- function() iMatrix
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

cacheSolve <- function(x,...) {
        
        # 
        # This function checks it an inverted matrix exists in cache.  If it does then
        # it is displayed, else a new matrix is created and inverted.
        #
        # JFlipse - 3 Jan 2018
        #
        
        # Call getinv to check for the inverted matrix "x" in cache
        iMatrix <- x$getinv()
        
        if(is.null(iMatrix)) {
                message("cached matrix not found, creating...")
                tMat.data <- x$get()                # Call get() to get the matrix
                iMatrix <- solve(tMat.data, ...)    # use solve to invert
                x$setinv(iMatrix)                   # save the inverted matrix to cache
        } else {
                message("cached matrix found...")
        }
        return(iMatrix)
}