## This function could create a special "Matrix" object and cache its own inverse.
makeCacheMatrix <- function(x = matrix()) {
        #the cache to store the inverse of the matrix
        InversedMatrixCache <- NULL
        #set the initial matrix obj
        set <- function(y) {
                x <<- y
                InversedMatrixCache <<- NULL
        }
        #return the matrix obj
        get <- function() x
        #store the inverse of the matrix to cache
        setMatrixInverse <- function(inverse) InversedMatrixCache <<- inverse
        #return the chaced inverse result.
        getMatrixInverse <- function() InversedMatrixCache
        list(set = set, get = get,
             setMatrixInverse = setMatrixInverse,
             getMatrixInverse = getMatrixInverse)
        
}


##  This function could calcualte the inverse of the "matrix" created by makeCacheMatrix funxtion. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        InversedMatrixCache <- x$getMatrixInverse()
        #if the the result is in the cache then return from cache
        if(!is.null(InversedMatrixCache)) {
                message("getting cached matrix data")
                return(InversedMatrixCache)
        }
        #get the matrix obj
        data <- x$get()
        #calculate the inverse of the matrix
        InversedMatrixCache <- solve(data, ...)
        #Store the inverse of matrix to the cache
        x$setMatrixInverse(InversedMatrixCache)
        InversedMatrixCache
}