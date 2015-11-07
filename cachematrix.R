## The functions to compute the inverse, store in cache and retrieve from cache
## if already calculated

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m_inv <- NULL ##begin by setting an empty inverse as a placeholder for inverse matrix
        
        set <- function(y) {
                x <<- y
                m_inv <<-NULL
        }
        ## returns the matrix, x
        get <- function() x
        ## sets the inverse, m_inv, to inverse_mat
        setinv <- function(solve) m_inv <<- solve
        ## returns the inverse, m_inv
        getinv <- function() m_inv
        ## returns the 'special matrix' containing all of the functions just defined
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        m_inv <- x$getinv()
        if(!is.null(m_inv)) {
                message("getting cached data")
                return(m_inv)
        }
        matrix <- x$get()
        m_inv <- solve(matrix, ...)
        x$setinv(m_inv)
        m_inv
}
