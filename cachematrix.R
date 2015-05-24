## This is a set of functions that will compute and cache
## the inverse of a matrix


## This function creates a special matrix (which looks a lot like an object) that 
## can store the original matrix and its inverse 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL #declare/clear inverse variable
        set <- function(y) {
                mtrx <<- y #assign the matrix variable whatever was passed
                inv <<- NULL #clear the inverse variable
        }
        get <- function() mtrx
        setCacheInvMtrx <- function(Cachematrix) inv <<- Cachematrix
        getCacheInvMtrx <- function() inv
        list(set = set, get = get,
             setCacheInvMtrx = setCacheInvMtrx,
             getCacheInvMtrx = getCacheInvMtrx)
}

## The following function determines if a cached inverse exists.  
## If the inverse is cached the values is returned.  
## If no cache exists, the inverse of the matrix is calculated, cached and returned.

cacheSolve <- function(x, ...) {
        m <- x$getCacheInvMtrx()
        if(!is.null(m)) {
                message("getting cached data") #notify that cached data is being returned
                return(m) #return value and exit function
        }
		#only get to this point if did not exit above with return statement
        data <- x$get() #retrieve matrix stored in the object
        m <- solve(data, ...) #calculate inverse of matrix
        x$setCacheInvMtrx(m)
        m
}
