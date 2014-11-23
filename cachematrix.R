## Following pair of functions cache the inverse of a special matrix.
     
## makeCackeMatrix: this function creates a matrix for which the inverse
## can be cached.
     
makeCacheMatrix <- function(x = matrix()) {
        inv_matrix <- NULL
        set <- function ( oth_matrix ) {
                x <<- oth_matrix
                inv_matrix <<- NULL
        }
        setinverse <- function( inv_mat ) inv_matrix <<- inv_mat
   
        get <- function() x
        getinverse <- function() inv_matrix
         
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}
     
## cacheSolve: This function computes the inverse of the matrix created by
## makeCacheMatrix using the solve function in R.
     
cacheSolve <- function( x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_matrix <- x$getinverse()
        if(!is.null(inv_matrix)) {
                message("getting cached data")
                return (inv_matrix)
        }
        inp_matrix <- x$get()
        inv_matrix <- solve(inp_matrix)
        x$setinverse(inv_matrix)       
        inv_matrix
}

