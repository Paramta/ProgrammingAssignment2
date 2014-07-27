## ##################################################################
##
##	Objective: To obtain matrix inverse
## 	Author: Paramita Mukhopadhyay
##	Date: Jul-27-2014
##
#####################################################################

makeCacheMatrix <- function( m = matrix() ) {
    i <- NULL
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

     get <- function() {
    	m
    }
    setInverse <- function(inverse) {
        i <<- inverse
    }
    getInverse <- function() {
        i
    }
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()

    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    data <- x$get()
    m <- solve(data) 
    x$setInverse(m)
    m
}
