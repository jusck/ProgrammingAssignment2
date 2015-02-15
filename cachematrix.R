## Forked from https://github.com/rdpeng/ProgrammingAssignment2 for Coursera R Programming Course

## The purpose of these functions are to allow a matrix to be held and retrieved and also the intensive
## operation (potentially) of generating it's inverse to be carried out but cached. So if the matrix has
## not changed the inverse is returned without the need for it to be recalculated.


## makeCacheMatrix creates an object which has the get set getinverse and setinverse functions
## get function returns the matrix held in the object
## set function stores a matrix in the object
## getinverse function returns the inverse of the matrix held in the object
## setinverse function sets the inverse of the matrix held into the object

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function () x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set=set, get=get, 
             setinverse = setinverse, 
             getinverse=getinverse)
        
}


## cacheinverse returns the inverse of the stored matrix. If cached version is still relevant this is returned
## otherwise it is derived, cached and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("Getting cached data.")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
