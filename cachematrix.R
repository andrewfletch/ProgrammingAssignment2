## cachematrix.R contains two functions used to find the inverse of a matrix.  
## The first function, makeCacheMAtrix stores a matrix and its inverse in an object.
## The second function, cacheSolve, will either find the inverse of the matrix from makeCacheMatrix, 
## or return a cached inverse already stored in the makeCacheMatrix object

## makeCacheMatrix creates an object that containts the functions "set", "get", "setsolve", 
## and "getsolve".  These functions are made available in the parent environment.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) { #can set a new matrix "y", that changes the matrix "x" in the parent function
                x <<- y 
                s <<- NULL
        }
        get <- function() x #retrieves "x" from parent environment
        setsolve <- function(solve) s  <<- solve #sets the value of "solve", sets the value of "s" in the parent environment to "solve"
        getsolve <- function () s # retrieves "s" from the parent environment
        list(set = set, get = get, # creates a list of functions in the makeCacheMatrix object.  Can be acessed with the $ operator
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve calculates and assigns the inverse of a makeCacheMatrix object to "setsolve".
## if "solve" has been previsously calculated, another computation is avoided by retrieving the cached inverse matrix.

cacheSolve <- function(x, ...) {
        s <- x$getsolve() #gets inverse matrix from makeCacheMatrix
        if(!is.null(s)) { #if the inverse is already assigned to makeCacheMatrix, it retrieves it instead of calculating again.
                message("getting cached data")
                return(s)
        }
        data <- x$get() # assigns the matrix from makeCacheMatrix, to "data"
        s <- solve(data, ...) #calculates the inverse matrix
        x$setsolve(s) #assigns the inverse matrix to the makeCacheMatrix object
        s # prints the inverse matrix
}
