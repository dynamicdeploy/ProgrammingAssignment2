## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse is already cached and the matrix has not changed, the function returns result from the cache.

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## The output from this function is the input to the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
    #create a matrix using the makeCache matrix
    #cache the matrix using the cacheSolve matrix
    
    i <- NULL
    set = function(y) {
        x <<- y
        i <<- NULL
        
    }
    get = function() x
    setinv = function(inverse) i <<- inverse 
    getinv = function() i
    list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    #The input to cacheSolve matrix should come from the matrix created using makeCacheMatrix()
    
    i = x$getinv()
    
    # if the inverse is already cached
    if (!is.null(i)){
        # return from the cache
        message("getting cached data")
        return(i)
    }
    
    # else, inverse it 
    mat.data = x$get()
    i = solve(mat.data, ...)
    
    # cache the inverse
    x$setinv(i)
    
    return(i)
}

testmatrixinv <- function(mat){
    
    print("Starting matrix performance test!!")
    t = makeCacheMatrix(mat)
    
    #without cache
    start.time = Sys.time()
    cacheSolve(t)
    stopwatch = Sys.time() - start.time
    print(stopwatch)
    
    #with cache
    start.time = Sys.time()
    cacheSolve(t)
    stopwatch = Sys.time() - start.time
    print(stopwatch)
    
    
}
