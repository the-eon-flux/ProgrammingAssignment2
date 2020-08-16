## Caching the inverse of a matrix rather than compute it repeatedly.
## 2 functions required. First : Makes a cacheableMatrix. Second : Computes inverse of
## given matrix if it's not pre calculated.

## Following Fn gives a list of 4 functions
'sets the value of the matrix
gets the value of the matrix
sets the value of the inverse
gets the value of the inverse'

makeCacheMatrix <- function(x = matrix()) {
    inverseMat <- NULL
    
    # sets the value of the matrix
    set <- function(y) {
        x <<- y
        inverseMat <- NULL
    }
    
    # gets the value of the matrix
    get <- function() x
    
    # sets the value of the inverse
    setInverse <- function(Inverse = matrix()) {
        inverseMat <<- Inverse
    }
    
    # gets the value of the inverse
    getInverse <- function() {inverseMat
        }
    list(set= set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Function computing the matrix Inverse if it's not pre-calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    InverseM <- x$getInverse()
    if(!is.null(InverseM)) {
        print("Showing cached Inverse of the Matrix")
        InverseM
    } else {
        Data <- x$get()
        InverseM <- solve(Data)
        x$setInverse(InverseM)
        InverseM
    }
}

X <- matrix(rnorm(100,2,10), 10,10) # Normal matrix initialization
Mat <- makeCacheMatrix(X)           # Converting that matrix into a cached Matrix

# computing the Inverse of our special matrix here
cacheSolve(Mat)



