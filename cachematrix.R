## Function makeCacheMatrix creates a matrix and cache its inverse for future use.

makeCacheMatrix <- function(x = matrix()) {
    inv_mat <- NULL
    set <- function(y) {
        x <<- y
        inv_mat <<- NULL
    }
    
    get = function() x
    set_inv_mat = function(inverse) inv_mat <<- inverse
    get_inv_mat = function() inv_mat
    list(set = set, get = get, set_inv_mat = set_inv_mat, get_inv_mat = get_inv_mat)
}

## Function cacheSolve computes the ouput of function makeCacheMatrix according to two constraints:
## 1) If the matrix has been calculated, it retrieves the output and skips the computation;
## 2) Otherwise, it calculates the inverse of the data and computes the inverse matrix.

cacheSolve <- function(x, ...) { ## output of function MakeCacheMatrix
    inv = x$get_inv_mat() ## Return the inverse matrix of 'x'
    if (!is.null(inv_mat)) { ## if the inverse matrix has already been calculated
        message("cached data")
        return(inv_mat) ## retrieves it from the cache and skips the computation (saving time and processing) 
    }
    
    mat_data = x$get() ## otherwise, calculates the inverse matrix
    inv_mat = solve(mat_data, ...)
    x$set_inv_mat(inv_mat)
    return(inv_mat)
}
