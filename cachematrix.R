# The following functions can be used to cache the inverse of a matrix. As
# matrix inversion is usually a costly computation, there may be some
# benefit to caching the inverse of a matrix rather than computing it
# repeatedly.

# makeCacheMatrix: creates a special "matrix" that can cache its inverse.
# Returns a list containing functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the inverse of the matrix
# 4. Get the inverse of the matrix
# It is assumed that the given matrix is always invertible.
makeCacheMatrix <- function(x = matrix()) {
    # x: an invertible matrix

    # c: inverse cache
    c <- NULL

    # Accessor, mutator for matrix
    set <- function(y) {
        x <<- y
        c <<- NULL
    }
    get <- function() {
        x
    }

    # Accessor, mutator for inverse
    setSolve <- function(solve) {
        c <<- solve
    }
    getSolve <- function() {
        c
    }

    # Return the accessors, mutators as a list
    list(set = set,
         get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


# cacheSolve: calculates the inverse of a special "matrix" created using the
# makeCacheMatrix function. If a cached inverse is available, the computation is
# skipped.
# Returns a matrix which is the inverse of the given special "matrix".
# It is assumed that the given matrix is always invertible.
cacheSolve <- function(x, ...) {
    # x: a special "matrix" created using makeCacheMatrix()

    # Get the cached inverse, if any
    inv <- x$getSolve()

    # If cache is not available, compute and store the inverse
    if (is.null(inv)) {
        inv <- solve(x$get(), ...)  # Assuming that solve() will succeed
        x$setSolve(inv)
    }

    # Return the inverse
    inv
}
