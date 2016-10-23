## @summary - Stairflyer: paire de fonctions servant à l'optimisation du calcul de l'inverse
## d'une matrice. Cela passera par la création d'un objet spécifique ainsi que la mise en
## cache de l'inverse de cette matrice.

## @brief : creation d'une matrice pour servir de tampon à son inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        s <- function(y) {
                x <<- y
                inv <<- NULL
        }
        g <- function() x
        setInv <- function(inverse) inv <<- inverse
        getInv <- function() inv
        list(set = s,
             get = g,
             setInverse = setInv,
             getInverse = getInv)
}

## @brief : Determination de l'inverse de la matrice générée dans la fonction ci-dessus.
## Si l'inverse a déjà été calculé auparavant, on ne lance pas le recalcul.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(i)
        i
}
