#This function creates a vector, containig 4 functions (set and get value for matrix, set and get inverse matrix)

makeCacheMatrix <- function (x = matrix())
{
    #Creating an object with is used for calculations and assigning NULL value
    m<-NULL

    #Creating functions
    #setting the value of vector with 4 functions
    set <- function (y)
    {
        x<<-y
        m<-NULL
    }
    #getting the value of vector with 4 funcions
    get <- function ()
    {
        x
    }
    #setting inverse
    setSolve <- function (solve)
    {
        m<<-solve
    }
    #getting inverse
    getSolve <- function ()
    {
        m
    }
    list(set=set, get=get, setSolve=setSolve, getSolve=getSolve)
}


#This function calculates inverse matix (if it is not stoted in cache) or gets value from cache (if we have calculated it before)
cacheSolve <- function (x,...)
{
    m<-x$getSolve()
    #checking if inverse matrix was already calculated and getting its value from cache
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    #actual inverse matrix calculation (if there is no cache)
    data <- x$get()
    m<-solve(data,...)
    #store inverse matrix to cache
    x$setSolve(m)
    m
}