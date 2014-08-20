makeVector <- function(x = numeric()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() {
            x
      }
      setMean <- function(calculated_mean) {
            m <<- calculated_mean
      }
      getMean <- function() {
            m
      }
      list(set = set, get = get,
           setmean = setMean,
           getmean = getMean)
}

cachemean <- function(x, ...) {
      m <- x$getmean()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- mean(data, ...)
      x$setmean(m)
      m
}