## data is a vector concatenating the latitude value, the vector of 
## irradiation values, and the vector of temperature values. See
## example below
fooProd <- function(data, modeTrk = 'two', timePeriod = 'month'){
    ## Number of days
    n <- (length(data) - 1)/2
    lat <- data[1]
    g0 <- data[2:(n + 1)]
    Ta <- data[(n + 2):(2*n + 1)]
    BD <- zoo(data.frame(G0 = g0, Ta = Ta),
              order.by = tt)
    Prod <- prodGCPV(lat = lat,
                     modeRad = 'bd',
                     dataRad= list(lat = lat, file = BD),
                     keep.night=FALSE, modeTrk = modeTrk)
    switch(timePeriod,
           year = as.data.frameY(Prod)['Yf'],
           month = as.data.frameM(Prod)['Yf']
           )
}

## ## Example with a cell

## iCell <- 10000
## xG <- SISS[iCell]
## xT <- Tas[iCell]
## lat <- y[iCell]
## xx <- c(lat, xG, xT)

## xProd <- fooProd(xx, timePeriod = 'year')
## xProd <- fooProd(xx, timePeriod = 'month')
