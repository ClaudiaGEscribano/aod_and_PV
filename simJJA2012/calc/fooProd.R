fooProd <- function(data, modeTrk = 'two', timePeriod = 'day'){
    ## Number of days
    n <- (length(data) - 1)/2
    lat <- data[1]
    g0 <- data[2:(n + 1)]
    Ta <- data[(n + 2):(2*n + 1)]
    BD <- zoo(data.frame(G0 = g0, Ta = Ta),
              order.by = tt)
    Prod <- prodGCPV(lat = lat,
                     modeRad = 'bdI',
                     dataRad= list(lat = lat, file = BD),
                     keep.night=TRUE, modeTrk = 'fixed')
    switch(timePeriod,
           year = as.data.frameY(Prod)['Yf'],
           month = as.data.frameM(Prod)['Yf'],
           day= as.data.frameD(Prod)['Yf']
           )
}

## ## Example with a cell

#iCell <- 10000
#xG <- SISS[iCell]
#xT <- Tas[iCell]
#lat <- lat[iCell]
#xx <- c(lat, xG, xT)

#fooProd(xx, 'daily')
