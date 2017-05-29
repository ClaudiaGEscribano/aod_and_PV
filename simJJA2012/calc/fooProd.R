fooProd <- function(data, modeTrk = 'fixed', timePeriod = 'month'){
    ## Number of days
    n <- (length(data) - 1)/2
    lat <- data[1]
    g0 <- data[2:(n + 1)]
    Ta <- data[(n + 2):(2*n + 1)]
    BD <- zoo(data.frame(G0 = g0, Ta = Ta),
              order.by = tt)
    Prod <- prodGCPV(lat = lat,
                     modeRad = 'bdI',
                     sample='3 hours',
                     dataRad= list(lat = lat, file = BD),
                     keep.night=FALSE, modeTrk = 'fixed')
    switch(timePeriod,
           year = as.data.frameY(Prod)['Yf'],
           month = as.data.frameM(Prod)['Yf']
           )
}

## Falta definir bien el objeto zoo, por los pasos de tiempo etc. Quizás dónde empieza la serie etc.


## ## Example with a cell

## iCell <- 10000
## xG <- SISS[iCell]
## xT <- Tas[iCell]
## lat <- y[iCell]
## xx <- c(lat, xG, xT)
