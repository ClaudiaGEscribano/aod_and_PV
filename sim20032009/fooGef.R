
fooGef <- function(data, modeTrk='fixed'){
    n <- (length(data) - 1)/2
    lat <- data[1]
    g0 <- data[2:(n + 1)]
    Ta <- data[(n + 2):(2*n + 1)]
    BD <- zoo(data.frame(G0 = g0, Ta = Ta),
              order.by = tt)
    gef <- calcGef(lat=lat,
                   modeRad='bd',
                   dataRad=list(lat=lat, file=BD),
                   modeTrk=modeTrk)
    result <-  as.data.frameY(gef)[c('Gefd')]
    as.numeric(result)
}
