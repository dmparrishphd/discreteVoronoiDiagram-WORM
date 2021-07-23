discreteVoronoiDiagram <- function ( X ) {

    NROW <- nrow ( X )

    ROW.NOS <- seq_len ( NROW )

    NEARNESS <- X [ cbind ( ROW.NOS , max.col (
        - X , ties.method = "first" ) ) ]

    equidistant <- function ( i )
            X [ i , ] == NEARNESS [[ i ]]

    NNEAREST <- max ( vapply (
        X = ROW.NOS ,
        USE.NAMES = FALSE ,
        FUN.VALUE = integer ( 1 ) ,
        FUN = function ( i ) sum ( equidistant ( i ) ) ) )

    Zones <- matrix (
        data = NA_integer_ ,
        nrow = NROW ,
        ncol = NNEAREST )

    for ( i in ROW.NOS ) {
        Which <- which ( equidistant ( i ) )
        Zones [ i , seq_along ( Which ) ] <- Which }

    Zones }
