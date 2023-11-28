


#' Title
#'
#' @return Palauttaa lista-muodossa vastauksen
#'
#' @export
#'
#' @examples{
#' \dontrun{rep_readDB()}
#'}
rep_readDB <- function(){

  if( is.null(getOption("DatabaseFile"))){
    rep_setOption("F:\\R codes\\Salkku1\\")
    }

  logger::log_info("readDB<-function() ", getOption("DatabaseFile"))

  mydb <- DBI::dbConnect(RSQLite::SQLite(), getOption("DatabaseFile"))
  Portfolios <- DBI::dbReadTable(mydb, "Portfolios")
  Portfolios$CloseVal = 1000
  suppressWarnings(Portfolios$date<- format(as.Date(Portfolios$date), getOption("DateFormat")))
  Portfolio <- DBI::dbReadTable(mydb, "Portfolio")
  Groups <- DBI::dbReadTable(mydb, "Groups")
  HandlingData <- DBI::dbReadTable(mydb, "HandlingData")
  Trickers <- DBI::dbReadTable(mydb, "Trickers")
  Stats <- DBI::dbReadTable(mydb, "Stats")
  if( nrow(Trickers)>0){
    suppressWarnings(Trickers$FirstData <- format( as.Date(as.numeric(Trickers$FirstData), origin = "1970-01-01"), getOption("DateFormat")))
    suppressWarnings(Trickers$LastUpdate <- format( as.Date(as.numeric(Trickers$LastUpdate), origin = "1970-01-01"), getOption("DateFormat")))
    # browser()
    #Portfolios$date <- format( as.Date(as.numeric(Portfolios$date), origin = "1970-01-01"), getOption("DateFormat"))
  }else {
    Trickers[1,] <- NA
  }


  DBI::dbDisconnect(mydb)

  ret<- list(Portfolios=Portfolios,  Portfolio = Portfolio,
             Groups = Groups, Trickers = Trickers,
             HandlingData = HandlingData, Stats = Stats)
  return(ret)
}



