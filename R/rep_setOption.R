

#' Asettaa perusasetukset
#'
#' @param baseDir perushakemisto. Jos tyhjä käyttää työhakemistoa
#'
#' @return Jos kaikki OK niin ei palauta mitään
#' @export
#'
#' @examples rep_setOption("F:\\R codes\\Salkku1\\")
rep_setOption <- function(baseDir=NULL){

  if( is.null(baseDir)) baseDir <- "Data\\" else {
    options(baseDir = baseDir)
  }

  options(DateFormat = "%d.%m.%Y")
  options(DateFormatRH = "DD.MM.YY")
  options(theme = "roma")
  options(DatabaseFile = paste0(baseDir, "Data\\Datas.db"))
  options( StockDataFile= paste0(baseDir,"Data\\stockData.RData"))
  options( PortfolioResults= paste0(baseDir, "Data\\"))
}
