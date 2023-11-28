
#' rep_Stat näyttää taulukkomuodossa komponenttien tilastoja
#'
#'
#' @return palauttaa taulukon html-muodossa
#' @export
#'
#' @examples rep_StatComp(groupName = "HOSKL)
rep_StatComp <- function(groupName = "HOSKL"){
  logger::log_info("rep_StatComp")
  portfolioInfo <- rep_readDB()

  grps <- portfolioInfo$Groups |> dplyr::filter( Name == groupName )



  dff <- NULL
  for( i in 1:length(grps$Portfolio) ) {
    df <- portfolioInfo$Stats |> dplyr::filter( Portfolio == grps$Portfolio[i])
    dff <- rbind( dff, df)
  }

  dff |> DT::datatable(filter='top', rownames = FALSE,
                       options = list(
                         autoWidth=TRUE,
                         columnDefs = list(list(width = '20%',
                                                targets = 2)
                         )
                       )
  )

  # dff |> DT::datatable(
  #    rownames = FALSE, options = list( pageLength=10,
  #
  #                                      bFilter=0,
  #                                      search = list(caseInsentive = FALSE),
  #                                      columnDefs = list(list(width = '40%',
  #                                                         targets = "_all")
  #                                                            ),
  #                                      style = "font-size: 75%; width: 75%"
  #   ),
  #   fillContainer = FALSE) |>
  #   DT::formatRound(columns=c('Hinta', "Summa", "div", "Total"), digits=2, mark = "", dec.mark = ",")
}


argTest <- function(GroupName = NULL,...){
  arguments <- list(...)
  browser()
}




#' rep_DividendData näyttää taulukkomuodossa komponenttien tilan. Jos GroupName ei määritellä niin se on HOSKL
#'
#'
#' @return palauttaa datan jolloin voidaan näyttää suoraan taulukko
#'   data |> DT::datatable(filter='top', rownames = FALSE,
#'   options = list(
#'     autoWidth=TRUE
#'   )
#'   ) |>
#'     DT::formatRound(columns=c('Lkm', 'Hinta', "Summa"),
#'                     digits=2, mark = "", dec.mark = ",")|>
#'     DT::formatRound(columns=c("Saldo"),
#'                     digits=0, mark = "", dec.mark = ",")
#' @export
#'
#' @examples rep_DividendData()
rep_DividendData <- function(GroupName = NULL, ...){
  logger::log_info("rep_DividendData")

  # arguments <- "OPLongShort"


  portfolioInfo <- rep_readDB()

  sdf <- getOption("StockDataFile")
  sd <- load(sdf)
  stockData <- get(sd)
  pg <- NULL
  if( !is.null(GroupName)){
    pg <- portfolioInfo$Groups |> dplyr::filter( Name==GroupName)
  }

  arguments <- list(...)

  addPfsNbr <- length(arguments)
  pgs <- pg$Portfolio
  if( length(arguments)>0){
    for(i in 1:length(arguments)){
      pgs <- append(pgs, arguments[[i]] )
    }
  }
  logger::log_info("Portfoliot pgs: {pgs}")
  ppp<-portfolioInfo$Portfolios|>dplyr::group_by(PortfolioName)
  pp<-portfolioInfo$Portfolios|>dplyr::filter(PortfolioName %in%   pgs)

  dff<-NULL
  ##################################

  pfNbr <- 1
  for(pfNbr in 1:length(pgs)){
    pfName <- pgs[pfNbr]
    #Portfolios$Tricker
    pfs <- portfolioInfo$Portfolios |> dplyr::filter(PortfolioName == pfName)
    trs <- unique(pfs$Tricker)
    trs <- trs[!is.na(trs)]
    trs <- trs[trs!=""]
    for( nn in 1:length(trs)){
      #trs[1] <- "KEMPOWR.HE"
      logger::log_info(paste0("Katsotaan arvot ", trs[nn]))
      tr <- trs[nn]
      anal <- pp |>dplyr::filter( PortfolioName == pfName & Tricker == tr)
      Taulukko <- NULL
      # Taulukko$Salkku[1] <- "HOSKL Unelmat"
      # Taulukko$Trikkeri <- "UPM.HE"

      pvm <- NULL
      for(i in 1:nrow(anal)){
        Taulukko$Salkku[i] <- pfName
        Taulukko$Trikkeri[i] <- tr
        Taulukko$Nimi[i] <- pp$Description
        type <- "UNDEFINED"
        if( anal[i,]$Qnt > 0){
          type <- "BUY"
          Taulukko$Hinta[i] <- anal[i,]$Price
          Taulukko$Lkm[i] <- anal[i,]$Qnt
          Taulukko$Summa[i] <- -Taulukko$Hinta[i]*Taulukko$Lkm[i]
          Taulukko$Pvm[i] <- lubridate::dmy(anal[i,]$date)
          if( is.null (pvm)) {
            pvm <- lubridate::dmy(anal[i,]$date)
          }
        }
        if( anal[i,]$Qnt < 0){
          type <- "SELL"
          Taulukko$Hinta[i] <- anal[i,]$Price
          Taulukko$Lkm[i] <- anal[i,]$Qnt
          Taulukko$Summa[i] <- -Taulukko$Hinta[i]*Taulukko$Lkm[i]
          Taulukko$Pvm[i] <- lubridate::dmy(anal[i,]$date)
          if( is.null (pvm)) {
            pvm <- lubridate::dmy(anal[i,]$date)
          }
        }
        Taulukko$Tyyppi[i] <- type
      }
      df <- data.frame(Taulukko)
      df$div <-  0
      # df$divTotal <- 0
      # Lisätään osingot
      trDiv <- stockData |> dplyr::filter(div != 0 & ticker == tr)
      if(nrow(trDiv) > 0) {
        for( i in 1:nrow(trDiv)){
          if( trDiv[i,]$ref_date >= pvm){
            df <- tibble::add_row(df)
            df[nrow(df),]$Salkku <- pfName
            df[nrow(df),]$Trikkeri <- tr
            df[nrow(df),]$Nimi <- pp$Description
            df[nrow(df),]$Pvm <- trDiv[i,]$ref_date
            df[nrow(df),]$div <- trDiv[i,]$div
            df[nrow(df),]$Tyyppi <- "DIV"
          }

        }
      }

      df <- df[order(df$Pvm),]
      df$div <- tidyr::replace_na( df$div, 0)
      df$Lkm <- tidyr::replace_na( df$Lkm, 0)
      df$Total <- cumsum(df$Lkm)
      for( i in 1:nrow(df)){
        if( df[i,]$Tyyppi == "DIV"){
          df[i,]$Summa <- df[i,]$Total * df[i,]$div
        }
      }



      df <- df |> dplyr::mutate(  Pvm = as.Date(df$Pvm))
      cols <- c("pvm", "Trikkeri", "pvm", "Tyyppi", "Lkm", "Hinta", "div", "Total")
      cols <- c("Pvm", "Salkku", "Tyyppi", "Trikkeri", "Lkm", "Hinta")
      data.table::setcolorder(df, cols)
      dff <- rbind( dff, df)
    }
  }
  #####################################


  #dff$Salkku <- stringr::str_replace( dff$Salkku, "HOSKL", "")
  name <- dff$Trikkeri

  for( i in 1:length(name)){
    pp <- portfolioInfo$Trickers |> dplyr::filter( Tricker == name[i])
    if( nrow(pp) == 1){
      name[i] <- pp$Description
      dff$Trikkeri[i] <- pp$Description
    }
  }

  # Lisätään lopun käteisarvot
  dffF <- addCashAmount( dff) # PANO, NOSTO, RAHAA (osingot, viimeinen tilanne)
  dffF$Salkku <- stringr::str_replace( dffF$Salkku, "HOSKL", "")
  # Vaihdetaan Total sarakkeen nimi saldoksi selkeyden vuoksi
  cols <- colnames(dffF)
  cols <- stringr::str_replace( cols, "Total", "Saldo")
  colnames(dffF) <- cols
  dffF$div <- NULL
  dffF$Pvm <- format(as.Date(dffF$Pvm), "%y.%m.%d")
  return(dffF)

  #
  # dffF |> DT::datatable(filter='top', rownames = FALSE,
  #                       options = list(
  #                         autoWidth=TRUE
  #                       )
  # ) |>
  #   DT::formatRound(columns=c('Lkm', 'Hinta', "Summa"),
  #                   digits=2, mark = "", dec.mark = ",")|>
  #   DT::formatRound(columns=c("Saldo"),
  #                   digits=0, mark = "", dec.mark = ",")


  # dff |> DT::datatable(
  #    rownames = FALSE, options = list( pageLength=10,
  #
  #                                      bFilter=0,
  #                                      search = list(caseInsentive = FALSE),
  #                                      columnDefs = list(list(width = '40%',
  #                                                         targets = "_all")
  #                                                            ),
  #                                      style = "font-size: 75%; width: 75%"
  #   ),
  #   fillContainer = FALSE) |>
  #   DT::formatRound(columns=c('Hinta', "Summa", "div", "Total"), digits=2, mark = "", dec.mark = ",")
}


#' rep_Dividend näyttää taulukkomuodossa komponenttien tilan
#'
#'
#' @return palauttaa taulukon html-muodossa
#' @export
#'
#' @examples rep_Dividend()
rep_Dividend <- function(...){
  logger::log_info("rep_Dividend")

# arguments <- "OPLongShort"


  portfolioInfo <- rep_readDB()

  sdf <- getOption("StockDataFile")
  sd <- load(sdf)
  stockData <- get(sd)

  pg <- portfolioInfo$Groups |> dplyr::filter( Name=="HOSKL")
  arguments <- list(...)
  addPfsNbr <- length(arguments)
  pgs <- pg$Portfolio
  for(i in 1:length(arguments)){
    pgs <- append(pgs, arguments[[i]] )
  }

  ppp<-portfolioInfo$Portfolios|>dplyr::group_by(PortfolioName)
  pp<-portfolioInfo$Portfolios|>dplyr::filter(PortfolioName %in%   pgs)

  dff<-NULL
##################################

  pfNbr <- 1
for(pfNbr in 1:length(pgs)){
  pfName <- pgs[pfNbr]
  #Portfolios$Tricker
  pfs <- portfolioInfo$Portfolios |> dplyr::filter(PortfolioName == pfName)
  trs <- unique(pfs$Tricker)
  trs <- trs[!is.na(trs)]
  trs <- trs[trs!=""]
for( nn in 1:length(trs)){
  #trs[1] <- "KEMPOWR.HE"
  logger::log_info(paste0("Katsotaan arvot ", trs[nn]))
  tr <- trs[nn]
  anal <- pp |>dplyr::filter( PortfolioName == pfName & Tricker == tr)
  Taulukko <- NULL
  # Taulukko$Salkku[1] <- "HOSKL Unelmat"
  # Taulukko$Trikkeri <- "UPM.HE"

  pvm <- NULL
for(i in 1:nrow(anal)){
    Taulukko$Salkku[i] <- pfName
    Taulukko$Trikkeri[i] <- tr
    Taulukko$Nimi[i] <- pp$Description
      type <- "UNDEFINED"
      if( anal[i,]$Qnt > 0){
        type <- "BUY"
        Taulukko$Hinta[i] <- anal[i,]$Price
        Taulukko$Lkm[i] <- anal[i,]$Qnt
        Taulukko$Summa[i] <- -Taulukko$Hinta[i]*Taulukko$Lkm[i]
        Taulukko$Pvm[i] <- lubridate::dmy(anal[i,]$date)
        if( is.null (pvm)) {
          pvm <- lubridate::dmy(anal[i,]$date)
            }
      }
      if( anal[i,]$Qnt < 0){
        type <- "SELL"
        Taulukko$Hinta[i] <- anal[i,]$Price
        Taulukko$Lkm[i] <- anal[i,]$Qnt
        Taulukko$Summa[i] <- -Taulukko$Hinta[i]*Taulukko$Lkm[i]
        Taulukko$Pvm[i] <- lubridate::dmy(anal[i,]$date)
        if( is.null (pvm)) {
          pvm <- lubridate::dmy(anal[i,]$date)
        }
            }
      Taulukko$Tyyppi[i] <- type
  }
  df <- data.frame(Taulukko)
  df$div <-  0
  # df$divTotal <- 0
   # Lisätään osingot
   trDiv <- stockData |> dplyr::filter(div != 0 & ticker == tr)
   if(nrow(trDiv) > 0) {
   for( i in 1:nrow(trDiv)){
     if( trDiv[i,]$ref_date >= pvm){
       df <- tibble::add_row(df)
       df[nrow(df),]$Salkku <- pfName
       df[nrow(df),]$Trikkeri <- tr
       df[nrow(df),]$Nimi <- pp$Description
       df[nrow(df),]$Pvm <- trDiv[i,]$ref_date
       df[nrow(df),]$div <- trDiv[i,]$div
       df[nrow(df),]$Tyyppi <- "DIV"
     }

   }
   }

  df <- df[order(df$Pvm),]
  df$div <- tidyr::replace_na( df$div, 0)
  df$Lkm <- tidyr::replace_na( df$Lkm, 0)
  df$Total <- cumsum(df$Lkm)
  for( i in 1:nrow(df)){
    if( df[i,]$Tyyppi == "DIV"){
       df[i,]$Summa <- df[i,]$Total * df[i,]$div
    }
  }



  df <- df |> dplyr::mutate(  Pvm = as.Date(df$Pvm))
  cols <- c("pvm", "Trikkeri", "pvm", "Tyyppi", "Lkm", "Hinta", "div", "Total")
  cols <- c("Pvm", "Salkku", "Tyyppi", "Trikkeri", "Lkm", "Hinta")
  data.table::setcolorder(df, cols)
  dff <- rbind( dff, df)
}
}
#####################################


  #dff$Salkku <- stringr::str_replace( dff$Salkku, "HOSKL", "")
  name <- dff$Trikkeri

for( i in 1:length(name)){
  pp <- portfolioInfo$Trickers |> dplyr::filter( Tricker == name[i])
  if( nrow(pp) == 1){
    name[i] <- pp$Description
    dff$Trikkeri[i] <- pp$Description
  }
}

  # Lisätään lopun käteisarvot
  dffF <- addCashAmount( dff) # PANO, NOSTO, RAHAA (osingot, viimeinen tilanne)
  dffF$Salkku <- stringr::str_replace( dffF$Salkku, "HOSKL", "")
  # Vaihdetaan Total sarakkeen nimi saldoksi selkeyden vuoksi
  cols <- colnames(dffF)
  cols <- stringr::str_replace( cols, "Total", "Saldo")
  colnames(dffF) <- cols
  dffF$div <- NULL
  dffF$Pvm <- format(as.Date(dffF$Pvm), "%y.%m.%d")
dffF |> DT::datatable(filter='top', rownames = FALSE,
                     options = list(
                       autoWidth=TRUE
                     )
                     ) |>
    DT::formatRound(columns=c('Lkm', 'Hinta', "Summa"),
                    digits=2, mark = "", dec.mark = ",")|>
  DT::formatRound(columns=c("Saldo"),
                  digits=0, mark = "", dec.mark = ",")


 # dff |> DT::datatable(
 #    rownames = FALSE, options = list( pageLength=10,
 #
 #                                      bFilter=0,
 #                                      search = list(caseInsentive = FALSE),
 #                                      columnDefs = list(list(width = '40%',
 #                                                         targets = "_all")
 #                                                            ),
 #                                      style = "font-size: 75%; width: 75%"
 #   ),
 #   fillContainer = FALSE) |>
 #   DT::formatRound(columns=c('Hinta', "Summa", "div", "Total"), digits=2, mark = "", dec.mark = ",")
}

addCashAmount <- function( dff){
ddd <- dff

  pfs <- levels(factor(dff$Salkku))
  empty <- dff[0,]
  i=5 # Rikkaaksi
  for(i in 1:length(pfs)){

    pfName <- paste0( options("PortfolioResults"), pfs[i], ".RData")
    an <- load(pfName)
    pf <- get(an)
    additions <- pf |> dplyr::filter(Additions != 0)
    withdrawls <- pf |> dplyr::filter(Withdrawals != 0)
    RealizedPL <- pf |> dplyr::filter(Realized.PL != 0)
    dfDa <- dff |> dplyr::filter(Salkku == pfs[i])

    ii <- 1
    emptyD <- dfDa[0,]
    while ( ii <= nrow(additions)){
      emptyD <- tibble::add_row(emptyD)
      emptyD$Pvm[ii] <- additions$date[ii]
      emptyD$Salkku[ii] <- pfs[i]
      emptyD$Tyyppi[ii] <- "PANO"
      emptyD$Summa[ii] <- additions$Additions[ii]

      ii <- ii+1
    }
    dfDa <- rbind( dfDa, emptyD)

    ii <- 1
    emptyD <- dfDa[0,]
    while ( ii <= nrow(withdrawls)){
      emptyD <- tibble::add_row(emptyD)
      emptyD$Pvm[ii] <- withdrawls$date[ii]
      emptyD$Salkku[ii] <- pfs[i]
      emptyD$Tyyppi[ii] <- "NOSTO"
      emptyD$Summa[ii] <- -withdrawls$Withdrawals[ii]

      ii <- ii+1
    }
    dfDa <- rbind( dfDa, emptyD)

    ii <- 1
    emptyD <- dfDa[0,]
    # Katsotaan kaikki osingot
    #pfData <- portfolioInfo$Portfolios |> dplyr::filter( PortfolioName == pfs[i])

    #trick <- levels(factor(pfData$Tricker))


    # while ( ii <= nrow(RealizedPL)){
    #   emptyD <- tibble::add_row(emptyD)
    #   emptyD$pvm[ii] <- RealizedPL$date[ii]
    #   emptyD$Salkku[ii] <- pfs[i]
    #   emptyD$Tyyppi[ii] <- "TULOS"
    #   emptyD$Summa[ii] <- RealizedPL$Realized.PL[ii]
    #
    #   ii <- ii+1
    # }
    #
    # dfDa <- rbind( dfDa, emptyD)

dfDa <- dfDa[order (dfDa$Pvm),]
dfDa$Total <- cumsum( dfDa$Summa)
dff <- dff |> dplyr::filter(Salkku != pfs[i])
dff <- rbind(dff, dfDa)
#cumsum(dfs$Summa)


#Lasketaan viimeisen päivän käteisen määrä
# Lasketaan panot yhteen
#     kateinen <- 0
#     panot <- dff |> dplyr::filter( Tyyppi == "PANO" & Salkku==pfs[i])
#
#
#     if( nrow(panot)>0 ){
#       ps <- cumsum(panot$Summa)
#       kateinen <- ps[nrow(panot)]
#     }
# # Vähennetään (mahdolliset) nostot
#     nostot <- dff |> dplyr::filter( Tyyppi == "NOSTO" & Salkku==pfs[i])
#
#
#     if( nrow(nostot)>0 ){
#       kateinen <- kateinen + nostot$Summa[nrow(nostot)]
#     }
# # Vähennetään ostoihin käytetty rahamäärä
#     ostot <- dff |> dplyr::filter( Tyyppi == "BUY" & Salkku==pfs[i])
#     if( nrow(ostot)>0 ){
#       ost <- cumsum(ostot$Summa)
#       kateinen <- kateinen + ost[length(ost)]
#     }
#
# # Lisätään osingot
#   dffDiv <- dff |> dplyr::filter( Tyyppi == "DIV" & Salkku==pfs[i])
#   if( nrow(dffDiv)>0 ){
#     cumDiv <- cumsum(dffDiv$Summa)
#     kateinen <- kateinen + cumDiv[length(cumDiv)]
#   }
#
#   logger::log_info(paste0("rep_Dividend ", pfs[i] ))
#   logger::log_info(paste0("kateinen ", kateinen ))
#   pv <- sort(dff$pvm)
#   pvm <- pv[length(pv)]
#   dff <- tibble::add_row(dff)
#   dff$pvm[nrow(dff)] <- pvm
#   dff$Salkku[nrow(dff)] <- pfs[i]
#   dff$Tyyppi[nrow(dff)] <- "KÄTEINEN"
#   dff$Summa[nrow(dff)] <- kateinen

  } #for(i in 1:length(pfs))


  return(dff)
}

#' @export
#'
#' @examples rep_plotly()
rep_plotly <- function(){
  fig <- plotly::plot_ly(ggplot2::midwest, x = ~percollege, color = ~state, type = "box")
  fig
}


#' rep_StartingState näyttää taulukkomuodossa salkkujen alkutilan
#'
#'
#' @return palauttaa taulukon html-muodossa
#' @export
#'
#' @examples rep_StartingState()
rep_StartingState <- function(){
  logger::log_info("rep_StartingState")
  portfolioInfo <- rep_readDB()


  pg<-portfolioInfo$Groups |> dplyr::filter( Name=="HOSKL")
  ppp<-portfolioInfo$Portfolios|>dplyr::group_by(PortfolioName)
  pp<-portfolioInfo$Portfolios|>dplyr::filter(PortfolioName %in%   pg$Portfolio)
  pp <- pp[,!names(pp) %in% c("CloseVal", "Total")]
  names <- colnames(pp)
  names <- stringr::str_trunc(names, 5, "right", ellipsis = "")
  colnames(pp)<-names
  # DT::datatable(
  #   pp,  rownames = FALSE, options = list( pageLength=6, lengthChange = FALSE,
  #                                          bFilter=0,
  #                                          columnDefs = list(list(width = '50%', targets = "_all"))
  #   ),
  #   fillContainer = FALSE)



  pp |> DT::datatable(filter='top', rownames = FALSE,
                       options = list(
                         autoWidth=TRUE,
                         columnDefs = list(list(width = '20%',
                                                targets = 2)
                         )
                       )
  ) |>
    DT::formatRound(columns=c('Qnt', "Price"),
                  digits=2, mark = "", dec.mark = ",")

}
