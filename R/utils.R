

#' Palauttaa salkkuryhmän tuloksen eri aikaperodeilla
#'
#' @param GroupName ryhmän nimi
#' @param period = days, weeks, months, quarters, years
retPortfolioResult<-function(GroupName = NULL, PortfolioNames = NULL, StockNames=NULL, period="days"){
  logger::log_info("retPortfolioResult<-function(GroupName={GroupName}, period={period}) ")
  perf <- rep_readDB()

  grpn = GroupName
  # if(is.null(GroupName)) {
  #   grpn <- "HOSKL"
  # } else {
  #     grpn<- GroupName}


if(!is.null(grpn)){

  perf <- perf$Groups |> dplyr::filter( Name==grpn)

  if(nrow(perf)==0){
    message(paste0("Ei löydy portfolio ryhmää tälle nimelle ", GroupName))
    return(NULL)
  }
  }else
  {
    perf <- NULL
  }

  por <-rpf(GroupName =grpn, PortfolioNames = PortfolioNames, StockNames = StockNames)

    pfolios<-unique(perf$Portfolio)

    pfolios <- append( pfolios, StockNames)
    pfolios <- append( pfolios, PortfolioNames)



    qdf<-NULL

    for( pf in pfolios){
      porf<-por|>dplyr::filter( Portfolio == pf)
      qq<-xts::xts(porf[,'End.Eq'], order.by=porf[,'date'])
      colnames(qq)<-"Close"

      qq<-qq |> xts::to.period(period = period)

      qqdf <- data.frame(date=zoo::index(qq), zoo::coredata(qq))
      qqdf$Portfolio = pf

      colnames(qqdf) <- c("date", "Open", "High", "Low", "Close", "Portfolio")
      Qf <- qqdf |> dplyr::mutate( date = format( qqdf$date, getOption("DateFormat")), Open=Open, High=High, Low=Low, Close=Close, Portfolio=Portfolio)

      qdf <- rbind(qdf, Qf)
    }

return(qdf)
}
retPorfolio<-function(GroupName){
  perf <- rep_readDB()
  perfo <- perf$Groups |> dplyr::filter( Name==GroupName)
  ret<- perfo$Portfolio
  return(ret)
}

#' Title
#'
#' @param PortfolioName
#' @param period
#' @param nullDay
#'
#' @return
#'
#' @examples
retComponents<-function(PortfolioName=NULL, period="days", nullDay = "2022-05-20"){
  logger::log_info("retComponents<-function(PortfolioName={PortfolioName}, period={period}, nullDay = {nullDay}) ")
  nullVal <- 10000
  pfName=PortfolioName
  perf <- rep_readDB()
  if(is.null(PortfolioName)) {pfName <- "HOSKL Unelmat"} else { pfName<- PortfolioName}
  # findstocks

  pfs <- perf$Portfolios |> dplyr::filter(PortfolioName==pfName)
  if(nrow(pfs)==0){
    message( paste0("Ei löydy osakesalkkua tuolla nimellä ", PortfolioName))
    return(NULL)}
   pft <- levels(factor(pfs$Tricker))
   pft <- pft[pft!="" & !is.null(pft)]

   levels(factor(pfs$Tricker))
   dd <- loadStocks(stockList=pft, nullDay=nullDay, nullVal=nullVal)
  #
   # sd<-load(getOption( "StockDataFile"))
   # stockData <- get(sd)
   # df<-NULL
  # pft<-append(pft, indexes)
   # for( i in 1:length(pft)){
   #   std <- stockData|> dplyr::filter(ticker==pft[i])
  #   price_adjusted
  #   ss<-std|>dplyr::filter(ref_date ==nullDay )
  #
  #   multip <- nullVal /ss$ret_cumulative
  #   std$Value <- multip*std$ret_cumulative
  #   df<-rbind(df, std)
  # }

  # dd<-data.frame(
  #   date = df$ref_date,
  #   Portfolio = df$ticker,
  #   Close = df$Value)
  return(dd)
}


#' Title
#'
#' @param stockList
#' @param nullDay
#' @param nullVal
#'
#' @return
#' @export
#'
#' @examples
loadStocks<-function(stockList, nullDay = "2022-05-20", nullVal=10000 ){
  sd<-load(getOption( "StockDataFile"))
  stockData <- get(sd)
  df<-NULL
  data <- rep_readDB()
  for( i in 1:length(stockList)){
    std <- stockData|> dplyr::filter(ticker==stockList[i])

    da <- data$Trickers |> dplyr::filter( Tricker == stockList[i])
    pp <- da$Description
    std$ticker <- pp
    #price_adjusted

    ss<-std|>dplyr::filter(ref_date ==nullDay )
    if( nrow(ss)==1){
      multip <- nullVal /ss$ret_cumulative
    } else{
      std$ref_date-as.Date(nullDay)
      ms <- min(abs(std$ref_date-as.Date(nullDay)))

      ss<-std|>dplyr::filter(abs(std$ref_date-as.Date(nullDay)) == ms  )
      multip <- nullVal /ss$ret_cumulative
    }
    std$Value <- multip*std$ret_cumulative
    std<-std[order(std$ref_date),]
    std$ticker <- stockList[i]

    df<-rbind(df, std)
  }
  dd<-data.frame(
    date = df$ref_date,
    Portfolio = df$ticker,
    Close = df$Value)

  return(dd)
}



rpf<-function(GroupName, StockNames = NULL, PortfolioNames = NULL){
  grpn<- GroupName
  df<-NULL
  # if(is.null(GroupName)) {
  #   grpn <- "HOSKL"}
  # else {
  #   grpn<- GroupName
  #   }
  perf <- rep_readDB()
  pfs<-NULL
  if(!is.null(grpn)){
    pfs<- perf$Groups |> dplyr::filter( Name==grpn)
    pfsu <- unique(pfs$Portfolio)

    df<-NULL
    for( pf in pfsu){
      pp<-load(paste0( getOption("PortfolioResults"), pf, ".RData") )
      pfolio<-get(pp)
      pfolio$Portfolio <- pf
      df <- rbind( df, pfolio)
    }
  }

  if(!is.null(PortfolioNames)){
    pfsu <- unique(PortfolioNames)

    for( pf in pfsu){
      pp<-load(paste0( getOption("PortfolioResults"), pf, ".RData") )
      pfolio<-get(pp)
      pfolio$Portfolio <- pf
      df <- rbind( df, pfolio)
    }
  }





  sd<-load(getOption( "StockDataFile"))
  stockData <- get(sd)
  emptyDf <- df[FALSE,]
  cn <- colnames(df)

  for( st in StockNames){
    dd <- stockData |> dplyr::filter(ticker == st)
    data <- data.frame(date = dd$ref_date, Portfolio = dd$ticker, End.Eq = dd$price_close)
    data <- add_cols( data, cn)
    df <- rbind( df, data)
  }


return(df)
}


plotPic <- function(df){
valu <- min(df$Close[!is.na(df$Close)])*0.95
echarts4r::e_charts() |>
  echarts4r::e_list(OO) |> echarts4r::e_datazoom(y_index = 0, startValue = valu , type = "slider") |>
  echarts4r::e_toolbox_feature("dataZoom") |> echarts4r::e_datazoom() |> echarts4r::e_theme(getOption("theme"))
}
