
findDesc <- function(DBData, findData){
  fnd <- DBData$Trickers |> dplyr::filter( Tricker == findData)
  if( nrow(fnd)==1) {
    ret <- fnd$Description[1]
  }else
    ret <- findData
}

#' Title
#'
#' @param GroupName Ryhmän nimi
#' @param period  "days", "weeks", "months", "quarters", and "years"
#'
#' @return korrelaatiokuva
#' @export
#'
#' @examples
#' \dontrun{rep_showCompCorr("HOSKL", "weeks")}
#'
rep_showCompCorr<- function(GroupName=NULL, period="weeks"){
  DBData <- rep_readDB()
  sd<-load(getOption( "StockDataFile"))
  stockData <- as.data.frame(get(sd))
  #stockData |> dplyr::filter( ticker =="^OMXHCAPGI")

  df<-NULL
  stData <- stockData |> dplyr::transmute(date = ref_date,
                                Open = price_open,
                                High = price_high,
                                Low = price_low,
                                Close = price_close,
                                Portfolio = ticker)
  perf <- stData |> dplyr::filter( date > "2015-01-01")
  #perf <- retPortfolioResult(GroupName, period=period)
  my_df <- as.data.frame(perf)


  qdf<-NULL
  for( pf in unique( perf$Portfolio )) {
    #print( pf)
    porf<-my_df |> dplyr::filter( Portfolio == pf)
    qq<-xts::xts(porf[,'Close'], order.by=porf[,'date'])
    colnames(qq)<-"Close"
    qq<-qq |> xts::to.period(period = period)

    qqdf <- data.frame(date=zoo::index(qq), zoo::coredata(qq))
    qqdf$Portfolio = pf
    colnames(qqdf) <- c("date", "Open", "High", "Low", "Close", "Portfolio")
    Qf <- qqdf |> dplyr::mutate( date = format( qqdf$date, getOption("DateFormat")), Open=Open, High=High, Low=Low, Close=Close, Portfolio=Portfolio)
    qdf <- rbind(qdf, Qf)
  }

  perf <- qdf

  perff <- retPortfolioResult(GroupName, period=period)
  perf <- rbind(perf, perff)
  unique(perf$Portfolio)

  dff=NULL
  for( pf in unique(perf$Portfolio)){
    pp <- perf|>dplyr::filter( Portfolio == pf)
    qq<-xts::xts(pp['Close'],  order.by=as.Date( pp[,'date'], getOption("DateFormat")))
    qqRet <- PerformanceAnalytics::CalculateReturns(qq)
    if(is.null(dff))
      dff<-qqRet else
        dff <- merge( dff, qqRet)
    na<- colnames(dff)
    na[length(na)]<-pf
    colnames(dff) <- na
  }




  colMat <- NULL
  cn <- colnames(dff)
  ddd <- NULL
  # Täytyy tehdä näin koska sarakkeiden nimissä ei voi olla erikoismerkkejä
  colNames <- unique( perf$Portfolio )

for( i in 1:(length(colNames)-1)) {
  for( ii in (i+1):length(colNames)) {
    arv <-  merge( dff[,i], dff[,ii])
    arvv <- zoo::coredata(arv)
    dffl <- na.omit(arvv)
    co <- cor(dffl)
    dfff <- data.frame(
      Nimi = paste0("Korrelaatio ", colNames[i], " - ", colNames[ii]),
      Kuvaus = paste0("Korrelaatio ",
                      findDesc( DBData, colNames[i]),
                      " - ",
                      findDesc( DBData,colNames[ii])),
      Arvo = co[1,2]
    )
    ddd<- rbind( ddd, dfff)
  }
}



  ddd |> DT::datatable(filter='top', rownames = FALSE
                       ) |>
    DT::formatRound(columns=c("Arvo"),
                    digits=2, mark = "", dec.mark = ",")



}



#' Title
#'
#' @param GroupName Ryhmän nimi
#' @param period  "days", "weeks", "months", "quarters", and "years"
#'
#' @return korrelaatiokuva
#' @export
#'
#' @examples
#' \dontrun{rep_showCorr("HOSKL", "weeks")}
#'
rep_showCorr<- function(GroupName=NULL, PortfolioNames = NULL, StockNames = NULL,  period="weeks"){

  perf <- retPortfolioResult(
    GroupName = GroupName,
    PortfolioNames = PortfolioNames,
    StockNames = StockNames,
    period = period)




  dff=NULL
  for( pf in unique(perf$Portfolio)){
    pp <- perf|>dplyr::filter( Portfolio == pf)
    qq<-xts::xts(pp['Close'],  order.by=as.Date( pp[,'date'], getOption("DateFormat")))
    qqRet <- PerformanceAnalytics::CalculateReturns(qq)
    if(is.null(dff)) dff<-qqRet else dff <- merge( dff, qqRet)
    na<- colnames(dff)
    na[length(na)]<-pf
    colnames(dff) <- na
  }

  dff<-zoo::coredata(dff)
  dff <- na.omit(dff)



if(!is.null(dff) ){
  round(cor(dff), digits=4) |>
    echarts4r::e_charts() |>
    echarts4r::e_correlations(
      order = 'hclust',
      visual_map = FALSE
    ) |>
    echarts4r::e_visual_map(
      min = -1,
      max = 1
    )  |>
    echarts4r::e_labels(position = 'inside') |> echarts4r::e_tooltip()
}
}
