#' @export
StdDev  <- PerformanceAnalytics::StdDev

#' @export
VaR <- PerformanceAnalytics::VaR

#' @export
ES <- PerformanceAnalytics::ES


#' Title
#'
#' @param GroupName
#'
#' @return
#' @export
#'
#' @examples
rep_summary<- function(GroupName = "HOSKL"){
  perf <- retPortfolioResult(GroupName)
  portfolios <- unique( perf$Portfolio)
  xx<-NULL
  pf <- portfolios[4]
  for( pf in portfolios){
    pff <- perf|>dplyr::filter(Portfolio == pf)
    pff$Portfolio<-NULL
    xpff <- xts::xts(pff[,-1],  order.by=as.Date(pff$date, format = getOption("DateFormat")))
    na<-colnames(xpff)
    xpff <- xts::to.daily(xpff)
    #xpff <- xts::to.weekly(xpff)
    colnames(xpff)<-na
    cor_xpff <- zoo::coredata(xpff)


    dfx <- data.frame( date=zoo::index(xpff),
                       Close=zoo::coredata(xpff$Close),
                       Portfolio = pf
                       )

    xx <- rbind( xx, dfx)

  }


  perf <- xx
  portfolios= unique(perf$Portfolio)


  #perf|>split(perf$Portfolio)|>map(sd(perf$Close))
  tb <- tibble::tibble(Operaatio=c("Keskiarvo", "Mediaani"))

  pp<- "HOSKL Unelmat"
  tb <- tibble::tibble( Nimi=c("KA"))
  #tb<-data.frame( Nimi=c("KA"))
  # aa = c(12,54)
  # tibble::add_column(tb, "{pp}" := aa)
  pf <- portfolios[1]
  for(pf in portfolios ){
    data <- perf |> dplyr::filter(Portfolio==pf)
    data <- data |> dplyr::transmute(date = date, Value = Close)
    dataXTS <- data |> xts::xts(Value, order.by = as.Date(data$date, format = getOption("DateFormat")) )


    return <- PerformanceAnalytics::Return.calculate(data)
    ##na = c("KA", "Q 0%", "" )
    qnames<- "KA"

    value <- round( mean(return$Value, na.rm = TRUE)*100, digits=2)
    qnames <- append( qnames, "SD")
    value <- append( value, round( sd(return$Value, na.rm = TRUE)*100*sqrt(250), digits=2))

    q<- quantile(return$Value, probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.9, 1), na.rm = TRUE)
    qn <- names(q)
    qval <- round(unname(q)*100, digits=2)
    for( qnn in 1:length(qn)){
      #qn[qnn] <- paste( "Q",qn[qnn])
      qnames <- append( qnames, paste( "Q",qn[qnn]) )
      value <- append( value, qval[qnn] )
    }
    retu <- PerformanceAnalytics::Return.calculate(data)
    xd <- xts::xts(zoo::coredata(retu) ,order.by = as.Date(rownames(retu)))
    i=1
    Rf <- 0.0
    for(i in 1:3 ){
      SharpeR <- PerformanceAnalytics::SharpeRatio(xd, Rf <- Rf)
      rn <- rownames(SharpeR)
      qnames <- append( qnames, rn[1] ) #StdDev Sharpe
      value <- append( value, round(SharpeR[1,], digits=3) )
      Rf <- Rf + 0.015
    }
    # qnames <- append( qnames, rn[2] ) #VaR Sharpe (Rf=0%, p=95%)
    # value <- append( value, round(SharpeR[2,], digits=4) )
    # qnames <- append( qnames, rn[3] ) #ES Sharpe (Rf=0%, p=95%)
    # value <- append( value, round(SharpeR[3,], digits=4) )



    assign( pf, value)

     }


  tb <- tibble::tibble( Lasku = qnames)
  for(pf in portfolios ){
    tb <- tibble::add_column(tb, "{pf}" := get(pf))
  }
  DT::datatable(tb, rownames=FALSE, options = list(pageLength=9))

  #https://www.youtube.com/watch?v=oRWHEnJQgdI
}
