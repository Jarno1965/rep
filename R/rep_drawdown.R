



#' Title
#'
#' @return
#' @export
#'
#' @examples
rep_drawdown_sectors<-function(){
  sectors<-c("FSDPX", "FSRFX","FSHOX","FSAVX","FCYIX","FSRBX","FSELX","FSPTX","FSENX","FSPHX", "SPY")
  sdf <- getOption("StockDataFile")
  sd <- load(sdf)
  stockData <- get(sd)

  dfP<-NULL
  for( pf in sectors){
    dd<-stockData |> dplyr::filter(ticker == pf)
    df <- data.frame(date = dd$ref_date,
                      Portfolio = dd$ticker,
                      Close = dd$cumret_adjusted_prices)

    dfP<-rbind( dfP, df)
  }

  df<-NULL
  for( pf in sectors){
    li<-dfP |> dplyr::filter( Portfolio== pf)
    xtsli <- xts::xts(li$Close, order.by=as.Date(li$date))
    names(xtsli)<-c("Value")
    xtsliM <- xts::to.monthly(xtsli)

    rets <- PerformanceAnalytics::Return.calculate(xtsliM)
    ddli <- PerformanceAnalytics::Drawdowns(rets)
    dff <- data.frame(date=zoo::index(ddli), zoo::coredata(ddli), portfolio=pf)
    df<- rbind(df, dff)
  }



  df$D <- as.Date(ISOdate( lubridate::year(df$date), lubridate::month(df$date), lubridate::day(df$date)))


  valu <- min(df$xtsli.Low[!is.na(df$xtsli.Low)])*0.95
  df |> dplyr::group_by(portfolio) |> echarts4r::e_charts(D) |> echarts4r::e_line(xtsli.Low, symbol = "none") |> echarts4r::e_datazoom() |>
    echarts4r::e_tooltip(trigger = "axis", axisPointer = list(type = "cross")) |>
    echarts4r::e_toolbox_feature("dataZoom") |> echarts4r::e_theme(getOption("theme")) |> echarts4r::e_animation(duration = 1000) |>
    echarts4r::e_datazoom(y_index = 0, startValue = valu , type = "slider") |>
    echarts4r::e_grid(gridIndex = "1")|>
    echarts4r::e_y_axis(
      formatter = echarts4r::e_axis_formatter(style=c("percent"),
                                              digits=0
      ))


}






#' piirtää drawdown kuvan annetulle salkkuryhmälle
#'
#' @param GroupName ryhmän nimi
#' @param index indeksi, ei ole vielä koodattu
#'
#' @return palauttaa kuvan
#' @export
#'
#' @examples
rep_drawdown<-function( GroupName="HOSKL", index=NULL){
  portfolios <- retPorfolio(GroupName)

  por=NULL
  for( pf in portfolios) {
    pfResult <- paste0(getOption("PortfolioResults"), pf, ".RData")
    resn <- load(pfResult)
    pfolio <- get(resn)
    pff <- data.frame(date = pfolio$date,
               portfolio = pf,
               value = pfolio$End.Eq)
    por<-rbind(por, pff)

  }



  dfP <- data.frame(date = por$date,
             Portfolio = por$portfolio,
             Close = por$value)
  portfolios <-unique(dfP$Portfolio)
  df<-NULL
  for( pf in portfolios){
    li<-dfP |> dplyr::filter( Portfolio== pf)
    xtsli <- xts::xts(li$Close, order.by=as.Date(li$date, format=getOption("DateFormat")))
    names(xtsli)<-c("Value")
    rets <- PerformanceAnalytics::Return.calculate(xtsli)
    ddli <- PerformanceAnalytics::Drawdowns(rets)
    dff <- data.frame(date=zoo::index(ddli), zoo::coredata(ddli), portfolio=pf)
    df<- rbind(df, dff)
  }
  df <- df[!is.na(df$Value), ]

  valu <- min(df$Value[!is.na(df$Value)])*0.95
   df$date <- as.Date(df$date)
   df <- df[order(df$date),]
   df$date <-  format(df$date, format="%d.%m.%Y" )


  # df$date <-  format(df$date, format="%d.%m.%Y" )


  suppressWarnings({
  df |> dplyr::group_by(portfolio) |> echarts4r::e_charts(date) |> echarts4r::e_line(Value, symbol="none") |> echarts4r::e_datazoom() |>
    echarts4r::e_tooltip(trigger = "axis", axisPointer = list(type = "cross")) |>
    echarts4r::e_toolbox_feature("dataZoom") |> echarts4r::e_theme(getOption("theme")) |> echarts4r::e_animation(duration = 1000) |>
    echarts4r::e_datazoom(y_index = 0, startValue = valu , type = "slider") |>
    echarts4r::e_grid(gridIndex = "1")|>
    echarts4r::e_y_axis(
      formatter = echarts4r::e_axis_formatter(style=c("percent"),
                                              digits=0
      ))
  })
}
