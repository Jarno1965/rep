

loadMont<-function(sectors, nullDay="2000-01-03", nullVal=10000){
  sd<-load(getOption( "StockDataFile"))
  stockData <- get(sd)
  df<-NULL


  for( i in 1:length(sectors)){
    std <- stockData|> dplyr::filter(ticker==sectors[i])
    #price_adjusted
    ss<-std|>dplyr::filter(ref_date ==nullDay )

    multip <- nullVal /ss$ret_cumulative
    std$Value <- multip*std$ret_cumulative
    df<-rbind(df, std)
  }

  dd<-data.frame(
    ticker = df$ticker,
    date = df$ref_date,
    Value = df$Value)

  df<-NULL
  for( pf in sectors){
    li<-dd |> dplyr::filter( ticker== pf)
    xtsli <- xts::xts(li$Value, order.by=as.Date(li$date))
    names(xtsli)<-c("Value")
    xtsliM <- xts::to.monthly(xtsli)
    dff <- data.frame(
      date = zoo::index(xtsliM),
      Portfolio = pf,
      Value = xtsliM$xtsli.Close
    )
    names(dff)<-c("date", "Portfolio", "Close")

    df<- rbind(df, dff)
  }


  df$date<-as.Date(ISOdate( lubridate::year(df$date), lubridate::month(df$date), lubridate::day(df$date)))
return(df)
}

startStruct<-function(xax){
  OO <- list(
    legend =list(
      show=TRUE
    ),


    xAxis = list(
      type = "category",
      data = xax
    ),
    yAxis = list(
      type = "value"

    ),
    tooltip=list(
      valueFormatter = htmlwidgets::JS("(value) => Number(value).toFixed(0)"),
      # formatter = htmlwidgets::JS("function(params){
      #     return(params)
      #   }"),
      trigger='axis',
      axisPointer=list(
        type='cross',
        label=list(
          backgroundColor= '#6a7985'
        )
      )
    )


  )
  return(OO)
}

#' Title
#'
#' @param data
#' @param lineWidth
#'
#' @return
#'
#' @examples
buildLineSeries<-function(xAxis = xax, Series, lineWidth=2){
  logger::log_info("buildLineseries<-function(Series, lineWidth=2)")
  #browser()
  pf<- unique(Series$Portfolio)
  xa<- data.frame(
    date = xAxis
  )
  dd <- Series |> dplyr::filter( Portfolio==pf[2])
  #liit <- full_join( xa, dd,by="date")
  aa<-NULL
  frst=NULL
  plen <-length(pf)
 # browser()
  for( i in 1:plen){

    dd <- Series |> dplyr::filter( Portfolio==pf[i])
    liit <- dplyr::full_join( xa, dd,by="date")

    # sss <- dd |> dplyr::filter( date=="2022-05-20")
    # logger::log_info("Nolla-arvo {sss$Close}")
    # logger::log_info("dd[1] = {dd$Close[1]}")

    aa <-  list(
      list(
        type = "line",
        symbol = "none",
        name = pf[i],
        data = liit$Close,
        lineStyle = list(
          width = lineWidth
        ),
        xAxis = list(
          type = "category",
          data = liit$date
        )

      )
    )
    frst<-append( frst, aa)
  }
  return(frst)
}

retXAxis<-function(...){
  l1 <- list(...)


  x <- NULL
  N<-length(l1)
  for( i in 1:N){
    x <- append(x, l1[[i]]$date)
  }
  x <- unique(x)

return(sort(x))
}

#' @export
rep_showSectors<-function()
{

  sectors<-c("FSDPX", "FSRFX","FSHOX","FSAVX","FCYIX","FSRBX","FSELX","FSPTX","FSENX","FSPHX")
  sectors1<-c("SPY")
  df <- loadMont(sectors)
  dff <- loadMont(sectors1)
  xax <- retXAxis(df, dff)



  OO <- startStruct(xax)
sect <- buildLineSeries(xax, Series=df, lineWidth=2)
ind <- buildLineSeries(xax, Series=dff, lineWidth=5)
li <-append( sect, ind)
OO$series <- li



valu <- min(df$Close[!is.na(df$Close)])*0.95
echarts4r::e_charts() |>
  echarts4r::e_list(OO) |> echarts4r::e_datazoom(y_index = 0, startValue = valu , type = "slider") |>
  echarts4r::e_toolbox_feature("dataZoom") |> echarts4r::e_datazoom() |> echarts4r::e_theme(getOption("theme"))
}

#' @param PortfolioName
#'
#' @export
rep_showCompData <- function(PortfolioName="HOSKL Unelmat")
{
  indexes=NULL
  compData <- retComponents(PortfolioName=PortfolioName, period="days", nullDay = "2022-05-20")
  indData<-loadStocks(stockList=c("^OMXHCAPGI", "EUNL.DE"), nullDay = "2022-05-20" )
  compData$Width <- 2
  indData$Width <- 4
  data <- rbind(compData, indData)
  return(data)
  # xax <- retXAxis(compData, indData)
  # OOg <- startStruct(xax)
  # compSer <- buildLineSeries(xax, Series=compData, lineWidth=2)
  # indSer <- buildLineSeries(xax, Series=indData, lineWidth=5)
  # li <-append( compSer, indSer)
  # OOg$series <- li
  #
  # valu <- min(compData$Close[!is.na(compData$Close)])*0.95
  # echarts4r::e_charts() |>
  #   echarts4r::e_list(OOg) |> echarts4r::e_datazoom(y_index = 0, startValue = valu , type = "slider") |>
  #   echarts4r::e_toolbox_feature("dataZoom") |> echarts4r::e_datazoom() |> echarts4r::e_theme(getOption("theme"))



  # indexes=c("^OMXHCAPGI", "EUNL.DE")
  # logger::log_info("Plot {nrow(compData)} data items")
  # valu <- min(compData$Value)*0.95
  #
  #
  # compData |> dplyr::group_by(ticker) |> echarts4r::e_charts(date) |> echarts4r::e_line(Value) |> echarts4r::e_datazoom() |>
  #   echarts4r::e_tooltip(trigger = "axis", axisPointer = list(type = "cross")) |>
  #   # e_title(text = "HOSKn mallisalkut", left = "left") |>
  #   echarts4r::e_toolbox_feature("dataZoom") |> echarts4r::e_theme("chalk") |> echarts4r::e_animation(duration = 1000) |>
  #   echarts4r::e_datazoom(y_index = 0, startValue = valu , type = "slider") |>
  #   echarts4r::e_grid(gridIndex = "1")


}

#' @param PortfolioName
#'
#' @export
rep_showComp<-function(PortfolioName="HOSKL Unelmat")
{
  indexes=NULL
  compData <- retComponents(PortfolioName=PortfolioName, period="days", nullDay = "2022-05-20")
  indData<-loadStocks(stockList=c("^OMXHCAPGI", "EUNL.DE"), nullDay = "2022-05-20" )
  xax <- retXAxis(compData, indData)
  OOg <- startStruct(xax)
  compSer <- buildLineSeries(xax, Series=compData, lineWidth=2)
  indSer <- buildLineSeries(xax, Series=indData, lineWidth=5)
  li <-append( compSer, indSer)
  OOg$series <- li

  valu <- min(compData$Close[!is.na(compData$Close)])*0.95
  echarts4r::e_charts() |>
    echarts4r::e_list(OOg) |> echarts4r::e_datazoom(y_index = 0, startValue = valu , type = "slider") |>
    echarts4r::e_toolbox_feature("dataZoom") |> echarts4r::e_datazoom() |> echarts4r::e_theme(getOption("theme"))



  # indexes=c("^OMXHCAPGI", "EUNL.DE")
  # logger::log_info("Plot {nrow(compData)} data items")
  # valu <- min(compData$Value)*0.95
  #
  #
  # compData |> dplyr::group_by(ticker) |> echarts4r::e_charts(date) |> echarts4r::e_line(Value) |> echarts4r::e_datazoom() |>
  #   echarts4r::e_tooltip(trigger = "axis", axisPointer = list(type = "cross")) |>
  #   # e_title(text = "HOSKn mallisalkut", left = "left") |>
  #   echarts4r::e_toolbox_feature("dataZoom") |> echarts4r::e_theme("chalk") |> echarts4r::e_animation(duration = 1000) |>
  #   echarts4r::e_datazoom(y_index = 0, startValue = valu , type = "slider") |>
  #   echarts4r::e_grid(gridIndex = "1")


}

#' Näyttää milloin viimeksi on päivitetty HOSKL salkku
#'
#'
#' @return palauttaa tekstin
#'
#' @export
#'
#'@examples
#'
#'\dontrun{
#'rep_lastUpdate()
#'}
#'
rep_lastUpdate <- function(){
  perf <- retPortfolioResult("HOSKL")

  dd<-as.Date(perf$date, format="%d.%m.%Y")
  dd <- sort(dd, decreasing = TRUE)
  da <- dd[1]
  da <- format(da, "%d.%m.%Y")
  da <- paste0( "<font color='red'> **", da, "**</font>")

  da
}

add_cols <- function(df, cols) {
  add <- cols[!cols %in% names(df)]
  if(length(add) !=0 ) df[add] <- NA
  return(df)
}

#' Palauttaa HOSKL salkkujen datan
#'
#' @param GroupName Ryhmän nimi, oletuksena HOSKL
#' @param type portfolio, components
#' @param indexes
#'
#' @return kuvan käyristä
#'
#' @export
#'
#'@examples
#'
#'\dontrun{
#'rep_showPerformanceData
#'}
#'
rep_showPerformanceData <- function(GroupName="HOSKL", Type="portfolio", indexes=NULL,
                                    stocks = NULL, zeroVal=100000, nullDay = "2022-05-20"){
  logger::log_info("rep_StartingState({GroupName}, Type = {Type}, indexes={indexes})")

  if( Type == "stock"){
    compData <- retComponents(PortfolioName=GroupName, period="days", nullDay = "2022-05-20")
    logger::log_info("Plot {nrow(compData)} data items")

    compData |> dplyr::group_by(ticker) |> echarts4r::e_charts(ref_date) |> echarts4r::e_line(Value) |> echarts4r::e_datazoom() |>
      echarts4r::e_tooltip(trigger = "axis", axisPointer = list(type = "cross")) |>
      # e_title(text = "HOSKn mallisalkut", left = "left") |>
      echarts4r::e_toolbox_feature("dataZoom") |>
      echarts4r::e_theme(getOption("theme")) |>
      echarts4r::e_datazoom(y_index = 0, startValue = valu , type = "slider") |>
      echarts4r::e_grid(gridIndex = "1")

  }
  retList=NULL

  if( Type == "portfolio"){
    gpm <- levels(factor(GroupName))
    perf <- NULL
    for( pp in gpm){
      perfn <- rpf(pp)
      perf <- rbind(perf, perfn )
    }
    # perf <- retPortfolioResult(GroupName)
    # perf <-rpf(GroupName)

    #perf <- perf |> dplyr::filter(Portfolio == "EUNL")
    valu <- min(perf$End.Eq)*0.95
    df <- data.frame(val = 1:7)

    colorss <-
      c("red", "blue", "#BF444C", "purple", "black", "#AC8E79","#CF444C")
    # df <- data.frame(val = 1:7)
    #
    # colo <- echarts4r::e_color_range(df, val, colors)

    perf$date <- as.Date(perf$date)
    if(!is.null(stocks)){
      sd<-load(getOption( "StockDataFile"))
      stockData <- get(sd)
      for( tr in stocks){
        trd <- stockData |> dplyr::filter(ticker==tr)

        dd <- trd |> dplyr::filter(abs(ref_date - as.Date(nullDay)) == min(abs(ref_date - as.Date( nullDay)) ))
        if( nrow(dd) != 0) {
          multiple <- zeroVal / dd$price_adjusted_close[1]
        }else {
          multiple <- 1
        }



        pe <- data.frame( date = trd$ref_date, End.Eq = trd$price_adjusted_close*multiple)
        pe$Portfolio = tr

        pe <- add_cols(pe, colnames(perf))
        perf <- rbind( perf, pe)
        # for( n in 1:nrow(trd)){
        #   perf <- tibble::add_row(perf)
        #   perf[nrow(perf), ]$date <- trd$ref_date[n]
        #   perf[nrow(perf), ]$Portfolio <- tr
        #   perf[nrow(perf), ]$End.Eq <- trd$price_adjusted_close[n]*multiple
        # }
      }

    }

    retList <- list( perf=perf, colors = colorss, valu = valu)
    return(retList)

  }

}
##############################################
#retList <- rep_showPerformanceData(GroupName="HOSKL", stocks = "BAS.DE")


##############################################
retbestDays <- function(pd, daysMinus, daysPlus ){
  logger::log_info("retbestDays (pd, daysMinus = {daysMinus}, daysPlus = {daysPlus}")
  pfs <- levels(factor(pd$portfolio))

  retu <- NULL
  for( por in pfs){
    dd <- pd |> dplyr::filter(portfolio == por)
    qxts <- xts::xts(dd[,'value'], order.by = dd[,1])
    colnames(qxts) <- c("Values")
    ret <- PerformanceAnalytics::Return.calculate(qxts)

    for( i in daysMinus:daysPlus){
      data1 <- data.frame( date = zoo::index(ret), Value = zoo::coredata(ret))
      if( i<0){
        data1 <- data1[order(data1$Values),] } else{
          data1 <- data1[order(data1$Values, decreasing = TRUE),]
        }

      data1 <- data1 |> dplyr::mutate( newVal = Values)
      if( i != 0) {
        data1[1:abs(i),'newVal'] <- 0
      }

        dxts <- xts::xts(data1$newVal, order.by = data1$date)
        pa <- PerformanceAnalytics::Return.cumulative(dxts, geometric = TRUE)
        r<-NULL
        r$Portfolio <- por
        r$Days <- i
        r$ret <- pa[1, 'x']
        r<-data.frame(portfolio =por, days=i, ret=pa[1,'x'])
        retu <- rbind(retu, r)
      }

    }

return(retu)
  }


#' Palauttaa HOSKL salkkujen datan mistä on parhaita ja huonoimpia päiviä poistettu
#'
#' @param GroupName Ryhmän nimi, oletuksena HOSKL
#' @param type portfolio, components
#' @param indexes
#'
#' @return datan
#'
#' @export
#'
#'@examples
#'
#'\dontrun{
#'rep_retRemoveData
#'}
#'
rep_retRemoveData <- function(GroupName="HOSKL", Type="portfolio", indexes=NULL, daysMinus=-4, daysPlus=4) {
  logger::log_info("rep_retRemoveData ({GroupName}, Type = {Type}, indexes={indexes})")
  if( Type == "stock"){
    compData <- retComponents(PortfolioName=GroupName, period="days", nullDay = "2022-05-20")
    logger::log_info("Plot {nrow(compData)} data items")

    compData |> dplyr::group_by(ticker) |> echarts4r::e_charts(ref_date) |> echarts4r::e_line(Value) |> echarts4r::e_datazoom() |>
      echarts4r::e_tooltip(trigger = "axis", axisPointer = list(type = "cross")) |>
      # e_title(text = "HOSKn mallisalkut", left = "left") |>
      echarts4r::e_toolbox_feature("dataZoom") |>
      echarts4r::e_theme(getOption("theme")) |>
      echarts4r::e_datazoom(y_index = 0, startValue = valu , type = "slider") |>
      echarts4r::e_grid(gridIndex = "1")

  }
  retList=NULL

  if( Type == "portfolio"){
    perf <- retPortfolioResult(GroupName)
    perf <-rpf(GroupName)

    #perf <- perf |> dplyr::filter(Portfolio == "EUNL")
    valu <- min(perf$End.Eq)*0.95
    df <- data.frame(val = 1:7)

    colorss <-
      c("red", "blue", "#BF444C", "purple", "black", "#AC8E79","#CF444C")
    # df <- data.frame(val = 1:7)
    #
    # colo <- echarts4r::e_color_range(df, val, colors)

    perf$date <- as.Date(perf$date)

    retperf <- perf$End.Eq
    pd <- data.frame(date = perf$date, portfolio = perf$Portfolio, value = perf$End.Eq)

    ret <- retbestDays(pd, daysMinus, daysPlus )

    return(ret)


  }

}


#' Näyttää HOSKL salkkujen tuottokäyrän
#'
#' @param GroupName Ryhmän nimi, oletuksena HOSKL
#' @param type portfolio, components
#' @param indexes
#'
#' @return kuvan käyristä
#'
#' @export
#'
#'@examples
#'
#'\dontrun{
#'rep_showPerformance
#'}
#'
rep_showPerformance<-function(GroupName="HOSKL", Type="portfolio", indexes=NULL){
  logger::log_info("rep_StartingState({GroupName}, Type = {Type}, indexes={indexes})")
  if( Type == "stock"){
    compData <- retComponents(PortfolioName=GroupName, period="days", nullDay = "2022-05-20")
    logger::log_info("Plot {nrow(compData)} data items")

    compData |> dplyr::group_by(ticker) |> echarts4r::e_charts(ref_date) |> echarts4r::e_line(Value) |> echarts4r::e_datazoom() |>
      echarts4r::e_tooltip(trigger = "axis", axisPointer = list(type = "cross")) |>
      # e_title(text = "HOSKn mallisalkut", left = "left") |>
      echarts4r::e_toolbox_feature("dataZoom") |>
      echarts4r::e_theme(getOption("theme")) |>
      echarts4r::e_datazoom(y_index = 0, startValue = valu , type = "slider") |>
      echarts4r::e_grid(gridIndex = "1")

  }

  if( Type == "portfolio"){
    perf <- retPortfolioResult(GroupName)
    perf <-rpf(GroupName)

    #perf <- perf |> dplyr::filter(Portfolio == "EUNL")
    valu <- min(perf$End.Eq)*0.95
    df <- data.frame(val = 1:7)

    colorss <-
      c("red", "blue", "#BF444C", "purple", "black", "#AC8E79","#CF444C")
    # df <- data.frame(val = 1:7)
    #
    # colo <- echarts4r::e_color_range(df, val, colors)

    perf$date <- as.Date(perf$date)
    perf |> dplyr::group_by(Portfolio) |>
      echarts4r::e_charts(date) |>
      echarts4r::e_line(End.Eq, symbol="none",
                        lineStyle = list(normal = list(width = 3))
                        ) |>
      echarts4r::e_datazoom() |>
      echarts4r::e_tooltip(trigger = "axis",
                           axisPointer = list(type = "cross")) |>
      # e_title(text = "HOSKn mallisalkut", left = "left") |>
      echarts4r::e_toolbox_feature("dataZoom") |> echarts4r::e_theme(getOption("theme")) |>
      echarts4r::e_datazoom(y_index = 0, startValue = valu , type = "slider") |>
      echarts4r::e_grid(gridIndex = "1")|>
      echarts4r::e_y_axis(
        formatter = echarts4r::e_axis_formatter(style=c("currency"),
                                     digits=0,
                                     currency="EUR"
        )) |>
      echarts4r::e_color(colorss)

  }

}


#' Näyttää HOSKL salkkujen tuottokäyrän
#'
#' @param GroupName Ryhmän nimi, oletuksena HOSKL
#' @param type portfolio, components
#' @param indexes
#'
#' @return kuvan käyristä
#'
#' @export
#'
#'@examples
#'
#'\dontrun{
#'rep_showPerformance
#'}
#'
rep_showPerformance<-function(GroupName="HOSKL", Type="portfolio", indexes=NULL){
  logger::log_info("rep_StartingState({GroupName}, Type = {Type}, indexes={indexes})")
  if( Type == "stock"){
    compData <- retComponents(PortfolioName=GroupName, period="days", nullDay = "2022-05-20")
    logger::log_info("Plot {nrow(compData)} data items")

    compData |> dplyr::group_by(ticker) |> echarts4r::e_charts(ref_date) |> echarts4r::e_line(Value) |> echarts4r::e_datazoom() |>
      echarts4r::e_tooltip(trigger = "axis", axisPointer = list(type = "cross")) |>
      # e_title(text = "HOSKn mallisalkut", left = "left") |>
      echarts4r::e_toolbox_feature("dataZoom") |> echarts4r::e_theme(getOption("theme")) |>
      echarts4r::e_datazoom(y_index = 0, startValue = valu , type = "slider") |>
      echarts4r::e_grid(gridIndex = "1")

  }

  if( Type == "portfolio"){
    perf <- retPortfolioResult(GroupName)
    perf <-rpf(GroupName)

    #perf <- perf |> dplyr::filter(Portfolio == "EUNL")
    valu <- min(perf$End.Eq)*0.95
    df <- data.frame(val = 1:7)

    colorss <-
      c("red", "blue", "#BF444C", "purple", "black", "#AC8E79","#CF444C")
    # df <- data.frame(val = 1:7)
    #
    # colo <- echarts4r::e_color_range(df, val, colors)

    perf$date <- as.Date(perf$date)
    perf |> dplyr::group_by(Portfolio) |>
      echarts4r::e_charts(date) |>
      echarts4r::e_line(End.Eq, symbol="none",
                        lineStyle = list(normal = list(width = 3))
      ) |>
      echarts4r::e_datazoom() |>
      echarts4r::e_tooltip(trigger = "axis", axisPointer = list(type = "cross")) |>
      # e_title(text = "HOSKn mallisalkut", left = "left") |>
      echarts4r::e_toolbox_feature("dataZoom") |> echarts4r::e_theme(getOption("theme")) |>
      echarts4r::e_datazoom(y_index = 0, startValue = valu , type = "slider") |>
      echarts4r::e_grid(gridIndex = "1")|>
      echarts4r::e_y_axis(
        formatter = echarts4r::e_axis_formatter(style=c("currency"),
                                                digits=0,
                                                currency="EUR"
        )) |>
      echarts4r::e_color(colorss)

  }

}

