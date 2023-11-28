

#' Title
#'
#' @param GroupName
#'
#' @return
#' @export
#'
#' @examples
rep_boxplot<- function(GroupName = "HOSKL"){
  perf <- retPortfolioResult(GroupName)
  #perf <-rpf(GroupName)
  portfolios<-unique(perf$Portfolio)
  #li <-  perf |> split(perf$Portfolio) |> map(summary)

  #for( pf in portfolios){
  tbb<-NULL
  portfolios<-c( "HOSKL Unelmat", "HOSKLRikkaaksi", "HOSKL Sarvinta")
  for( pf in portfolios){
    #pf<-portfolios[i]
    #pf<- "HOSKL Unelmat"
    # pf<- "EUNL"
    # pf<- "OMXHCAPGI"
    # pf<- "HOSKLRikkaaksi"
    # pf <- "HOSKL Sarvinta"
    pe <- perf |> dplyr::filter( Portfolio == pf)
    pe$Portfolio <- NULL
    xtsPerf <- xts::xts(pe[,-1], order.by=as.Date(pe[,1], format(getOption("DateFormat"))))
    weeklyd <-xts::to.weekly(xtsPerf, OHLC=FALSE)
    weeklyret  <- weeklyd |> PerformanceAnalytics::Return.calculate()
    weekC <- weeklyret$Close
    core <- as.double(zoo::coredata(weekC))

    tb <-tibble::tibble( weekly = core*100)
    tb$Portfolio = pf
    tbb <- rbind( tbb, tb)
  }

  tbb |>
         dplyr::group_by(Portfolio) |>
          echarts4r::e_charts() |>
          echarts4r::e_boxplot(weekly) |>
          echarts4r::e_theme(getOption("theme")) |>
          echarts4r::e_title("Viikkomuutoksien jakauman kuvauksia boxplotilla")
}


#' Title
#'
#' @param GroupName
#'
#' @return
#' @export
#'
#' @examples
rep_histogram <- function(GroupName = "HOSKL"){

  perf <- retPortfolioResult(GroupName)
  #perf <-rpf(GroupName)
  portfolios<-unique(perf$Portfolio)


  #for( pf in portfolios){
  tbb<-NULL
  portfolios<-c( "HOSKL Unelmat", "HOSKLRikkaaksi", "HOSKL Sarvinta")
  for( pf in portfolios){
    #pf<-portfolios[i]
    #pf<- "HOSKL Unelmat"
    # pf<- "EUNL"
    # pf<- "OMXHCAPGI"
    # pf<- "HOSKLRikkaaksi"
    # pf <- "HOSKL Sarvinta"
    pe <- perf |> dplyr::filter( Portfolio == pf)
    pe$Portfolio <- NULL
    xtsPerf <- xts::xts(pe[,-1], order.by=as.Date(pe[,1], format(getOption("DateFormat"))))
    weeklyd <-xts::to.weekly(xtsPerf, OHLC=FALSE)
    weeklyret  <- weeklyd |> PerformanceAnalytics::Return.calculate()
    weekC <- weeklyret$Close
    core <- as.double(zoo::coredata(weekC))

    tb <-tibble::tibble( weekly = core*100)
    tb$Portfolio = pf
    tbb <- rbind( tbb, tb)
  }
#
#
#
# tbb |> dplyr::group_by(Portfolio) |> echarts4r::e_charts() |>
#   echarts4r::e_density(weekly, areaStyle = list(opacity = .1), smooth = TRUE, name = Portfolio, y_index = 1) |>
#   echarts4r::e_legend(TRUE) |>
#   echarts4r::e_y_axis( formatter = echarts4r::e_axis_formatter("percent", digits = 0)) |>
#   echarts4r::e_tooltip(trigger = "axis")



tbb |> dplyr::group_by(Portfolio) |> echarts4r::e_charts() |>
  echarts4r::e_histogram(weekly, backgroundStyle = list(opacity=.9)) |>
  echarts4r::e_legend(TRUE) |>
  echarts4r::e_tooltip(trigger = "axis") |> echarts4r::e_theme(getOption("theme"))


#   echarts4r::e_tooltip(trigger = "axis") |> echarts4r::e_facet(rows=2, cols=3, legend_pos = "top", legend_space = 12) |>
#   echarts4r::e_legend(TRUE)




}



