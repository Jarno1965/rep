


rep_leverSPY <- function(){
  lever=c(0, 0.25, 0.5, 0.75)
  sd<-load(getOption( "StockDataFile"))
  stockData <- get(sd)
  SPY<- stockData|>dplyr::filter(ticker=="SPY")
  df <- data.frame( date=SPY$ref_date,
                    ret0 = tidyr::replace_na(SPY$ret_adjusted_prices,0)
      )



  df <- df|>dplyr::mutate( ret25 = ret0*1.25, ret50 =ret0*1.50, ret75 =ret0*1.75  )
  df <- df|>dplyr::transmute(
          date = date,
          cum0 = cumprod(1 + ret0),
          cum25 = cumprod(1 + ret0*1.25),
          cum50 = cumprod(1 + ret0*1.50),
          cum75 = cumprod(1 + ret0*1.75)
          )
  dff <- df |>tidyr::pivot_longer( cols=dplyr::starts_with("cum"),
                            names_to="Open",
                            values_to ="Values" )
  mutate
  xtsDff <- xts::xts( dff[-1], order.by=dff$date)
  xtsWeek <- xts::to.weekly(xtsDff, OHLC=FALSE)
  zc <- zoo::coredata(xtsWeek)
  zc$Values <- as.numeric(zc$Values)
  dfWeek <- data.frame( date=zoo::index(xtsWeek),   zoo::coredata(xtsWeek) )

  valu <- min(dfWeek$Values[!is.na(dfWeek$Values)])*0.95
  qqplot()

  dfWeek |> dplyr::group_by(Open) |> echarts4r::e_chart(date)|>echarts4r::e_line(Values) |>
  #  echarts4r::e_tooltip(trigger = "axis", axisPointer = list(type = "cross")) |>
    echarts4r::e_toolbox_feature("dataZoom")

  echarts4r::e_charts(as.Date(df$date) ) |>
    echarts4r::e_line(df$cum0)
  #
  # |>
  #   echarts4r::e_line(df$cum25) |>
  #   echarts4r::e_line(df$cum50) |>
  #   echarts4r::e_line(df$cum75) |>
  #   echarts4r::e_datazoom() |>
  #   echarts4r::e_tooltip(trigger = "axis", axisPointer = list(type = "cross")) |>
  #   echarts4r::e_toolbox_feature("dataZoom") |>
  #   echarts4r::e_theme("chalk") |> echarts4r::e_animation(duration = 1000) |>
  #   echarts4r::e_datazoom(y_index = 0, startValue = valu , type = "slider") |>
  #   echarts4r::e_grid(gridIndex = "1")


  # compData |> dplyr::group_by(ticker) |> echarts4r::e_charts(date) |> echarts4r::e_line(Value) |> echarts4r::e_datazoom() |>
  #   echarts4r::e_tooltip(trigger = "axis", axisPointer = list(type = "cross")) |>
  #   # e_title(text = "HOSKn mallisalkut", left = "left") |>
  #   echarts4r::e_toolbox_feature("dataZoom") |> echarts4r::e_theme("chalk") |> echarts4r::e_animation(duration = 1000) |>
  #   echarts4r::e_datazoom(y_index = 0, startValue = valu , type = "slider") |>
  #   echarts4r::e_grid(gridIndex = "1")
}
