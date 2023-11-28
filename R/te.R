library(echarts4r)

N <- 20 # data points
#echarts4r::e_line(Close, lineStyle = list(width = "1"))
opts <- list(
  tooltip = list(
    trigger = "axis"
  ),
  xAxis = list(
    type = "category",
    data = LETTERS[1:N]
  ),
  yAxis = list(
    type = "value"
  ),
  series = list(
    list(
      type = "line",
      data = round(runif(N, 5, 20)),
      lineStyle = list(width=5)
    ),
    list(
      type = "line",
      data = round(runif(N, 5, 20)),
      lineStyle = list(width=1)
    )

  )




)

e_charts() |>
  e_list(opts)
