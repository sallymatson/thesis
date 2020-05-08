graph_longitude <- function(lon_data){
  plot(lon_data, type='b', axes=FALSE, xlab="Longitude", ylab="Value")
  axis(at=seq(73)-0.5,labels=c(lons-2.5, 180), side=1)
  axis(side=2)
}

graph_latitude <- function(lat_data){
  plot(lat_data, type='b', axes=FALSE, xlab="Latitude", ylab="Value")
  axis(at=seq(35)-0.5,labels=c(lats.new-2.5, 85), side=1)
  axis(side=2)
}

graph_over_time <- function(time_data){
  plot(time_data, type='b', axes=FALSE, xlab="Date", ylab="Value")
  axis(at=seq(144),labels=date_is(times), side=1)
  axis(side=2)
}

time_plotly <- function(time_data){
  years = c("2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014")
  dates = date_is(times)

  all_data <- data.frame(XCH4 = time_data, Date = date_is(times))
  
  fig.time <- plot_ly(all_data, x = ~Date, y=~XCH4,  mode = 'lines+markers', type="scatter", line=list(color="#2CA02C"), marker=list(color="#2CA02C"))
  fig.time <- fig.time %>%
    layout(
      xaxis = list(
        type = 'date',
        tickformat = "%b %Y",
        dtick="M12"
      ), yaxis=list(title="CH4 (ppb)"), zeroline=FALSE)
  fig.time <- fig.time %>% layout(showlegend = FALSE)
  return(fig.time)
}


longitude_plotly <- function(lon_data){
  lon_data <- data.frame(XCH4=lon_data, Longitude=lons)
  fig.lon <- plot_ly(lon_data, x = ~Longitude, y=~XCH4,  mode = 'lines+markers', type="scatter", line=list(color="#FF7F0E"), marker=list(color="FF7F0E"))
  fig.lon <- fig.lon %>% layout(xaxis=list(dtick=20, tick0=-180, zeroline=FALSE), yaxis=list(title="CH4 (ppb)", zeroline=FALSE))
  return(fig.lon)
}

latitude_plotly <- function(lat_data){
  lat_data <- data.frame(XCH4=lat_data, Latitude=lats.new)
  fig.lat <- plot_ly(lat_data, x = ~Latitude, y=~XCH4,  mode = 'lines+markers', type="scatter", line=list(color="#1F77B4"), marker=list(color="#1F77B4"))
  fig.lat <- fig.lat %>% layout(xaxis=list(dtick=10, range=list(-90,90), zeroline=FALSE), yaxis=list(title="CH4 (ppb)", zeroline=FALSE))
  return(fig.lat)
}
