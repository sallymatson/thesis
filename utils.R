
date_is <- function(num_days){
  start_date = as.Date('1990-01-01')
  return (start_date + num_days)
}

date_as_num <- function(date, times){
  start_date = as.Date('1990-01-01')
  end_date = as.Date(date)
  return (match(as.numeric(end_date - start_date), times))
}

find_at <- function(lat, lats){
  return (match(lat, lats))
}

make_year_matrix <- function(date,xch4,lats,lons,times){
  new_mat = matrix(rep(0, 2592*3), nrow=2592)
  index = 1
  time = date_as_num(date,times)
  for (i in seq(from=1, to=36)){
    for (j in seq(from=1, to=72)){
      new_mat[index,1] = xch4[j,i,time]
      new_mat[index,2] = lats[i]
      new_mat[index,3] = lons[j]
      index = index + 1
    }
  }
  df = as.data.frame(new_mat)
  df = na.omit(df)
  return (df)
}

make_na_matrix <- function(xch4,lats,lons){
  new_mat = matrix(rep(0, 2592*3), nrow=2592)
  index = 1
  for (i in seq(from=1, to=36)){
    for (j in seq(from=1, to=72)){
      new_mat[index,1] = (sum(!is.na(xch4[j,i,]))/dim(xch4)[3])
      new_mat[index,2] = lats[i]
      new_mat[index,3] = lons[j]
      index = index + 1
    }
  }
  return(as.data.frame(new_mat))
}

world_plot <- function(df){
  world <- map_data("world")
  ggplot(df, aes(V3, V2)) +
    geom_point(aes(colour = V1))+geom_polygon(data = world, aes(x=long, y = lat, group = group), fill = NA, color = "red")
}

over_time <- function(lat, lon, xch4, lats, lons){
  index = 1
  mat = matrix(ncol=2, nrow=144)
  for (t in seq(from=1, to=144)) {
    mat[index,1] = times[t]
    mat[index,2] = xch4[match(lon, lons),match(lat, lats),t]
    index = index + 1
  }
  return(mat)
}

lat_lon_matrix_at_date <- function(date,xch4,lats,lons,times){
  # Gets the methane data for a certain date, and puts it in a 
  # matrix where each ROW is a latitude and each COLUMN is a longitude.
  
  date_mat <- make_year_matrix(date,xch4,lats,lons,times)
  
  # todo: check for rows / cols with all NA
  lats_index = lats
  lons_index = lons
  
  m = length(lons_index)
  n = length(lats_index)
  
  A0 = matrix(rep(NA, m*n), ncol = m)
  
  ## A0 indexes row, column. lon is 
  
  for (i in 1:nrow(date_mat)) {
    lon_index = which(lons_index == date_mat[,3][i])
    lat_index = which(lats_index == date_mat[,2][i])
    A0[lat_index, lon_index] = date_mat[,1][i]
  }
  return(A0)
}

fixed_latitude_matrix <-function(lat, xch4, lats, lons){
  # Matrix will have 144 columns and 72 rows
  B0 = matrix(rep(NA, 144*72), ncol=144)
  curr_row = 1
  
  # This gives it at one place, need to loop over LONGITUDE
  for (lon in lons){
    # Over_Time makes a list with column 1 as time and column 2 as amt
    ot = over_time(lat, lon, xch4, lats, lons)
    B0[curr_row,] = t(ot[,2])
    curr_row = curr_row + 1 
  }
  return(B0)
}

sum_isna = function(vec){
  return(sum(is.na(vec)))
}


softimpute_extended <- function(mat, acceptable_na, indices){
  # Drop columns with too many NA:
  keep_indices = which(apply(mat, 1, sum_isna) <= acceptable_na)
  D1 = mat[keep_indices, ]
  
  # Scale
  D1.new = D1*10^6
  
  # Get fit
  fit = softImpute(D1.new,rank=5,lambda=0.1)
  
  trend_1 = fit$u[,1]
  trend_1
  mat = matrix(rep(NA, length(indices)*2), nrow=2)
  mat[1,] = indices
  mat[2,keep_indices] = trend_1
  
  return(mat)
}
