
library(highcharter)

###
# BARPLOTS

## IN: array(Country, Year, Sector) - tipo %GDP o tipo GERD
#      
## OUT: Barplots con todos los Sectors apilados, para un (Country) y el stack
#       ordenado por los Sector(es) que se especifique
stackBP_Country <- function(a, country, sectors_stack = NULL, 
                            order_stack_by = NULL) {
  
  if (is.null(sectors_stack))
    sectors_stack <- dimnames(a)[["Sector"]]
  
  out <- a[country, , sectors_stack]
  out <- as.data.frame(out)
  
  sel_rows <- apply(out, 1, function(x) sum(is.na(x))) <= (ncol(out) - 1)
  out <- out[sel_rows, , drop = FALSE]
  
  rownames(out) <- dimnames(a)[["Year"]][sel_rows]
  names(out)    <- sectors_stack
  
  if(!is.null(order_stack_by))
    out <- out[, order_stack_by]
  
  out
}

hc_stackBP <- function(df,
                       title = "", subtitle = "", labX = "", labY = "", 
                       seriesNames = NULL,
                       sumLine = FALSE) {
  
  if(is.null(seriesNames)) 
    # seriesNames <- paste0("Col", 1:ncol(df))
    seriesNames <- names(df)
  
  hc <- highchart() %>% 
    hc_chart(type = "column",
             options3d = list(enabled = TRUE, beta = 15, alpha = 15)) %>% 
    hc_title(text = title) %>% 
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text = labX)) %>%
    hc_yAxis(title = list(text = labY)) %>% 
    hc_plotOptions(column = list(
      dataLabels = list(enabled = FALSE),
      stacking = "normal",
      enableMouseTracking = TRUE)
    ) 
  
  for(i in (1:ncol(df))) {
    hc <- (hc %>% hc_add_series(name = seriesNames[i], 
                                data = df[, i]))
  }
  
  if (sumLine == TRUE) {
    d <- data.frame(Total = apply(df, 1, sum, na.rm = TRUE))
    hc <- (hc %>% hc_add_series(type = "line",
                                name = "Total", 
                                data = d,
                                hcaes(y = Total)))
  }

  hc <- (hc %>% hc_xAxis(categories = row.names(df)))
  
  hc  
}

#       Barplots de todos los paises con los Sectors apilados para un (Year) y 
#       el stack ordenado ordenado por los sectores que se especifique y los
#       barplots ordenados por los sectores que se especifique
stackBP_Year <- function(a, Year, sectors_stack = NULL, 
                            order_stack_by = NULL) {
  
  if (is.null(sectors_stack))
    sectors_stack <- dimnames(a)[["Sector"]]
  
  out <- a[, Year, sectors_stack]
  out <- as.data.frame(out)
  
  sel_rows <- apply(out, 1, function(x) sum(is.na(x))) <= (ncol(out) - 1)
  out <- out[sel_rows, , drop = FALSE]
  
  rownames(out) <- dimnames(a)[["Country"]][sel_rows]
  names(out)    <- sectors_stack
  
  if(!is.null(order_stack_by))
    out <- out[, order_stack_by]
  
  out
}


### 
# TIME SERIES

## IN: array(Country, Year, Sector) - tipo %GDP o tipo GERD
#
## OUT: Time series de los (Country, Sector) especificados


mtsCS <- function(a, countries = NULL, sector) {
  
  if (is.null(countries))
    countries <- dimnames(a)[["Country"]]

  out <- ts(t(a[countries,,sector]),
                 frequency = 1, start = c(2004,1))
  out
}

hc_mtsCS <- function(mts_in,
                     title = "", labX = "", labY = "") {
  hc <- highchart()
  
  for(c in 1:ncol(mts_in)) {
    hc <- (hc %>% hc_add_series_ts(mts_in[, c], type = "line", 
                                   name = colnames(mts_in)[c]))
  }
  
  hc <- hc %>% hc_title(text = title) %>% 
    hc_xAxis(title = list(text = labX)) %>%
    hc_yAxis(title = list(text = labY))
  
  # hc <- (hc %>% hc_xAxis(categories = as.character(time(mts_in))))
  
  hc
}

hc_mtsCS2 <- function(mts_in,
                     title = "", labX = "", labY = "") {
  hc <- highchart()
  
  df_in <- as.data.frame(mts_in)
  df_in$Year <- time(mts_in)
  
  for(c in 1:ncol(mts_in)) {
    hc <- (hc %>% hc_add_series(data = df_in, type = "line",
                                hcaes(x = Year,
                                      y = eval(expression(colnames(df_in)[c]))),
                                   name = colnames(df_in)[c]))
  }
  
  hc <- hc %>% hc_title(text = title) %>% 
    hc_xAxis(title = list(text = labX)) %>%
    hc_yAxis(title = list(text = labY))
  
  # hc <- (hc %>% hc_xAxis(categories = as.character(time(mts_in))))
  
  hc
}



