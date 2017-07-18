
kk <- apply(X = RD_Exp_GDP, MARGIN = c("Country", "Year"), FUN = function(sectors) {
  sapply(sectors, function(x) {
    x/sectors["AllS"]
  })
})

kk[,c("EU28", "Euro area", "Spain"),"2015"]
RD_Exp_GDP[c("EU28", "Euro area", "Spain"), "2015", ]


#####
library(highcharter)

# No ts
hc <- highchart()

for(c in 1:ncol(allS_mts)) {
  hc <- (hc %>% hc_add_series(as.numeric(allS_mts[, c]), type = "line", 
                              name = colnames(allS_mts)[c]))
}

hc <- (hc %>% hc_xAxis(type = "category",
                       categories = dimnames(RD_Exp_GDP)$Year))

hc

# ts
hc <- highchart()

for(c in 1:ncol(allS_mts)) {
  hc <- (hc %>% hc_add_series_ts(allS_mts[, c], type = "line", 
                              name = colnames(allS_mts)[c]))
}

hc <- hc %>% hc_title(text = "Research and development expenditure, by sectors of performance - % of GDP")

hc

## Column

myData <- as.data.frame(RD_pGERD[,"2013", ])
myData <- myData[apply(myData, 1, function(x) sum(is.na(x))) < 5, ]
hc <- highchart()
hc <- hc %>% 
  hc_chart(type = "column") %>% 
  hc_title(text = "Gross domestic expenditure on R&D (GERD) by source of funds - % of total GERD") %>% 
  # hc_yAxis(title = list(text = "Weights")) %>% 
  hc_plotOptions(column = list(
    dataLabels = list(enabled = FALSE),
    stacking = "normal",
    enableMouseTracking = TRUE)
  ) %>% 
  hc_add_series(name = "Business enterprise sector", data = myData[, "BES"])  %>%
  hc_add_series(name = "Government sector",          data = myData[, "GS"]) %>%
  hc_add_series(name = "Higher education sector",    data = myData[, "HES"]) %>%
  hc_add_series(name = "PNPS",                       data = myData[, "PNPS"]) %>%
  hc_add_series(name = "Abroad",                     data = myData[, "Abroad"])

hc <- (hc %>% hc_xAxis(categories = row.names(myData)))

hc

myData <- as.data.frame(RD_Exp_GDP[,"2013", ])
myData <- myData[apply(myData, 1, function(x) sum(is.na(x[-1]))) < 4, ]
myData <- myData[order(myData$GS+myData$HES, decreasing = TRUE), ]
hc <- highchart()
hc <- hc %>% 
  hc_chart(type = "column",
           options3d = list(enabled = TRUE, beta = 15, alpha = 15)) %>% 
  hc_title(text = "Research and development expenditure, by sectors of performance - % of GDP") %>% 
  # hc_yAxis(title = list(text = "Weights")) %>% 
  hc_plotOptions(column = list(
    dataLabels = list(enabled = FALSE),
    stacking = "normal",
    enableMouseTracking = TRUE)
  ) %>% 
  hc_add_series(name = "Business enterprise sector", data = myData[, "BES"])  %>%
  hc_add_series(name = "PNPS",                       data = myData[, "PNPS"]) %>%
  hc_add_series(name = "Government sector",          data = myData[, "GS"]) %>%
  hc_add_series(name = "Higher education sector",    data = myData[, "HES"])
   

hc <- (hc %>% hc_xAxis(categories = row.names(myData)))

hc

myData <- as.data.frame(RD_Exp_GDP[,"2013", ])
myData <- myData[apply(myData, 1, function(x) sum(is.na(x[-1]))) < 4, ]
myData <- myData[order(myData$BES, decreasing = TRUE), ]
hc <- highchart()
hc <- hc %>% 
  hc_chart(type = "column",
           options3d = list(enabled = TRUE, beta = 15, alpha = 15)) %>% 
  hc_title(text = "Research and development expenditure, by sectors of performance - % of GDP") %>% 
  # hc_yAxis(title = list(text = "Weights")) %>% 
  hc_plotOptions(column = list(
    dataLabels = list(enabled = FALSE),
    stacking = "normal",
    enableMouseTracking = TRUE)
  ) %>% 
  hc_add_series(name = "PNPS",                       data = myData[, "PNPS"]) %>%
  hc_add_series(name = "Government sector",          data = myData[, "GS"]) %>%
  hc_add_series(name = "Higher education sector",    data = myData[, "HES"]) %>%
  hc_add_series(name = "Business enterprise sector", data = myData[, "BES"])  


hc <- (hc %>% hc_xAxis(categories = row.names(myData)))

hc

# SAVE
library(htmlwidgets)
saveWidget(hc, 
           file = "RD_by_GDP_BES.html", 
           selfcontained = FALSE)

## CORR and LM

GDP_DF <- data.frame(AllS = myData$AllS, BES = myData$BES,
                     GS = myData$GS, HES = myData$HES, PNPS = myData$PNPS,
                     GS_HES = (myData$GS+myData$HES))
cor(GDP_DF)

summary(lm(AllS ~ BES-1, data = GDP_DF))
summary(lm(AllS ~ GS_HES - 1, data = GDP_DF))
summary(lm(AllS ~ BES + GS_HES - 1, data = GDP_DF))


###
RD_Exp_GDP <- get_RD_Exp_GDP()
RD_RD_pGERD <- get_RD_RD_pGERD()
npcPPA <- readExcel_PC_PPA("./DATA/IMF_PC_PPA_2016.xlsx")
nPPA <- readExcel_PPA("./DATA/IMF_PC_PPA_2016.xlsx")
PPA_RD_Exp_GDP <- merge_PPA_RD_Exp_GDP(npcPPA, nPPA, RD_Exp_GDP[,"2013", "AllS"])
PPA_RD_Exp_GDP$RD_pGERD_BES <- RD_RD_pGERD[dimnames(RD_RD_pGERD)[["Country"]] %in% rownames(PPA_RD_Exp_GDP),
                                           "2013", "BES"]
PPA_RD_Exp_GDP$AbsGDP <- PPA_RD_Exp_GDP$PPA * PPA_RD_Exp_GDP$Exp_GDP / 100

plot(PPA_RD_Exp_GDP,
     xlab = "R&D Expenditure as %GDP (2013)",
     ylab = "Per Capita PPA (2016)")
PPA_RD_Exp_GDP_lm <- lm(PPA ~ Exp_GDP, PPA_RD_Exp_GDP)
abline(reg = PPA_RD_Exp_GDP_lm)
abline(h = mean(PPA_RD_Exp_GDP$PPA, na.rm = TRUE), lty = 2)
abline(v = mean(PPA_RD_Exp_GDP$Exp_GDP, na.rm = TRUE), lty = 2)

library(broom)
library(dplyr)

aux <- augment(PPA_RD_Exp_GDP_lm)
aux$Country = rownames(na.omit(PPA_RD_Exp_GDP))
myFit <- arrange(aux, PPA)
PPA_RD_Exp_GDP$Country = rownames(PPA_RD_Exp_GDP) # ??

q_PPA     <- quantile(PPA_RD_Exp_GDP$PPA, na.rm = TRUE)
q_Exp_GDP <- quantile(PPA_RD_Exp_GDP$Exp_GDP, na.rm = TRUE)

library(highcharter)
highchart() %>% 
  hc_add_series(PPA_RD_Exp_GDP,
                type = "scatter",
                hcaes(x = Exp_GDP, y = PPA),
                name = "R&D Expenditure as %GDP",
                dataLabels = list(enable = TRUE,
                                  label = rownames(na.omit(PPA_RD_Exp_GDP)))) %>%
  # hc_add_series_scatter(x = PPA_RD_Exp_GDP$Exp_GDP,
  #                       y = PPA_RD_Exp_GDP$PPA,
  #                       name = "R&D Expenditure as %GDP",
  #                       label = rownames((PPA_RD_Exp_GDP))) %>%
  hc_add_series(myFit, 
                type = "line", 
                hcaes(x = Exp_GDP, y = .fitted),
                name = "Fit", 
                id = "fit") %>% 
  hc_title(text = "") %>% 
  hc_chart(zoomType = "xy") %>%
  hc_yAxis(title = list(text = "Per Capita PPA (2016)"),
           plotLines = list(list(label = list(text = "q25%"),
                                 value = as.numeric(q_PPA["25%"]),
                                 color = "#FF0000",
                                 width = 2,
                                 dashStyle = "ShortDot"),
                            list(label = list(text = "q75%"),
                                 value = as.numeric(q_PPA["75%"]),
                                 color = "#FF0000",
                                 width = 2,
                                 dashStyle = "ShortDot"))) %>%
  hc_xAxis(title = list(text = "R&D Expenditure as %GDP (2013)"),
           plotLines = list(list(label = list(text = "q25%"),
                                 value = as.numeric(q_Exp_GDP["25%"]),
                                 color = "#FF0000",
                                 width = 2,
                                 dashStyle = "ShortDot"),
                            list(label = list(text = "q75%"),
                                 value = as.numeric(q_Exp_GDP["75%"]),
                                 color = "#FF0000",
                                 width = 2,
                                 dashStyle = "ShortDot"))) %>%
  hc_tooltip(#crosshairs = TRUE, 
             backgroundColor = "#FCFFC5",
             shared = FALSE, 
             borderWidth = 5) 



## Otro intento
PPA_RD_Exp_GDP$RD_pGERD_BES[is.na(PPA_RD_Exp_GDP$RD_pGERD_BES)] <- 1
PPA_RD_Exp_GDP$AbsGDP[is.na(PPA_RD_Exp_GDP$AbsGDP)] <- 0


colors <- c("#FB1108","#FD150B","#FA7806","#FBE426","#FCFB8F",
            "#F3F5E7", "#C7E4EA","#ABD6E6","#9AD2E1")
# colors <- c("Red", "Orange", "Yellow", "Green", "Cyan", "Blue", "Violet")

PPA_RD_Exp_GDP$color <- colorize(log(PPA_RD_Exp_GDP$RD_pGERD_BES), colors = colors)
PPA_RD_Exp_GDP$color[is.na(PPA_RD_Exp_GDP$color)] <- 0

# PPA_RD_Exp_GDP$size <- log(PPA_RD_Exp_GDP$AbsGDP + 1)
PPA_RD_Exp_GDP$size <- PPA_RD_Exp_GDP$AbsGDP

x <- c("", "Exp_GDP", "RD_pGERD_BES", "pcPPA", "PPP")
# y <- sprintf("{point.%s:.2f}", c("Country", "Exp_GDP", "RD_pGERD_BES", "pcPPA"))
y <- c("{point.Country}", sprintf("{point.%s:.2f}", c("Exp_GDP", "RD_pGERD_BES", "pcPPA", "PPA")))
tltip <- tooltip_table(x, y)

PPA_RD_Exp_GDP <- na.omit(PPA_RD_Exp_GDP)

hchart(PPA_RD_Exp_GDP, "scatter", 
       hcaes(x = Exp_GDP, y = pcPPA, color = color, size = size),
       name = "R&D Expenditure as %GDP") %>% 
  hc_chart(backgroundColor = "black") %>% 
  hc_tooltip(useHTML = TRUE, 
             headerFormat = "", 
             pointFormat = tltip) %>%
  hc_add_series(myFit,
                type = "line",
                hcaes(x = Exp_GDP, y = .fitted),
                name = "Fit",
                color = "Blue",
                id = "fit",
                lineWidth = 1) %>%
  hc_title(text = "") %>% 
  hc_chart(zoomType = "xy") %>%
  hc_yAxis(title = list(text = "Per Capita PPA (2016) - K Int. Dollars"),
           gridLineWidth = 0,
           plotLines = list(list(label = list(text = "q25%"),
                                 value = as.numeric(q_PPA["25%"]),
                                 color = "#FF0000",
                                 width = 1,
                                 dashStyle = "ShortDot"),
                            list(label = list(text = "q75%"),
                                 value = as.numeric(q_PPA["75%"]),
                                 color = "#FF0000",
                                 width = 1,
                                 dashStyle = "ShortDot"))) %>%
  hc_xAxis(title = list(text = "R&D Expenditure as %GDP (2013)"),
           plotLines = list(list(label = list(text = "q25%"),
                                 value = as.numeric(q_Exp_GDP["25%"]),
                                 color = "#FF0000",
                                 width = 1,
                                 dashStyle = "ShortDot"),
                            list(label = list(text = "q75%"),
                                 value = as.numeric(q_Exp_GDP["75%"]),
                                 color = "#FF0000",
                                 width = 1,
                                 dashStyle = "ShortDot"))) %>% 
  # hc_add_theme(hc_theme_chalk()
  hc_add_theme(hc_theme_darkunica())

#### Un país en el tiempo
country <- "Spain"

kk <- RD_Exp_GDP[country,,c("BES", "GS", "HES", "PNPS")]
barplot(t(kk))


kk2 <-RD_pGERD[country,,c("BES", "GS", "HES", "PNPS", "Abroad")]
barplot(t(kk2))


## binding arrays
kk <- abind::abind(RD_Exp_GDP, 
                   RD_pGERD[dimnames(RD_pGERD)[["Country"]]!="Bosnia and Herzegovina",,], along = 3)
names(dimnames(kk)) <- names(dimnames(RD_Exp_GDP))

dimnames(kk)[["Sector"]] <- c(dimnames(RD_Exp_GDP)[["Sector"]],
                              paste0("p", dimnames(RD_pGERD)[["Sector"]]))
