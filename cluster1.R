source("readExcelData.R")

RD_Exp_GDP  <- get_RD_Exp_GDP()
RD_RD_pGERD <- get_RD_RD_pGERD()

npcPPA <- readExcel_PC_PPA("./DATA/IMF_PC_PPA_2016.xlsx")
nPPA   <- readExcel_PPA("./DATA/IMF_PC_PPA_2016.xlsx")

clustData <- merge_PPA_RD_Exp_GDP(npcPPA, nPPA, RD_Exp_GDP[,"2013", "AllS"])

clustData$RD_Exp_GDP_BES <- 
  RD_Exp_GDP[dimnames(RD_Exp_GDP)[["Country"]] %in% rownames(clustData),
              "2013", "BES"]

clustData$RD_Exp_GDP_GS <- 
  RD_Exp_GDP[dimnames(RD_Exp_GDP)[["Country"]] %in% rownames(clustData),
             "2013", "GS"]

clustData$RD_Exp_GDP_HES <- 
  RD_Exp_GDP[dimnames(RD_Exp_GDP)[["Country"]] %in% rownames(clustData),
             "2013", "HES"]

clustData$RD_Exp_GDP_PNPS <- 
  RD_Exp_GDP[dimnames(RD_Exp_GDP)[["Country"]] %in% rownames(clustData),
             "2013", "PNPS"]

clustData$RD_pGERD_BES <- 
  RD_RD_pGERD[dimnames(RD_RD_pGERD)[["Country"]] %in% rownames(clustData),
              "2013", "BES"]

clustData$RD_pGERD_GS <- 
  RD_RD_pGERD[dimnames(RD_RD_pGERD)[["Country"]] %in% rownames(clustData),
              "2013", "GS"]

clustData$RD_pGERD_HES <- 
  RD_RD_pGERD[dimnames(RD_RD_pGERD)[["Country"]] %in% rownames(clustData),
              "2013", "HES"]

clustData$RD_pGERD_PNPS <- 
  RD_RD_pGERD[dimnames(RD_RD_pGERD)[["Country"]] %in% rownames(clustData),
              "2013", "PNPS"]

clustData$RD_pGERD_Abroad <- 
  RD_RD_pGERD[dimnames(RD_RD_pGERD)[["Country"]] %in% rownames(clustData),
              "2013", "Abroad"]

clustData$color <- log(clustData$RD_pGERD_BES+1)
clustData$color[is.na(clustData$color)] <- 0

for(col in grep("^RD", names(clustData))) {
  clustData[is.na(clustData[, col]), col] <- 0
}

clustData$AbsGDP <- clustData$PPA * clustData$Exp_GDP / 100

# Get rid of NAs
clustData <- na.omit(clustData)

# Scale
# col2Scale <- c("pcPPA", "PPA", "AbsGDP")
col2Scale <- names(clustData)
clustData2 <- clustData
# varsDisp <- c("Exp_GDP", "pcPPA", "color", "AbsGDP")
varsDisp <- c("Exp_GDP", "pcPPA", "color")
for (c2s in varsDisp) {
  d <- abs(max(clustData[, c2s], na.rm = TRUE) - min(clustData[, c2s], na.rm = TRUE))
  clustData2[, c2s] <- (clustData[, c2s] - min(clustData[, c2s], na.rm = TRUE)) / d
}

set.seed(3)

kmObj <- kmeans(clustData2[, varsDisp], centers = 6, nstart = 36)

x<-sapply(1:10, function(i) {
  kmObj <- kmeans(clustData2[, varsDisp], centers = i, nstart = 36)
  return(c(i, kmObj$betweenss / kmObj$totss))
})
diff(x[2,], 3)
kmObj <- kmeans(clustData2[, varsDisp], centers = 3, nstart = 36)

clustData$cluster <- fitted(kmObj, "classes")


mydata <- clustData2[, varsDisp] 
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var)) 
ncenters <- 15
for (i in 2:ncenters) 
  wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
plot(1:ncenters, wss, type="b", 
     xlab="Number of Clusters", ylab="Within groups sum of squares")

# Unscale
for (c2s in varsDisp) {
  d <- abs(max(clustData[, c2s], na.rm = TRUE) - min(clustData[, c2s], na.rm = TRUE))
  #clustData[, c2s] <- clustData[, c2s] * d + min(clustData[, c2s], na.rm = TRUE)
  kmObj$centers[, c2s] <- kmObj$centers[, c2s] * d + min(clustData[, c2s], na.rm = TRUE)
}

plot(clustData$Exp_GDP, clustData$pcPPA,
     col = clustData$cluster, pch = clustData$cluster,
     xlab = "R&D Expenditure as %GDP (2013)",
     ylab = "Per Capita PPA (2016)")
clust_lm <- lm(pcPPA ~ Exp_GDP, clustData)
abline(reg = clust_lm)
abline(h = mean(clustData$pcPPA, na.rm = TRUE), lty = 2)
abline(v = mean(clustData$Exp_GDP, na.rm = TRUE), lty = 2)
q_PPA     <- quantile(clustData$pcPPA, na.rm = TRUE)
q_Exp_GDP <- quantile(clustData$Exp_GDP, na.rm = TRUE)
abline(h = q_PPA["25%"], lty = 3)
abline(h = q_PPA["75%"], lty = 3)
abline(v = q_Exp_GDP["25%"], lty = 3)
abline(v = q_Exp_GDP["75%"], lty = 3)



# PLOT --------------------------------------------------------------------

library(broom)
library(dplyr)

aux         <- augment(clust_lm)
# aux$Country <- rownames(na.omit(clustData))
# myFit       <- arrange(aux, pcPPA)
myFit <- arrange(aux, Exp_GDP)
myFit$Country <- myFit$.rownames
myFit$f <- myFit$.fitted
myFit <- cbind(myFit, with(myFit, list(Low = .fitted - 2*.se.fit, 
                                       High = .fitted + 2*.se.fit)))

clustData$Country <- rownames(clustData) 

q_PPA     <- quantile(clustData$pcPPA, na.rm = TRUE)
q_Exp_GDP <- quantile(clustData$Exp_GDP, na.rm = TRUE)

clustData$RD_pGERD_BES[is.na(clustData$RD_pGERD_BES)] <- 1
clustData$AbsGDP[is.na(clustData$AbsGDP)] <- 0

## PLOT
# Colors
# colors <- c("#FB1108","#FD150B","#FA7806","#FBE426","#FCFB8F",
#             "#F3F5E7", "#C7E4EA","#ABD6E6","#9AD2E1")
colors <- c("Red", "Orange", "Yellow", "Green", "Cyan", "Blue", "Violet")

# clustData$color <- colorize(log(clustData$RD_pGERD_BES), 
#                                  colors = colors)
clustData$color <- colorize(clustData$color, colors = colors)
clustData$color[is.na(clustData$color)] <- 0

# Sizes
clustData$size <- clustData$AbsGDP

# Format tooltip_table for scattered series
x <- c("", "R&D Expenses (%GDP)", "Private R&D Exp. Share (%GERD)", "Per Capita PPP (Int$)", "PPP (MInt$)")
y <- c("{point.Country}", sprintf("{point.%s:.2f}", c("Exp_GDP", "RD_pGERD_BES", "pcPPA", "PPA")))
tltip <- tooltip_table(x, y, title = "DATA")

# Format tooltip_table for fit series
x <- c("", "R&D Expenses (%GDP)", "Per Capita PPP (Int$)")
y <- c("{point.Country}", sprintf("{point.%s:.2f}", c("Exp_GDP",  "f")))
tltipFit <- tooltip_table(x, y)

# Format tooltip_table for CI
x <- c("", "R&D Expenses (%GDP)", "High", "Per Capita PPP (Int$) Fit","Low")
y <- c("{point.Country}", sprintf("{point.%s:.2f}", 
                                  c("Exp_GDP", "High",  "f",  "Low")))
tltipCI <- tooltip_table(x, y, title = "Linear Fit & 95% CI")

# Get rid of NAs
clustData <- na.omit(clustData)

# Plot
library(highcharter)
hc <- highchart() %>% 
  # hc_chart(backgroundColor = "black") %>% 
  hc_tooltip(useHTML      = TRUE,
             shared       = FALSE,
             headerFormat = "") %>%
  hc_add_series(myFit,
                type = "line",
                hcaes(x = Exp_GDP, 
                      y = .fitted),
                enableMouseTracking = FALSE, ####
                findNearestPointBy = "xy",
                name = "Per capita PPP ~ R&D Exp. Linear Fit",
                color = "Blue",
                id = "fit",
                lineWidth = 1,
                tooltip = list(pointFormat  = tltipFit),
                zIndex = 1) %>%
  hc_add_series(myFit, type = "arearange",
                hcaes(x    = Exp_GDP, 
                      low  = Low,
                      high = High),
                enableMouseTracking = TRUE, ####
                name = "95% CI",
                linkedTo = "fit",  ####
                findNearestPointBy = "xy",
                lineWidth = 0,
                color = "Gray",
                fillOpacity = 0,
                zIndex = 0,
                visible = TRUE,
                tooltip = list(pointFormat  = tltipCI)) %>%
  hc_add_series(clustData, "scatter", 
                hcaes(x = Exp_GDP, 
                      y = pcPPA, 
                      #color = color,
                      size = size,
                      group = factor(cluster, order = TRUE)),
                marker = list(symbol = 'triangle'),
                name = "R&D Expenditure",
                tooltip = list(pointFormat  = tltip,
                               valueSuffix = "K Int.$")) %>%
  hc_title(text = "Per Capita GDP vs. R&D Effort") %>% 
  hc_subtitle(text = "with R&D Expenditure and Private R&D Share") %>% 
  hc_chart(zoomType = "xy") %>%
  hc_yAxis(title = list(text = "Per Capita PPP (2016) - K Int. Dollars"),
           gridLineWidth = 0,
           plotLines = list(list(label = list(text = "c25"),
                                 value = as.numeric(q_PPA["25%"]),
                                 color = "#FF0000",
                                 width = 1,
                                 dashStyle = "ShortDot"),
                            list(label = list(text = "c75"),
                                 value = as.numeric(q_PPA["75%"]),
                                 color = "#FF0000",
                                 width = 1,
                                 dashStyle = "ShortDot"))) %>%
  hc_xAxis(title = list(text = "%GDP (2013)"),
           plotLines = list(list(label = list(text = "c25"),
                                 value = as.numeric(q_Exp_GDP["25%"]),
                                 color = "#FF0000",
                                 width = 1,
                                 dashStyle = "ShortDot"),
                            list(label = list(text = "c75"),
                                 value = as.numeric(q_Exp_GDP["75%"]),
                                 color = "#FF0000",
                                 width = 1,
                                 dashStyle = "ShortDot"))) 
#%>% 
 # hc_add_theme(hc_theme_chalk())
#hc_add_theme(hc_theme_darkunica())

hc
