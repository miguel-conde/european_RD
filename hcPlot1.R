source("readExcelData.R")

RD_Exp_GDP  <- get_RD_Exp_GDP()
RD_RD_pGERD <- get_RD_RD_pGERD()

npcPPA <- readExcel_PC_PPA("./DATA/IMF_PC_PPA_2016.xlsx")
nPPA   <- readExcel_PPA("./DATA/IMF_PC_PPA_2016.xlsx")

PPA_RD_Exp_GDP <- merge_PPA_RD_Exp_GDP(npcPPA, nPPA, RD_Exp_GDP[,"2013", "AllS"])
PPA_RD_Exp_GDP$RD_pGERD_BES <- 
  RD_RD_pGERD[dimnames(RD_RD_pGERD)[["Country"]] %in% rownames(PPA_RD_Exp_GDP),
              "2013", "BES"]
PPA_RD_Exp_GDP$AbsGDP <- PPA_RD_Exp_GDP$PPA * PPA_RD_Exp_GDP$Exp_GDP / 100

PPA_RD_Exp_GDP_lm <- lm(pcPPA ~ Exp_GDP, PPA_RD_Exp_GDP)

plot(PPA_RD_Exp_GDP$Exp_GDP, PPA_RD_Exp_GDP$pcPPA,
     xlab = "R&D Expenditure as %GDP (2013)",
     ylab = "Per Capita PPA (2016)")
abline(reg = PPA_RD_Exp_GDP_lm)
abline(h = mean(PPA_RD_Exp_GDP$pcPPA, na.rm = TRUE), lty = 2)
abline(v = mean(PPA_RD_Exp_GDP$Exp_GDP, na.rm = TRUE), lty = 2)

library(broom)
library(dplyr)

aux         <- augment(PPA_RD_Exp_GDP_lm)
# aux$Country <- rownames(na.omit(PPA_RD_Exp_GDP))
# myFit       <- arrange(aux, pcPPA)
myFit <- arrange(aux, Exp_GDP)
myFit$Country <- myFit$.rownames
myFit$f <- myFit$.fitted
myFit <- cbind(myFit, with(myFit, list(Low = .fitted - 2*.se.fit, 
                                       High = .fitted + 2*.se.fit)))

PPA_RD_Exp_GDP$Country <- rownames(PPA_RD_Exp_GDP) 

q_PPA     <- quantile(PPA_RD_Exp_GDP$pcPPA, na.rm = TRUE)
q_Exp_GDP <- quantile(PPA_RD_Exp_GDP$Exp_GDP, na.rm = TRUE)

PPA_RD_Exp_GDP$RD_pGERD_BES[is.na(PPA_RD_Exp_GDP$RD_pGERD_BES)] <- 1
PPA_RD_Exp_GDP$AbsGDP[is.na(PPA_RD_Exp_GDP$AbsGDP)] <- 0

## PLOT
library(highcharter)
# Colors
# colors <- c("#FB1108","#FD150B","#FA7806","#FBE426","#FCFB8F",
#             "#F3F5E7", "#C7E4EA","#ABD6E6","#9AD2E1")
# colors <- c("Red", "Orange", "Yellow", "Green", "Cyan", "Blue", "Violet")
colors <- rainbow(7)

PPA_RD_Exp_GDP$color <- colorize(log(PPA_RD_Exp_GDP$RD_pGERD_BES), 
                                 colors = colors)
PPA_RD_Exp_GDP$color[is.na(PPA_RD_Exp_GDP$color)] <- 0

# Sizes
PPA_RD_Exp_GDP$size <- PPA_RD_Exp_GDP$AbsGDP

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
PPA_RD_Exp_GDP <- na.omit(PPA_RD_Exp_GDP)

# Plot


hchart(PPA_RD_Exp_GDP, "scatter", 
       hcaes(x = Exp_GDP, y = pcPPA, color = color, size = size),
       name = "R&D Expenditure as %GDP") %>% 
  hc_chart(backgroundColor = "black") %>% 
  hc_tooltip(useHTML      = TRUE, 
             share        = FALSE,
             headerFormat = "", 
             pointFormat  = tltip) %>%
  hc_add_series(myFit,
                type = "line",
                hcaes(x = Exp_GDP, y = .fitted),
                enableMouseTracking = FALSE,
                name = "Fit",
                color = "Blue",
                id = "fit",
                lineWidth = 1) %>%
  # hc_add_series(myFit, type = "arearange",
  #               hcaes(x = Exp_GDP, low = .fitted - 2*.se.fit,
  #                     high = .fitted + 2*.se.fit),
  #               linkedTo = "fit") %>%
  hc_title(text = "National Wealth vs. R&D Effort") %>% 
  hc_subtitle(text = "with R&D Expenditure and Private R&D Share") %>% 
  hc_chart(zoomType = "xy") %>%
  hc_yAxis(title = list(text = "Per Capita PPA (2016) - K Int. Dollars"),
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
  hc_xAxis(title = list(text = "R&D Expenditure as %GDP (2013)"),
           plotLines = list(list(label = list(text = "c25"),
                                 value = as.numeric(q_Exp_GDP["25%"]),
                                 color = "#FF0000",
                                 width = 1,
                                 dashStyle = "ShortDot"),
                            list(label = list(text = "c75"),
                                 value = as.numeric(q_Exp_GDP["75%"]),
                                 color = "#FF0000",
                                 width = 1,
                                 dashStyle = "ShortDot"))) %>% 
  hc_add_theme(hc_theme_chalk())
  #hc_add_theme(hc_theme_darkunica())

# Plot 2
highchart() %>% 
  hc_chart(backgroundColor = "black") %>% 
  hc_tooltip(useHTML      = TRUE, 
             share        = FALSE,
             headerFormat = "", 
             pointFormat  = tltip) %>%
  hc_add_series(PPA_RD_Exp_GDP, "scatter", 
                hcaes(x = Exp_GDP, y = pcPPA, color = color, size = size),
                name = "R&D Expenditure") %>%
  hc_add_series(myFit,
                type = "line",
                hcaes(x = Exp_GDP, y = .fitted),
                enableMouseTracking = FALSE,
                name = "Per capita PPP ~ R&D Exp. Linear Fit",
                color = "Blue",
                id = "fit",
                lineWidth = 1) %>%
  # hc_add_series(myFit, type = "arearange",
  #               hcaes(x = Exp_GDP, low = .fitted - 2*.se.fit,
  #                     high = .fitted + 2*.se.fit),
  #               linkedTo = "fit") %>%
  hc_title(text = "National Wealth vs. R&D Effort") %>% 
  hc_subtitle(text = "with R&D Expenditure and Private R&D Share") %>% 
  hc_chart(zoomType = "xy") %>%
  hc_yAxis(title = list(text = "Per Capita PPA (2016) - K Int. Dollars"),
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
                                 dashStyle = "ShortDot"))) %>% 
  hc_add_theme(hc_theme_chalk())
#hc_add_theme(hc_theme_darkunica())

# Plot 3
hc <- highchart() %>% 
  hc_chart(backgroundColor = "black") %>% 
  hc_tooltip(useHTML      = TRUE,
             shared       = FALSE,
             headerFormat = "") %>%
  hc_add_series(myFit,
                type = "line",
                hcaes(x = Exp_GDP, 
                      y = .fitted),
                enableMouseTracking = TRUE,
                findNearestPointBy = "xy",
                name = "Per capita PPP ~ R&D Exp. Linear Fit",
                color = "Blue",
                id = "fit",
                lineWidth = 1,
                tooltip = list(pointFormat  = tltipFit),
                zIndex = 1) %>%
  hc_add_series(myFit, type = "arearange",
                hcaes(x    = Exp_GDP, 
                      low  = .fitted - 2*.se.fit,
                      high = .fitted + 2*.se.fit),
                name = "95% CI",
                findNearestPointBy = "xy",
                lineWidth = 0,
                color = "Gray",
                fillOpacity = 0,
                zIndex = 0,
                visible = FALSE) %>%
  hc_add_series(PPA_RD_Exp_GDP, "scatter", 
                hcaes(x = Exp_GDP, 
                      y = pcPPA, 
                      color = color,
                      size = size),
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
                                 dashStyle = "ShortDot"))) %>% 
  hc_add_theme(hc_theme_chalk())
#hc_add_theme(hc_theme_darkunica())

hc

# SAVE
save(hc, file = "./WIDGETS/hc_pcPPP-GERD.Rda")
library(htmlwidgets)
saveWidget(hc, 
           file = "pcPPP-GERD.html", 
           selfcontained = FALSE) # Quitar el selfcontained


# Plot 4
hc <- highchart() %>% 
  hc_chart(backgroundColor = "black") %>% 
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
                fillOpacity = 0.1,
                zIndex = 0,
                visible = TRUE,
                tooltip = list(pointFormat  = tltipCI)) %>%
  hc_add_series(PPA_RD_Exp_GDP, "scatter", 
                hcaes(x = Exp_GDP, 
                      y = pcPPA, 
                      color = color,
                      size = size),
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
                                 zIndex = 10,
                                 dashStyle = "ShortDot"),
                            list(label = list(text = "c75"),
                                 value = as.numeric(q_PPA["75%"]),
                                 color = "#FF0000",
                                 width = 1,
                                 zIndex = 11,
                                 dashStyle = "ShortDot"))) %>%
  hc_xAxis(title = list(text = "%GDP (2013)"),
           plotLines = list(list(label = list(text = "c25"),
                                 value = as.numeric(q_Exp_GDP["25%"]),
                                 color = "#FF0000",
                                 width = 1,
                                 zIndex = 12,
                                 dashStyle = "ShortDot"),
                            list(label = list(text = "c75"),
                                 value = as.numeric(q_Exp_GDP["75%"]),
                                 color = "#FF0000",
                                 width = 1,
                                 zIndex = 13,
                                 dashStyle = "ShortDot"))) %>% 
  hc_add_theme(hc_theme_chalk())
#hc_add_theme(hc_theme_darkunica())

hc

# SAVE
save(hc, file = "./WIDGETS/hc_pcPPP-GERD.Rda")
library(htmlwidgets)
saveWidget(hc, 
           file = "pcPPP-GERD.html", 
           selfcontained = FALSE) # Quitar el selfcontained
