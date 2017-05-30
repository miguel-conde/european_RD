source("hc_functions.R")

source("readExcelData.R")


### Gasto en I+D por países - 2014
RD_Exp_GDP <- get_RD_Exp_GDP()
RD_pGERD   <- get_RD_RD_pGERD()
secExpRD <- c("AllS")
stackBP_Year_ExpRD <- stackBP_Year(RD_Exp_GDP, "2014", 
                                   sectors_stack = secExpRD)
secPGERD <- c("Abroad", "HES", "GS", "PNPS",  "BES")
stackBP_Year_pGERD <- stackBP_Year(RD_pGERD, "2014",
                                   sectors_stack = secPGERD)

countries <- intersect(rownames(stackBP_Year_ExpRD), 
                       rownames(stackBP_Year_pGERD))
commStackBP_Year_ExpRD <- stackBP_Year_ExpRD[countries, , drop = FALSE]
commStackBP_Year_pGERD <- stackBP_Year_pGERD[countries, , drop = FALSE]

kk1 <- commStackBP_Year_ExpRD[order(RD_Exp_GDP[countries, "2014", "AllS"], 
                                    decreasing = TRUE), , drop = FALSE]
hc <- hc_stackBP(kk1, title = "Gasto en I+D por países - 2014", 
                 labX = "", labY = "%PIB", 
                 seriesNames = "Gasto Total en I+D")
# hc <- hc %>% hc_plotOptions(series = list(zoneAxis = list(value = 2, 
#                                                        className = "zone-0",
#                                                        color = colors()[2])))
# hc <- hc %>% hc_series(zoneAxis = "x", zones = list(list(value = 2,
#                                                          color = colors()[2])))
hc
library(htmlwidgets)
saveWidget(hc, file = "RD_Exp_by_country_2014.html", selfcontained = TRUE)
save(hc, file = "./WIDGETS/RD_Exp_by_country_2014.Rda")

### Gasto en I+D por países, desglosado y ordenado por pGERD BES - 2013
secExpRD <- c("GS", "HES", "PNPS", "BES")
stackBP_Year_ExpRD <- stackBP_Year(RD_Exp_GDP, "2013", 
                                   sectors_stack = secExpRD)
secPGERD <- c("Abroad", "HES", "GS", "PNPS",  "BES")
stackBP_Year_pGERD <- stackBP_Year(RD_pGERD, "2013",
                                   sectors_stack = secPGERD)

countries <- intersect(rownames(stackBP_Year_ExpRD), 
                       rownames(stackBP_Year_pGERD))
commStackBP_Year_ExpRD <- stackBP_Year_ExpRD[countries, ]
commStackBP_Year_pGERD <- stackBP_Year_pGERD[countries, ]


kk1 <- commStackBP_Year_ExpRD[order(RD_pGERD[countries, "2013", "BES"], decreasing = TRUE), ]
hc <- hc_stackBP(kk1, 
                 title = "Gasto en I+D - 2013",
                 subtitle = "Ordenado según la aportación de fondos BES",
           labX = "", labY = "Gasto Total en I+D - % PIB", 
           seriesNames = secExpRD)
hc
save(hc, file = "./WIDGETS/RD_Exp_by_country_sector_2013.Rda")

saveWidget(hc, 
           file = "RD_Exp_by_country_sector_2013.html", 
           selfcontained = TRUE) 

kk2 <- commStackBP_Year_pGERD[order(RD_Exp_GDP[countries, "2013", "AllS"], decreasing = TRUE), ]
hc <- hc_stackBP(kk2, 
                 title = "Origen de fondos I+D - 2013",
                 subtitle = "Ordenado por Gasto Total en I+D",
           labX = "", labY = "Origen de fondos I+D - % Gasto Total I+D", 
           seriesNames = secPGERD)
hc

save(hc, file = "./WIDGETS/RD_pGERD_by_country_sector_2013.Rda")

saveWidget(hc, 
           file = "RD_pGERD_by_country_sector_2013.html", 
           selfcontained = TRUE) 

### All sectors Research and Development expenditure - % of GDP
library(DT)
d_t <- datatable(RD_Exp_GDP[,,"AllS"], 
                 extensions = 'Buttons', 
                 options = list(pageLength = 10,
                                columns = list(orederable = FALSE),
                                dom = 'Bfrtip',
                                buttons = c('copy', 'csv', 'excel', 'pdf', 'print')),
                 caption = 'Table 1: All sectors Research and Development expenditure - % of GDP (Source: Eurostat)')

saveWidget(d_t, file = "dt_RD_Exp.html", selfcontained = TRUE)
save(d_t, file = "./WIDGETS/dt_RD_Exp.Rda")

### Business enterprise sector %GERD
d_t <- datatable(RD_pGERD[,,"BES"], 
                 extensions = 'Buttons',
                 options = list(pageLength = 10,
                                columns = list(orederable = FALSE),
                                dom = 'Bfrtip',
                                buttons = c('copy', 'csv', 'excel', 'pdf', 'print')),
                 caption = 'Table 2: Business enterprise sector %GERD (Source: Eurostat)')

saveWidget(d_t, file = "dt_RD_pGERD.html", selfcontained = TRUE)
save(d_t, file = "./WIDGETS/dt_RD_pGERD.Rda")

### Scatter Plot
npcPPA <- readExcel_PC_PPA("./DATA/IMF_PC_PPA_2016.xlsx")
nPPA   <- readExcel_PPA("./DATA/IMF_PC_PPA_2016.xlsx")

PPA_RD_Exp_GDP <- merge_PPA_RD_Exp_GDP(npcPPA, nPPA, RD_Exp_GDP[,"2013", "AllS"])
PPA_RD_Exp_GDP$RD_pGERD_BES <- 
  RD_pGERD[dimnames(RD_pGERD)[["Country"]] %in% rownames(PPA_RD_Exp_GDP),
              "2013", "BES"]
PPA_RD_Exp_GDP$AbsGDP <- PPA_RD_Exp_GDP$PPA * PPA_RD_Exp_GDP$Exp_GDP / 100

PPA_RD_Exp_GDP_lm <- lm(pcPPA ~ Exp_GDP, PPA_RD_Exp_GDP)

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
#x <- c("", "R&D Expenses (%GDP)", "Private R&D Exp. Share (%GERD)", "Per Capita PPP (Int$)", "PPP (MInt$)")
x <- c("", 
       "Gasto Total I+D (%PIB)", 
       "% I+D Empresas Comerciales (% Gasto Total I+D)", 
       "PIB per capita (PPA, Intl$)", 
       "Gasto Total I+D (PPA, MInt$)")
y <- c("{point.Country}", 
       sprintf("{point.%s:.2f}", 
               c("Exp_GDP", "RD_pGERD_BES", "pcPPA", "AbsGDP")))
tltip <- tooltip_table(x, y, title = "DATA")

# Format tooltip_table for fit series
# x <- c("", "R&D Expenses (%GDP)", "Per Capita PPP (Int$)")
x <- c("", "Gasto Total I+D (%PIB)", "PIB per capita (PPA, Intl$)")
y <- c("{point.Country}", sprintf("{point.%s:.2f}", c("Exp_GDP",  "f")))
tltipFit <- tooltip_table(x, y)

# Format tooltip_table for CI
# x <- c("", "R&D Expenses (%GDP)", "High", "Per Capita PPP (Int$) Fit","Low")
x <- c("", "Gasto Total I+D (%PIB)", "High", "PIB per capita (PPA, Intl$) Fit","Low")
y <- c("{point.Country}", sprintf("{point.%s:.2f}", 
                                  c("Exp_GDP", "High",  "f",  "Low")))
tltipCI <- tooltip_table(x, y, title = "Linear Fit & 95% CI")

# Get rid of NAs
PPA_RD_Exp_GDP <- na.omit(PPA_RD_Exp_GDP)


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
                # name = "Per capita GDP ~ R&D Exp. Linear Fit",
                name = "Modelo lineal PIB per capita ~ Gasto Total I+D",
                color = "Blue",
                id = "fit",
                lineWidth = 1,
                tooltip = list(pointFormat  = tltipFit),
                zIndex = 1,
                visible = FALSE) %>%
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
                visible = FALSE,
                tooltip = list(pointFormat  = tltipCI)) %>%
  hc_add_series(PPA_RD_Exp_GDP, "scatter", 
                hcaes(x = Exp_GDP, 
                      y = pcPPA, 
                      color = color,
                      size = size),
                # name = "R&D Expenditure",
                name = "Gasto Total I+D",
                tooltip = list(pointFormat  = tltip,
                               valueSuffix = "K Int.$")) %>%
  # hc_title(text = "Per Capita GDP vs. R&D Effort") %>% 
  # hc_subtitle(text = "with R&D Expenditure and Private R&D Share") %>% 
  hc_title(text = "PIB per capita vs. Intensidad Inversión I+D") %>% 
  hc_subtitle(text = "con Gasto en I+D y Cuota del Sector Privado") %>%
  hc_chart(zoomType = "xy") %>%
  hc_yAxis(title = list(text = "PIB per capita (PPA, 2016) - K Intl. Dollars"),
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

saveWidget(hc, 
           file = "pcPPP-GERD.html", 
           selfcontained = TRUE) 


### SPAIN
secExpRD <- c("GS", "HES", "BES", "PNPS")
stackBP_Country_ExpRD <- stackBP_Country(RD_Exp_GDP, "Spain", 
                                         sectors_stack = secExpRD)
secPGERD <- c("Abroad", "HES", "GS", "PNPS",  "BES")
stackBP_Country_pGERD <- stackBP_Country(RD_pGERD, "Spain",
                                         sectors_stack = secPGERD)

# RD_Exp_GDP
hc <- hc_stackBP(stackBP_Country_ExpRD, 
                 title = "Gasto en I+D desglosado por sectores en España", 
           labX = "", labY = "% PIB", 
           # seriesNames = secExpRD, 
           sumLine = TRUE)
hc

save(hc, file = "./WIDGETS/RD_Exp_Spain_by_sector_2013.Rda")

saveWidget(hc, 
           file = "RD_Exp_Spain_by_sector_2013.html", 
           selfcontained = TRUE) 


# RD_pGERD
hc <- hc_stackBP(stackBP_Country_pGERD, 
           title = "% de financiación, por sectores, del gasto en I+D - España", 
           labX = "", labY = "% Gasto Total en I+D")
hc

save(hc, file = "./WIDGETS/RD_pGERD_Spain_by_sector_2013.Rda")

saveWidget(hc, 
           file = "RD_pGERD_Spain_by_sector_2013.html", 
           selfcontained = TRUE) 

## Números absolutos SPAIN
spainData <- readExcelSpain()
hc <- hc_stackBP(spainData[, 2:5],
           title = "Gasto absoluto en I+D desglosado por sectores en España", 
           labX = "", labY = "International MDollars", 
           # seriesNames = secExpRD, 
           sumLine = TRUE)
hc
save(hc, file = "./WIDGETS/RD_AbsExp_Spain_by_sector.Rda")

saveWidget(hc, 
           file = "RD_AbsExp_Spain_by_sector.html", 
           selfcontained = TRUE) 

hc <- hc_stackBP(spainData[-nrow(spainData), 6:10],
                 title = "Origen de Fondos I+D desglosado por sectores en España", 
                 labX = "", labY = "International MDollars", 
                 # seriesNames = secExpRD, 
                 sumLine = TRUE)
hc
save(hc, file = "./WIDGETS/RD_AbspGERD_Spain_by_sector.Rda")

saveWidget(hc, 
           file = "RD_AbspGERD_Spain_by_sector.html", 
           selfcontained = TRUE) 

### Semipublic sources of RD funds
tmp <- as.data.frame(RD_pGERD[,"2013",c("HES", "GS"), drop = FALSE])
tmp <- tmp[apply(tmp, 1, function(x) sum(is.na(x)) < 2), ]
hc <- hc_stackBP(tmp[order(tmp[,2]+tmp[,1], decreasing = TRUE),],
                 title = 'Orígenes "Publicos" de fondos para I+D',
                 labY = "% Gasto Total en I+D", sumLine = FALSE)
hc
save(hc, file = "./WIDGETS/RD_PubpGERD_by_sector.Rda")

saveWidget(hc, 
           file = "RD_PubpGERD_by_sector.html", 
           selfcontained = TRUE) 

