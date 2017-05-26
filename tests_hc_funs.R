source("hc_functions.R")

source("readExcelData.R")
RD_Exp_GDP <- get_RD_Exp_GDP()
RD_pGERD   <- get_RD_RD_pGERD()


### ###
# BARPLOTS
## stackBP_Country()
RD_Exp_GDP <- get_RD_Exp_GDP()
RD_pGERD   <- get_RD_RD_pGERD()

# RD_Exp_GDP
stackBP_Country_ExpRD <- stackBP_Country(RD_Exp_GDP, "Spain")
stackBP_Country_ExpRD

stackBP_Country_ExpRD <- stackBP_Country(RD_Exp_GDP, "Spain", 
                                         sectors_stack = c("BES", "GS"))
stackBP_Country_ExpRD
stackBP_Country_ExpRD <- stackBP_Country(RD_Exp_GDP, "Spain", 
                                         sectors_stack = c("BES"))
stackBP_Country_ExpRD

stackBP_Country_ExpRD <- stackBP_Country(RD_Exp_GDP, "Spain", 
                                         sectors_stack = c("BES", "GS"),
                                         order_stack_by = c("GS", "BES"))
stackBP_Country_ExpRD

# RD_pGERD
stackBP_Country_pGERD <- stackBP_Country(RD_pGERD, "Spain")
stackBP_Country_pGERD

stackBP_Country_pGERD <- stackBP_Country(RD_pGERD, "Spain", 
                                         sectors_stack = c("BES", "GS"))
stackBP_Country_pGERD
stackBP_Country_pGERD <- stackBP_Country(RD_pGERD, "Spain", 
                                         sectors_stack = c("BES"))
stackBP_Country_pGERD

stackBP_Country_pGERD <- stackBP_Country(RD_pGERD, "Spain", 
                                         sectors_stack = c("BES", "GS"),
                                         order_stack_by = c("GS", "BES"))
stackBP_Country_pGERD

## hc_stackBP()
secExpRD <- c("GS", "HES", "BES", "PNPS")
stackBP_Country_ExpRD <- stackBP_Country(RD_Exp_GDP, "Spain", 
                                         sectors_stack = secExpRD)
secPGERD <- c("Abroad", "HES", "GS", "PNPS",  "BES")
stackBP_Country_pGERD <- stackBP_Country(RD_pGERD, "Spain",
                                         sectors_stack = secPGERD)

# RD_Exp_GDP
hc_stackBP(stackBP_Country_ExpRD, title = "", labX = "", labY = "", 
                   seriesNames = NULL)
hc_stackBP(stackBP_Country_ExpRD, title = "", labX = "", labY = "", 
                   seriesNames = secExpRD)
hc_stackBP(stackBP_Country_ExpRD, title = "SPAIN", 
                   labX = "TIME", labY = "Exp. R&D", 
                   seriesNames = secExpRD)
hc_stackBP(stackBP_Country_ExpRD, title = "SPAIN", 
           labX = "TIME", labY = "Exp. R&D", 
           seriesNames = secExpRD, sumLine = TRUE)

# RD_pGERD
hc_stackBP(stackBP_Country_pGERD, title = "", labX = "", labY = "", 
                   seriesNames = NULL)
hc_stackBP(stackBP_Country_pGERD, title = "", labX = "", labY = "", 
                   seriesNames = secPGERD)
hc_stackBP(stackBP_Country_pGERD, title = "SPAIN", 
                   labX = "TIME", labY = "Exp. R&D", 
                   seriesNames = secPGERD)


## stackBP_Year()
# RD_Exp_GDP

stackBP_Year_ExpRD <- stackBP_Year(RD_Exp_GDP, "2013")
stackBP_Year_ExpRD

stackBP_Year_ExpRD <- stackBP_Year(RD_Exp_GDP, "2013", 
                                   sectors_stack = c("BES", "GS"))
stackBP_Year_ExpRD
stackBP_Year_ExpRD <- stackBP_Year(RD_Exp_GDP, "2013", 
                                   sectors_stack = c("BES"))
stackBP_Year_ExpRD

stackBP_Year_ExpRD <- stackBP_Year(RD_Exp_GDP, "2013", 
                                   sectors_stack = c("BES", "GS"),
                                   order_stack_by = c("GS", "BES"))
stackBP_Year_ExpRD

# RD_pGERD
stackBP_Year_pGERD <- stackBP_Year(RD_pGERD, "2013")
stackBP_Year_pGERD

stackBP_Year_pGERD <- stackBP_Year(RD_pGERD, "2013", 
                                   sectors_stack = c("BES", "GS"))
stackBP_Year_pGERD
stackBP_Year_pGERD <- stackBP_Year(RD_pGERD, "2013", 
                                   sectors_stack = c("BES"))
stackBP_Year_pGERD

stackBP_Year_pGERD <- stackBP_Year(RD_pGERD, "2013", 
                                   sectors_stack = c("BES", "GS"),
                                   order_stack_by = c("GS", "BES"))
stackBP_Year_pGERD

## hc_stackBP()
secExpRD <- c("GS", "HES", "BES", "PNPS")
stackBP_Year_ExpRD <- stackBP_Year(RD_Exp_GDP, "2013", 
                                         sectors_stack = secExpRD)
secPGERD <- c("Abroad", "HES", "GS", "PNPS",  "BES")
stackBP_Year_pGERD <- stackBP_Year(RD_pGERD, "2013",
                                         sectors_stack = secPGERD)
# RD_Exp_GDP
hc_stackBP(stackBP_Year_ExpRD, title = "", labX = "", labY = "", 
                   seriesNames = NULL)
hc_stackBP(stackBP_Year_ExpRD, title = "", labX = "", labY = "", 
                   seriesNames = secExpRD)
hc_stackBP(stackBP_Year_ExpRD, title = "2013", 
                   labX = "TIME", labY = "Exp. R&D", 
                   seriesNames = secExpRD)

# RD_pGERD
hc_stackBP(stackBP_Year_pGERD, title = "", labX = "", labY = "", 
                   seriesNames = NULL)
hc_stackBP(stackBP_Year_pGERD, title = "", labX = "", labY = "", 
                   seriesNames = secPGERD)
hc_stackBP(stackBP_Year_pGERD, title = "2013", 
                   labX = "TIME", labY = "Exp. R&D", 
                   seriesNames = secPGERD)

## ORDER
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
hc_stackBP(kk1, title = "2013", 
           labX = "TIME", labY = "Exp. R&D", 
           seriesNames = secExpRD)
kk2 <- commStackBP_Year_pGERD[order(RD_Exp_GDP[countries, "2013", "AllS"], decreasing = TRUE), ]
hc_stackBP(kk2, title = "2013", 
           labX = "TIME", labY = "Exp. R&D", 
           seriesNames = secPGERD)


### BLOG
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
hc
library(htmlwidgets)
saveWidget(hc, file = "RD_Exp_by_country_2014.html", selfcontained = TRUE)

### Time Series
## mtsCS()
mtsCS_exp_RD <- mtsCS(RD_Exp_GDP, sector = "AllS")
mtsCS_pGERD  <- mtsCS(RD_pGERD,  sector = "BES")

## hc_mtsCS()
hc_mtsCS(mtsCS_exp_RD, title = "Título exp_RD", labX = "Year", labY = "AllS")
hc_mtsCS(mtsCS_pGERD, title = "Título pGERD", labX = "Year", labY = "BES")

sampleC <- c("Luxembourg", "Ireland", "Iceland", "Norway", "Netherlands")

hc_mtsCS(mtsCS_exp_RD[,sampleC], title = "Título exp_RD", labX = "Year", labY = "AllS")
hc_mtsCS(mtsCS_pGERD[,sampleC], title = "Título pGERD", labX = "Year", labY = "BES")
