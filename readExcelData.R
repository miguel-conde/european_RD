library(readxl)

readExcel_RD_Exp_GDP <- function(sheet) {
  out <-
    read_excel("D:/PROYECTOS DATA SCIENCE/european_RD/DATA/RD expenditure by sectors of performance.xlsx", 
               sheet = sheet, col_types = c("text", 
                                            "text", "skip", "text", "skip",
                                            "text", "skip", "text", "skip",
                                            "text", "skip", "text", "skip",
                                            "text", "skip", "text", "skip",
                                            "text", "skip", "text", "skip",
                                            "text", "skip", "text", "skip"),
               skip = 3)
  
  out <- out[-c(45:53), ]
  
  out[, -1] <- sapply(out[, -1], function(x) {
    x[x == ":"] <- NA
    as.numeric(x)
  })
  
  names(out)[1] <- "geo"
  out$geo[out$geo == "Former Yugoslav Republic of Macedonia, the"] <- "FYRM"
  out$geo[out$geo == "China (except Hong Kong)"] <- "China"
  
  as.data.frame(out)
}

get_RD_Exp_GDP <- function() {
  RD_Exp_GDP <- readExcel_RD_Exp_GDP("All Sectors")
  
  RD_Exp_GDP <-
    array(data = c(unlist(readExcel_RD_Exp_GDP("All Sectors")[,-1]),
                   unlist(readExcel_RD_Exp_GDP("Business enterprise sector")[,-1]),
                   unlist(readExcel_RD_Exp_GDP("Government sector")[,-1]),
                   unlist(readExcel_RD_Exp_GDP("Higher education sector")[,-1]),
                   unlist(readExcel_RD_Exp_GDP("Private non-profit sector")[,-1])),
          dim = c(nrow(RD_Exp_GDP),
                  ncol(RD_Exp_GDP)-1,
                  5),
          dimnames = list(Country = RD_Exp_GDP$geo,
                          Year = 2004:2015,
                          Sector = c("AllS", "BES", "GS", "HES", "PNPS")))
  RD_Exp_GDP
}

readExcel_RD_pGERD <- function(sheet) {
  out <-
    read_excel("D:/PROYECTOS DATA SCIENCE/european_RD/DATA/Gross domestic expenditure on R&D (GERD) by source of funds.xlsx", 
               sheet = sheet, col_types = c("text", 
                                            "text", "skip", "text", "skip",
                                            "text", "skip", "text", "skip",
                                            "text", "skip", "text", "skip",
                                            "text", "skip", "text", "skip",
                                            "text", "skip", "text", "skip",
                                            "text", "skip", "text", "skip"),
               skip = 3)
  
  out <- out[-c(46:54), ]
  
  out[, -1] <- sapply(out[, -1], function(x) {
    x[x == ":"] <- NA
    as.numeric(x)
  })
  
  names(out)[1] <- "geo"
  out$geo[out$geo == "Former Yugoslav Republic of Macedonia, the"] <- "FYRM"
  out$geo[out$geo == "China (except Hong Kong)"] <- "China"
  
  as.data.frame(out)
}

get_RD_RD_pGERD <- function() {
  RD_pGERD <- readExcel_RD_pGERD("Business enterprise sector")
  
  RD_pGERD <-
    array(data = c(unlist(readExcel_RD_pGERD("Business enterprise sector")[,-1]),
                   unlist(readExcel_RD_pGERD("Government sector")[,-1]),
                   unlist(readExcel_RD_pGERD("Higher education sector")[,-1]),
                   unlist(readExcel_RD_pGERD("Private non-profit sector")[,-1]),
                   unlist(readExcel_RD_pGERD("Abroad")[,-1])),
          dim = c(nrow(RD_pGERD),
                  ncol(RD_pGERD)-1,
                  5),
          dimnames = list(Country = RD_pGERD$geo,
                          Year = 2004:2015,
                          Sector = c("BES", "GS", "HES", "PNPS", "Abroad")))
  RD_pGERD
}


readExcel_PC_PPA <- function(file) {
  out <- read_excel(file, skip = 1)
  out
}

readExcel_PPA <- function(file) {
  out <- read_excel(file, skip = 1, sheet = "PPP")
  out
}

merge_PPA_RD_Exp_GDP <- function(npcPPA, nPPA, nRD_Exp_GDP) {
  out <- data.frame(Exp_GDP = nRD_Exp_GDP)
  out$pcPPA <- rep(NA, nrow(out))
  out$PPA <- rep(NA, nrow(out))
  
  for (country in names(nRD_Exp_GDP)) {
    idx <- grep(country, npcPPA$Country)
    if(length(idx == 1)) {
      out[country, "pcPPA"] <- as.numeric(npcPPA[idx, "IntDollars"])
    }
  }
  
  for (country in names(nRD_Exp_GDP)) {
    idx <- grep(country, nPPA$Country)
    if(length(idx == 1)) {
      out[country, "PPA"] <-   as.numeric(nPPA[idx, "GDP"])
    }
  }
  
  out
}