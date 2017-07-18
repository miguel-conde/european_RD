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

features <- names(clustData)

clustData$cluster <- "MidRD_MidPPP"

countryClasses <- c("LowRD_HighPPPs", "MidRD_HighPPP", "HighRD_HighPPP",
                 "LowRD_MidPPP", "MidRD_HighPPP", "LowRD_LowPPP",
                 "MidRD_LowPPP", "HighRD_LowPPP")

listClusters <- list(
  LowRD_HighPPP_Countries  = c(),
  MidRD_HighPPP_Countries  = c("Luxembourg", "Ireland", "Norway", "Iceland", 
                               "Netherlands"),
  HighRD_HighPPP_Countries = c("United States", "Germany", "Austria", "Denmark", 
                               "Sweden"),
  LowRD_MidPPP_Countries   = c("Cyprus", "Malta"),
  MidRD_HighPPP_Countries  = c("Belgium", "Slovenia", "Finland", "Japan", 
                               "South Korea"),
  LowRD_LowPPP_Countries   = c("Montenegro", "Romania", "Latvia", "Bulgaria", 
                               "Greece", "Croatia", "Serbia"),
  MidRD_LowPPP_Countries   = c("Turkey", "Russia", "China"),
  HighRD_LowPPP_Countries  = c())

for(i in 1:length(listClusters)) {
  clustData[listClusters[[i]], "cluster"] <- countryClasses[i]
}
clustData$cluster <- factor(clustData$cluster)




## CART
set.seed(123)
train_idx <- sample(nrow(clustData), 0.75*nrow(clustData))

clust_train <- clustData[train_idx,]
levels(clust_train$cluster) <- levels(clustData$cluster)
clust_test  <- clustData[-train_idx,]
levels(clust_test$cluster) <- levels(clustData$cluster)

library(rpart)

rpart_clust_model <- rpart(formula = cluster ~ ., data = clust_train)
rpart_clust_model

plot(rpart_clust_model, uniform = TRUE, branch = 0.6, margin = 0.1)
text(rpart_clust_model, all = TRUE, use.n = TRUE)

printcp(x= rpart_clust_model)

rpart_predictions <- predict(object = rpart_clust_model,
                             newdata = clust_test, 
                             type = "class")
rpart_predictions <- factor(rpart_predictions, 
                            levels = levels(clustData$cluster))
caret::confusionMatrix(data = rpart_predictions, 
                       reference = clust_test$clust)


## TS
###
no_MidMid_Countries <- as.character(unlist(listClusters))
MidMid_Countries <- setdiff(rownames(clustData), no_MidMid_Countries)
listClusters$MidRD_MidPPP_Countries <- MidMid_Countries
countryClasses <- c(countryClasses, "MidRD_MidPPP")
###

hc_mtsCS(mtsCS(RD_Exp_GDP[listClusters$MidRD_LowPPP_Countries,,], 
               sector = "AllS"), 
         title = "MidRD, MidPPP Countries - RD_Exp_GDP - AllS")
hc_mtsCS(mtsCS(RD_pGERD[listClusters$MidRD_LowPPP_Countries,,], 
               sector = "BES"), 
         title = "MidRD, MidPPP Countries - RD_Exp_GDP - AllS")
