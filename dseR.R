dseR <- function(preFile, postFile, numSweeps, preAvg = 15, postStart = 1, 
                 postAvg = 2, returnPlots = F){
  
  #' Calculate depolarization-induced suppression of evoked postsynaptic currents
  #' 
  #' @description Provide description here.
  #' 
  #' @param preFile .atf file from pClamp that connects identified peaks and
  #' calculated baseline (in pA) from ePSCs prior to depolarization. I.e, baseline
  #' ePSC peaks.
  #' @param postFile .atf file from pClamp that connects identified peaks and
  #' calculated baseline (in pA) from ePSCs after to depolarization. I.e, DSE/DSI
  #' ePSC peaks.
  #' @param numSweeps number of sweeps for each trial. May need to expand to
  #' provide to numSweeps value if the number is different between pre and post
  #' depol trials.
  #' @param preAvg number of sweeps prior to depolarization to average to calculate
  #' baseline amplitude.
  #' @param postStart which sweep to start the post depolarization averaging
  #' @param postAvg which sweep after depolarization to end postdepolarization
  #' average to calculate DSE/DSI amplitude.
  #' @param returnPlots boolean. Specify whether to return basic plots of the data.
  #' 
  #' @examples 
  #'  dseR(preFile = "./path/to/pre-depol.atf", 
  #'  postFile = "./path/to/post-depol.atf",
  #'  numSweeps = 60, preAvg = 15, postAvg = 2,
  #'  returnPlots = F)
  #'  
  
  # load in required libraries
  #library(ggplot2)
  library(dplyr)
  library(matrixStats)
  
  # read in the peak data and store as separate dataframes
  preData <- as.data.frame(read.delim(preFile, header = T, skip = 2))
  postData <- as.data.frame(read.delim(postFile, header = T, skip = 2))
  
  # remove File.Path column from dataframes
  preData <- subset(preData, select = -c(File.Path))
  postData <- subset(postData, select = -c(File.Path))
  
  # rename columns
  colnames(preData) <- c("FileName", "Sweep", "Time", "PrePeakAmp", "PreBaselineAmp")
  colnames(postData) <- c("FileName", "Sweep", "Time", "PostPeakAmp", "PostBaselineAmp")
  
  ## reorganize data frames so each cell is own dataframe
  # maybe make cleaner by changing from filenames to cell1, cell2, etc.
  
  # get filenames
  fNamesPre <- unique(preData$FileName)
  fNamesPost <- unique(postData$FileName)
  
  # create merged dataframe for each cell
  rawData <- list()
  
  for (i in 1:length(fNamesPre)){
    cName <- paste("Cell", as.character(i), sep = '')
    preTemp <- preData[preData$FileName == fNamesPre[i],]
    preTemp$UncorPrePeak <- preTemp$PrePeakAmp + preTemp$PreBaselineAmp
    postTemp <- postData[postData$FileName == fNamesPost[i],]
    postTemp$UncorPostPeak <- postTemp$PostPeakAmp + postTemp$PostBaselineAmp
    preTemp <- subset(preTemp, select = -c(FileName))
    postTemp <- subset(postTemp, select = -c(FileName))
    tempMerge <- merge(preTemp, postTemp, by = c("Sweep", "Time"))
    tempMerge <- tempMerge[order(tempMerge$Time),]
    tempMerge$Time <- tempMerge$Time/1000
    rawData[[cName]] <- tempMerge
  }
  
  # merge all raw data into one dataframe
  combinedData <- bind_rows(rawData, .id = "data.frame")

  
  # calculate baseline ePSC amplitude for each cell
  # calculated as the average peak amplitude for the number of sweeps prior
  # depolarization, as specified by preAvg
  
  # get number of cells/cell names
  allCells <- unique(combinedData$data.frame)
  
  # create dataframe for storing calculated values
  calcData <- data.frame(cellNames = allCells)
  
  ePSCbaseline <- c()
  for (cl in allCells){
    baseline <- mean(tail(rawData[[cl]]$PrePeakAmp, preAvg))
    ePSCbaseline <- append(ePSCbaseline, baseline)
  }
  
  calcData$ePSCbaseline <- ePSCbaseline
  
  # calculate DSE/DSI ePSC amplitude for each cell
  ePSCdse <- c()
  
  for (cl in allCells){
    dse <- mean(rawData[[cl]]$PostPeakAmp[postStart:postAvg])
    ePSCdse <- append(ePSCdse, dse)
  }
  
  calcData$ePSC_DSE <- ePSCdse
  
  calcData$DSE_Percent <- ((calcData$ePSCbaseline-calcData$ePSC_DSE)/calcData$ePSCbaseline)*100
  
  # calculate ePSC amplitude % baseline
  for (cl in allCells){
    rawData[[cl]]$percBaselinePre <- (rawData[[cl]]$PrePeakAmp/
                                        calcData$ePSCbaseline[which(calcData$cellNames == cl)])*100
    rawData[[cl]]$percBaselinePost <- (rawData[[cl]]$PostPeakAmp/
                                         calcData$ePSCbaseline[which(calcData$cellNames == cl)])*100
  }
  
  # merge all raw data again after calculating % amplitude
  combinedData <- bind_rows(rawData, .id = "data.frame")

  # calculate averages for plotting
  # generate pre-depolarization percent baseline dataframe
  preDepolPerc <- data.frame(matrix(nrow = numSweeps, ncol = length(allCells)))
  colnames(preDepolPerc) <- allCells
  
  # same for post-depol
  postDepolPerc <- data.frame(matrix(nrow = numSweeps, ncol = length(allCells)))
  colnames(postDepolPerc) <- allCells
  
  # dataframes with data
  for (cell in allCells){
    preDepolPerc[[cell]] <- rawData[[cell]]$percBaselinePre
    postDepolPerc[[cell]] <- rawData[[cell]]$percBaselinePost
  }
  
  # average percents and st dev for all cells
  preAvg <- rowMeans(preDepolPerc)
  postAvg <- rowMeans(postDepolPerc)
  preSE <- rowSds(data.matrix(preDepolPerc, rownames.force = NA))/sqrt(length(allCells))
  postSE <- rowSds(data.matrix(postDepolPerc, rownames.force = NA))/sqrt(length(allCells))
  preSD <- rowSds(data.matrix(preDepolPerc, rownames.force = NA))
  postSD <- rowSds(data.matrix(postDepolPerc, rownames.force = NA))

  preDepolPerc$preAvgPerc <- preAvg
  postDepolPerc$postAvgPerc <- postAvg
  preDepolPerc$prePercSE <- preSE
  postDepolPerc$postPercSE <- postSE
  preDepolPerc$prePercSD <- preSD
  postDepolPerc$postPercSD <- postSD
  
  #combine avg data for easier plotting later
  allAvgData <- data.frame(Sweep = rawData$Cell1$Sweep)
  #allAvgData$Sweep <- rawData$Cell1$Sweep
  allAvgData$percBaselinePre <- preAvg
  allAvgData$preSE <- preSE
  allAvgData$preSD <- preSD
  allAvgData$percBaselinePost <- postAvg
  allAvgData$postSE <- postSE
  allAvgData$postSD <- postSD
  
  # combine all data into a list for returning out
  allData <- rawData
  allData$combined <- combinedData
  allData$calcData <- calcData
  allData$preDepolPerc <- preDepolPerc
  allData$postDepolPerc <- postDepolPerc
  allData$allAvgData <- allAvgData
  
  return(allData)
  
}