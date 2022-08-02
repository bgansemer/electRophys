dseR <- function(preFile, postFile, numSweeps, preAvg = 15, postAvg = 2,
                 returnPlots = F){
  
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
  #' @param postAvg number of sweeps after depolarization to average to calculate 
  #' DSE/DSI amplitude.
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
  
  # preList <- list()
  # postList <- list()
  # 
  # # may need to add "pre" and "post" to cell names to avoid confusion
  # for (i in 1:length(fNamesPre)){
  #   templist <- preData[preData$FileName == fNamesPre[i],]
  #   templist$Time <- templist$Time/1000 # convert time to s instead of ms
  #   cName <- paste("PreCell", as.character(i), sep = '')
  #   preList[[cName]] <- templist
  # }
  # 
  # for (i in 1:length(fNamesPost)){
  #   templist <- postData[postData$FileName == fNamesPost[i],]
  #   templist$Time <- templist$Time/1000 # convert time to s instead of ms
  #   cName <- paste("PostCell", as.character(i), sep = '')
  #   postList[[cName]] <- templist
  # }
  
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
    dse <- mean(head(rawData[[cl]]$PostPeakAmp, postAvg))
    ePSCdse <- append(ePSCdse, dse)
  }
  
  calcData$DSE <- ePSCdse
  
  # calculate ePSC amplitude % baseline
  for (cl in allCells){
    rawData[[cl]]$percBaselinePre <- (rawData[[cl]]$PrePeakAmp/
                                        calcData$ePSCbaseline[which(calcData$cellNames == cl)])*100
    rawData[[cl]]$percBaselinePost <- (rawData[[cl]]$PostPeakAmp/
                                         calcData$ePSCbaseline[which(calcData$cellNames == cl)])*100
  }
  
  # merge all raw data again after calculating $ amplitude
  combinedData <- bind_rows(rawData, .id = "data.frame")
  
  # generate generic plots
  if (returnPlots == T){
    #graph code here
  }

  
  
  allData <- rawData
  allData$combined <- combinedData
  allData$calcData <- calcData
  
  return(allData)
  
}