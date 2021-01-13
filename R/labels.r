#' Parse Sawtooth Software Labels File
#'
#' @param path File path to a Sawtooth Software .labels file
#'
#' @return Returns list with labels metadata
#' @export
#'
#' @examples parseLabelsFile("golf.labels")
parseLabelsFile <- function(path)
{
  labelsXML <- xml2::read_xml(path)
  #****
  attributesXML <- xml2::xml_find_all(labelsXML,".//attribute|.//none")
  attributeLabels <- tibble(label = xml2::xml_text(xml2::xml_find_all(attributesXML,"./label")))

  #****
  levelXML <- xml2::xml_find_all(attributesXML,"./level")
  levelLabels <- xml2::xml_text(levelXML)

  #****
  attributeLevels <- sapply(xml2::xml_find_all(attributesXML, "//attribute"), function(x) {
    length(xml2::xml_find_all(x, ".//level"))
  })

  labelsData <-
    tibble(
      attribute = rep(1:length(attributeLevels),times=attributeLevels),
      level = unlist(sapply(attributeLevels,function(x){1:x})),
      label = levelLabels
    )
  utilityColumns <- as.numeric(xml2::xml_attr(xml2::xml_find_all(attributesXML,"//level"),"column"))

  nAttr <- length(attributeLevels)
  attrCoding <- xml2::xml_attr(attributesXML,"coding")[1:nAttr]

  labelsFileOutput <-list(attributeLevels = attributeLevels,
                          labelsData = labelsData,
                          attrCoding = attrCoding,
                          attributeLabels = attributeLabels,
                          hasNone = length(xml2::xml_find_all(attributesXML,"//none"))>0,
                          utilityColumns = utilityColumns
  )
  class(labelsFileOutput) <- "SSLabelData"
  return(labelsFileOutput)
}


#' Parse Lighthouse Studio Utility Report for label data
#'
#' @param utilityFilePath Filepath for Lighthouse Studio Utility Report
#'
#' @return Returns list with labels metadata
#' @export
#'
#' @examples noLabelsParser("Excel Report - CBCgolfexercise - HB.xlsx")
noLabelsParser <- function(utilityFilePath)
{
  output <- list()

  summaryPage <- lighthouseUtilityFileSheetReader(utilityFilePath,"Summary",col_names =FALSE)

  NAs <- (1:nrow(summaryPage))[is.na(summaryPage[,1])]

  utilitySectionStartRow     <- grep("Average Utilities",pull(summaryPage[,1]))
  utilitySectionEndRow       <- min(NAs[NAs > utilitySectionStartRow]) - 1
  utilityValues <- as.numeric(pull(summaryPage[(utilitySectionStartRow+1):utilitySectionEndRow,2]))
  levelLabels <- summaryPage[(utilitySectionStartRow+1):utilitySectionEndRow,1] %>% pull()
  if(length(grep("none",levelLabels[length(levelLabels)],ignore.case = TRUE)))
  {
    hasNone <- TRUE
  } else
  {
    hasNone <- FALSE
  }

  allPairs <- as.matrix(expand.grid(1:length(utilityValues),1:length(utilityValues)))
  upperPairs <- allPairs[allPairs[,1] < allPairs[,2],]

  cumSumUtilities <- function(x)
  {
    sum(utilityValues[seq(x[1],x[2])])
  }

  pairData <- as_tibble(cbind(1:nrow(upperPairs),
                              upperPairs,
                              apply(upperPairs,1,cumSumUtilities)),
                        .name_repair=~c("i","lowIndex","highIndex","diff"))

  potentialAttrPairs <- pairData[abs(pairData$diff) < 1e-6,]

  nAttrLevels <- NULL
  coding <- NULL
  allDone <- FALSE
  i <- 1
  while(!allDone)
  {
    matchIndex <- which(potentialAttrPairs$lowIndex==i)
    if(length(matchIndex)>0)
    {
      nlevels <- min(potentialAttrPairs[matchIndex,]$highIndex)-i+1
      nAttrLevels <- c(nAttrLevels,nlevels)
      i <- i+nlevels
      coding <- c(coding,"partworth")
    } else
    {
      nAttrLevels <- c(nAttrLevels,1)
      i <- i+1
      coding <- c(coding,"linear")
    }
    if(i > length(utilityValues)-hasNone)
    {
      allDone <- TRUE
    }
  }

  nAttr      <- length(nAttrLevels)
  output$nLevels <- nAttrLevels[1:nAttr]

  output$labelsData <-
    tibble(attribute = rep(1:nAttr,times=output$nLevels),
           level = unlist(Vectorize(function(x){1:x})(output$nLevels)),
           label = ifelse(rep(hasNone,sum(output$nLevels)),levelLabels[-length(levelLabels)],levelLabels)
    )
  output$attrCoding <- coding

  importancesSectionStartRow <- grep("Average Importances",pull(summaryPage[,1]))
  importancesSectionEndRow <- nrow(summaryPage)

  attributeLabels <- summaryPage[(importancesSectionStartRow+1):importancesSectionEndRow,1]

  output$attributeLabels <- attributeLabels
  output$hasNone <- hasNone


  class(output) <- "SSLabelData"

  return(output)
}

