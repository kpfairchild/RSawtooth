#' Parse Sawtooth Labels File
#'
#' @param path File path to a Sawtooth Software .labels file
#'
#' @return List of labels metadata
#' @export
#'
#' @examples parseLabelsFile("golf.labels")
parseLabelsFile <- function(path)
{
  labelsXML<-read_xml(path)
  #****
  attributesXML <-xml_find_all(labelsXML,".//attribute|.//none")
  attributeLabels <- tibble(label = xml_text(xml_find_all(attributesXML,"./label")))

  #****
  levelXML <- xml_find_all(attributesXML,"./level")
  levelLabels <- xml_text(levelXML)

  #****
  attributeLevels <- sapply(xml_find_all(attributesXML,"//attribute"),function(x){ length(xml_find_all(x,".//level"))})

  labelsData <-
    tibble(
      attribute = rep(1:length(attributeLevels),times=attributeLevels),
      level = unlist(sapply(attributeLevels,function(x){1:x})),
      label =levelLabels
    )
  utilityColumns <- as.numeric(xml_attr(xml_find_all(attributesXML,"//level"),"column"))

  nAttr <- length(attributeLevels)
  attrCoding <- xml_attr(attributesXML,"coding")[1:nAttr]

  labelsFileOutput <-list(attributeLevels = attributeLevels,
                          labelsData = labelsData,
                          attrCoding = attrCoding,
                          attributeLabels = attributeLabels,
                          hasNone = length(xml_find_all(attributesXML,"//none"))>0,
                          utilityColumns = utilityColumns
  )
  class(labelsFileOutput) <- "SSLabelData"
  return(labelsFileOutput)
}
