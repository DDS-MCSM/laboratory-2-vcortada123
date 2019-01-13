#******************************************************************************#
#                                                                              #
#                          Lab 2 - CPE Standard                                #
#                                                                              #
#              Arnau  Sangra Rocamora - Data Driven Securty                    #
#                                                                              #
#******************************************************************************#

if(!require("xml2")){
  install.packages("xml2")
  library("xml2")
}

if(!require("stringr")){
  install.packages("stringr")
  library("stringr")
}

if(!require("tidyr")){
  install.packages("tidyr")
  library("tidyr")
}

if(!require("dplyr")){
  install.packages("dplyr")
  library("dplyr")
}

#Descargar diccionario
cpes_url <- "https://nvd.nist.gov/feeds/xml/cpe/dictionary/official-cpe-dictionary_v2.3.xml.zip"
cpes_filename <- "cpes.zip"
download.file(cpes_url, cpes_filename)
unzip(zipfile = cpes_filename)



#Extraer los items a un fichero cpe.raw
GetCPEItems <- function(cpe_xml) {
  
  cpe_name <- xml2::xml_text(xml2::xml_find_all(cpe_xml, "//*[name()='cpe-item']/@name"))
  cpe_23 <- xml2::xml_text(xml2::xml_find_all(cpe_xml, "//*[cpe-23:cpe23-item]/*/@name"))
  cpe_title <- xml2::xml_text(xml2::xml_find_all(cpe_xml, "//*[cpe-23:cpe23-item]/*[@xml:lang='en-US'][1]"))
  cpes <- data.frame(cpe_name, cpe_title, cpe_23, stringsAsFactors = F)
  
  return(cpes)
}


CleanCPEs <- function(cpes){
  column_names <- c("st", "st_v", "part", "vendor", "product",
                    "version", "update", "edición", "lang", "sw_edition",
                    "target_sw", "target_hw", "others")
  
  cpes$cpe_23 <- stringr::str_replace_all(cpes$cpe_23, "\\\\:", ";")
  
  cpes <- tidyr::separate(data = cpes, col = cpe_23, into = column_names, sep = ":", remove = F)
  cpes <- dplyr::select(.data = cpes, -st, -st_v)
  
  cpes$vendor <- as.factor(cpes$vendor)
  cpes$product <- as.factor(cpes$product)
  cpes$lang <- as.factor(cpes$lang)
  cpes$sw_edition <- as.factor(cpes$sw_edition)
  cpes$target_sw <- as.factor(cpes$target_sw)
  cpes$target_hw <- as.factor(cpes$target_hw)
  cpes$others <- as.factor(cpes$others) 
  
  return(cpes)
}

ParseCPEData <- function() {
  
  cpe.file <- "./official-cpe-dictionary_v2.3.xml"
  
  #Cargar diccionario cpes y pasarlo a un fichero xml
  cpe_xml <- xml2::read_xml(x = cpe.file)
  
  # Hacer un Get de los CPEs
  cpes <- GetCPEItems(cpe_xml)
  
  # Transformar, limpiar, arreglar analisis cpes como data frame
  df <- CleanCPEs(cpes)
  
  # Obtenemos el data frame
  return(df)
}
