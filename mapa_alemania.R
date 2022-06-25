library("leaflet")
library("stringr")
library("raster")
library("leaflet.extras")

map3D <- read.csv2("mapa_alemania/map3D.csv", encoding = "utf8")
mainstation <- read.csv2("mapa_alemania/mainstation.csv", encoding = "utf8")
stationNairport <- read.csv2("mapa_alemania/stationNairport.csv", encoding = "utf8")
station <- read.csv2("mapa_alemania/station.csv", encoding = "utf8")
alternativestation <- read.csv2("mapa_alemania/alternativestation.csv", encoding = "utf8")



k1= as.numeric(map3D$Recommended.Range.with.TOW..km.)
k2= as.numeric(map3D$Range.by.the.Manufacturer..km.)
k3= as.numeric(map3D$Realistic.Range.with.TOW..km.)

df <- data.frame(
  "LEGEND" = c(k1,k2,k3)
)

l1 <- str_c("Range by Manufacture:", k1, sep = " ")
l2 <- str_c("Realistic Range:", k2, sep = " ")
l3 <- str_c("Recommended Range", k3, sep = " ")


n <- max(df)
cc <- colorRampPalette(c("blue","grey", "yellow"))(n)


getColor <- function(df) {
  sapply(df$LEGEND, function(LEGEND) {
    if(LEGEND <= (n)) {
      "yellow"
    } else if(Pop <= k2 ) {
      "grey"
    } else {
      "blue"
    } })
}

pal <- colorNumeric(
  palette = cc,
  domain = df$LEGEND
)


adm <- getData('GADM', country='DE', level=1)
popup <- paste0("<strong>Name: </strong>",adm$NAME_1)

color.purura <- makeAwesomeIcon(icon = 'plane', markerColor = 'purple', library='fa', iconColor = 'black')
color.NAirp <- makeAwesomeIcon(icon = ' ', markerColor = 'red', library='fa', iconColor = 'black')
color.rojo <- makeAwesomeIcon(icon = 'plane', markerColor = 'red', library='fa', iconColor = 'black')
color.verde <- makeAwesomeIcon(icon = 'plane', markerColor = 'green', library='fa', iconColor = 'black')


addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.5){
  colorAdditions <- paste0(colors, "; width:", sizes, "px; height:", sizes, "px")
  labelAdditions <- paste0("<div style='display: inline-block;height: ", 
                           sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", 
                           labels, "</div>")
  
  return(leaflet::addLegend(map, colors = colorAdditions, 
                   labels = labelAdditions, opacity = opacity))
}