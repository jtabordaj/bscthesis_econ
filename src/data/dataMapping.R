library(devtools)
# devtools::install_github("nebulae-co/colmaps")
library(colmaps)
# colmap(departamentos)
listaMuni <- data.frame(table(municipios@data[2]))[1]
names(listaMuni)[1] <- "id_depto"

atlasMethod <- function(sector, lowcolor, highcolor, title, subtitle){
    selectedSector <- sector
    geoDist <- data.frame(table(selectedSector$id_depto))
    geoDist$Freq <- as.numeric(geoDist$Freq)
    colnames(geoDist) <- c("id_depto", "Frequency")
    geoPlot <- right_join(geoDist, listaMuni, by = "id_depto")
    return(colmap(map = departamentos, data = geoPlot, map_id = "id_depto", autocomplete = TRUE) + 
    scale_fill_continuous(low = lowcolor, high = highcolor) +
    ggtitle(title, subtitle = subtitle) +
    theme(plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5) ))
}

## Groceries segment
meatAndFish <- atlasMethod(sector1011, "#fd5252", "#e60417", "Sector 1011: Processing of meat and fish", "Mark I")
oilAndFats <- atlasMethod(sector1030, "#fd5252", "#e60417", "Sector 1030: Production of oil and fats", "Mark I")
harinaPan <- atlasMethod(sector1051, "#fd5252", "#e60417", "Sector 1051: Milling products", "Mark I")
fruitsAndVegetables <- atlasMethod(sector1020, "#fd5252", "#e60417", "Sector 1020: Processing of fruits and vegetables", "Mark I")
ggarrange(ncol = 2, nrow = 2, meatAndFish, fruitsAndVegetables, oilAndFats, harinaPan)

theMilk <- atlasMethod(sector1040, "#fd5252", "#e60417", "Sector 1040: Dairy", "Mark I")
theBread <- atlasMethod(sector1081, "#fd5252", "#e60417", "Sector 1081: Bread and related", "Mark I")
theChocolate <- atlasMethod(sector1089, "#60adfa", "#005bb6", "Sector 1089: Chocolate", "Mark II")
theCoffee <- atlasMethod(sector1061, "#fd5252", "#e60417", "Sector 1061: Coffee products", "Mark I")
ggarrange(ncol = 2, nrow = 2, theMilk, theBread, theChocolate, theCoffee)

sodaBeverages <- atlasMethod(sector1104, "#fd5252", "#e60417", "Sector 1104: Non-alcoholic beverages", "Mark I")

## Textiles segment
textilesElabor <- atlasMethod(sector1312, "#60adfa", "#005bb6", "Sector 1312: Elaboration of textiles", "Mark II")
textilesFinish <- atlasMethod(sector1313, "#fd5252", "#e60417", "Sector 1313: Finishing of textiles", "Mark I")
textilesCrocht <- atlasMethod(sector1313, "#60adfa", "#005bb6", "Sector 1391: Crocheting", "Mark II")
textilesMadeUp <- atlasMethod(sector1392, "#60adfa", "#005bb6", "Sector 1392: Made-up textiles", "Mark II")
ggarrange(ncol = 2, nrow = 2, textilesElabor, textilesFinish, textilesCrocht, textilesMadeUp)

textileWear <- atlasMethod(sector1410, "#fd5252", "#e60417", "Sector 1410: Elaboration of wearing apparel", "Mark I")
knittedWear <- atlasMethod(sector1430, "#60adfa", "#005bb6", "Sector 1430: Elaboration of crocheted apparel", "Mark II")
ggarrange(ncol = 2, nrow = 1, textileWear, knittedWear)

## Furnitures
elabFurnit <- atlasMethod(sector3110, "#fd5252", "#e60417", "Sector 3110: Elaboration of furnitures", "Mark I")
elabMatres <- atlasMethod(sector3120, "#60adfa", "#005bb6", "Sector 3120: Elaboration of mattresses", "Mark II")
ggarrange(ncol = 2, nrow = 1, elabFurnit, elabMatres)

## Wood, cardboard and paper

woodSawmil <- atlasMethod(sector1610, "#60adfa", "#005bb6", "Sector 1610: Wood Sawmilling", "Mark II")
woodBuildn <- atlasMethod(sector1630, "#60adfa", "#005bb6", "Sector 1630: Wood elaboration for buildings", "Mark II")
cardboardContn <- atlasMethod(sector1702, "#fd5252", "#e60417", "Sector 1702: Containers made of wood, paper or cardboard", "Mark I")
cardboardHouse <- atlasMethod(sector1709, "#fd5252", "#e60417", "Sector 1709: Household and office products", "Mark I")
ggarrange(ncol = 2, nrow = 2, woodBuildn, woodSawmil, cardboardContn, cardboardHouse)

## Petroleum and metals

oilRefin <- atlasMethod(sector1921, "#60adfa", "#005bb6", "Sector 1921: Petroleum refining", "Mark II")
oilRefin

metalsOrMineralsConstruct <- rbind(sector2392, sector2394, sector2395, sector2410, sector2511, sector2592, sector2593, sector2599)
commonMetals <- atlasMethod(metalsOrMineralsConstruct, "#fd5252", "#e60417", "Ferrous Metals and minerals for construction and foundries", "(Selected industries) - Mark I" )
rareMetals <- atlasMethod(sector2429, "#60adfa", "#005bb6", "Sector 2429: Non-ferrous metals and other minerals", "Mark II" )
ggarrange(ncol = 2, nrow = 1, commonMetals, rareMetals)