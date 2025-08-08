[![DOI](https://zenodo.org/badge/680302445.svg)](https://doi.org/10.5281/zenodo.16783200)

# FreshEcoDExt


**FreshEcoDExt** is an R Shiny app designed to extract Freshwater ecoregions and subregions data of any locality in the world. 
This app also provides for additional visualization options, map downloads, 
shapefile download, ecoregion count heatmaps along with the tabular output. 


## Installation

The R shiny app can be downloaded directly from the **Rshinyapps** link as a '.tar' file,  unzipped and run locally. The app can also be
run directly from this link (<https://www.shinyapps.io/admin/#/application/9520711>),though, 
since the app is hosted in a free tier service, availability is limited. 


## Input

1.  The input file needs to be a ‘.csv’ and is loaded by clicking the ‘Browse’ button. 
2.  Excel files are currently not recognized.
3.  The contents should at least have a column named ‘Locality’ along with Latitude and Longitude columns
   (There could be any number of columns along with these three).
4.  The Latitude and Longitude data must be in decimal degrees. 
5.  The app does not require a shapefile input.


## Output

*Map* 

App provides a leaflet visualization of the ecoregions based on the locality data entered. Three tiles have been provided for visualization currently.  
Hovering over any colored area will display the respective ecoregion name. Hovering over the spatial point specifically will display the respective locality name. 
A leaflet map can be downloaded with ‘WorldGrayCanvas’ tile only. The map will be saved as a ‘.jpg’ file. The color combinations are fixed and cannot be changed.
Freshwater ecoregion shapefile of the selected localities can be downloaded (WGS84 projection)


*Tabular data*

Tabular data giving information about the ecoregion and its subregion for each location can be visualized and downloaded as a ‘.csv’ file.


*Heatmap*

A heatmap showing the crosstab (counts) of different subregions and ecoregions with respect to the user data can be visualized. The color combinations are fixed 
and cannot be changed via the app. Download option will be made available soon.


## Current limitations of the app

The map download aspect of the app does not work properly on smartphones and Mac OS in some cases. These issues will be resolved in the future. 
Similarly, the rendering of the leaflet plot might not be ideal. It is advised that the input data not be above 5000 records when running directly via rshinyapps.
Though map visualization zoom is changeable, the output map image zoom is set at a particular value and cannot be changed.
A large locality dataset might result in a heatmap with crowded labels.


*Citation:* Sameer Padhye. (2025). sameerpadhye/FreshEcoDExt: FreshEcoDExt (1.0.1). Zenodo. https://doi.org/10.5281/zenodo.16783206
