## Description

Modified from [Shiny's SuperZip example code](https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example).

Displays lat/long co-ordinates from a MySQL DB on a Leaflet map and DataTable, and allows you to run new queries and change what variables colour and size should represent on the map.

## Instructions

Define the connection parameters, default query, etc. in constants.R. In the R console, set working directory to this folder with `setwd("[PATH]")`, then run `shiny::runApp()`

Your queries must return the lat/long fields as 'latitude' and 'longitude' respectively. If you return a number as the field 'level', that will be automatically used to determine the size of the dots upon first load. You can return other fields to be displayed on the marker popups, data table, and to affect colour and size.
