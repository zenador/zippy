## Description

Modified from [Shiny's SuperZip example code](https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example).

Displays data from DBs (e.g. MySQL, PostgreSQL, SQLite, MongoDB, Neo4j, JSON Object) on a Leaflet map or as a ggvis/plotly scatter plot and DataTable, and allows you to run new queries and change what fields certain marker properties (e.g. colour, size) should represent.

## Instructions

Define which database to use (`dbDriverName`), the connection parameters (`dbDetails`), default query (`dbQuery`), etc. in `constants.R`. Install the R package(s) relevant to your selected database. In the R console, set working directory to this folder with `setwd("[PATH]")`, then run `shiny::runApp()`

When the webapp is running, you can use the first dropdown menu on the top right corner to choose which kind of graph to use. The fields returned will be displayed on the marker popups, data table, and affect the marker properties.

If you are using the Leaflet map:
The fields returned by your queries must include lat/long as 'latitude' and 'longitude' (both numeric) respectively.
