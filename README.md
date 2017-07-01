## Description

Modified from [Shiny's SuperZip example code](https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example).

Quick and easy data visualisation on the fly. Allows you to run new queries and change what fields certain marker properties (e.g. colour, size) should represent.

- Can read from databases:
    - MySQL
    - PostgreSQL
    - SQLite
    - MongoDB
    - Neo4j
- Can read from file formats:
    - JSON
    - CSV
- Can display as:
    - Leaflet map
    - ggvis scatter plot
    - Plotly scatter plot
    - DataTable

## Instructions

Install the R package(s) relevant to the databases you wish to use. Remove databases you don't want from the `dbDrivers` vector in `constants.R`. In the R console, set working directory to this folder with `setwd("[PATH]")`, then run `shiny::runApp()`

When the webapp is running, you can use the Graph Type dropdown menu on the top right corner to choose which kind of graph to use. The fields returned will be displayed on the marker popups, data table, and affect the marker properties. Define your own query and click the `Run` button to apply it.

You can also use the Database Type dropdown menu further down to change which DB you're reading data from, fill in the DB connection parameters, then click the `Set` button to apply your changes. After changing DBs, you should click the `Run` button again to run the new query and see your updates on the chart.

If you are using the Leaflet map:
The fields returned by your queries must include lat/long as 'latitude' and 'longitude' (both numeric) respectively.
