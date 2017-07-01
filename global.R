source("constants.R")

library(shiny)
library(dplyr)
library(leaflet)
library(ggvis)
library(plotly)
if ("MySQL" %in% dbDrivers) library(RMySQL)
if ("PostgreSQL" %in% dbDrivers) library(RPostgreSQL)
if ("SQLite" %in% dbDrivers) library(RSQLite)
if ("JSON" %in% dbDrivers) library(jsonlite)
if ("MongoDB" %in% dbDrivers) library(mongolite)
if ("Neo4j" %in% dbDrivers) library(RNeo4j)

valuesDefault <- reactiveValues()

updateDataInner <- function(query, dbDriverName, rvDict, conn=NULL, fileContents=NULL, mongoAggregate=FALSE) {
	# cat(file=stderr(), "db", dbDriverName, "\n")
	if (dbDriverName == "JSON") {
		dataPoints <- fromJSON(query)
	} else if (dbDriverName == "CSV") {
		dataPoints <- read.csv(text=query, header=TRUE)
	} else if (dbDriverName == "JSONFile") {
		# json_string <- readLines(query)
		dataPoints <- fromJSON(fileContents)
	} else if (dbDriverName == "CSVFile") {
		dataPoints <- fileContents
	} else if (dbDriverName == "MongoDB") {
		if (mongoAggregate)
			dataPoints <- conn$aggregate(query)
		else
			dataPoints <- conn$find(query)
	} else if (dbDriverName == "Neo4j") {
		dataPoints <- cypher(conn, query)
	} else if (grepl("SQL", dbDriverName)) {
		# rs <- dbSendQuery(conn, query)
		# dataPoints <- fetch(rs, n=-1)
		dataPoints <- dbGetQuery(conn, query)
	}
	if (is.null(dataPoints) || is.null(nrow(dataPoints)) || nrow(dataPoints) == 0)
		return()
	dataPoints$rowid_<-seq.int(nrow(dataPoints))

	tempvars <- c()
	for(i in names(dataPoints)) tempvars <- c(tempvars, i)
	names(tempvars) <- names(dataPoints)
	tempvars <- c(tempvars, CONSTANT="CONSTANT")

	# Leaflet bindings are a bit slow; for now we'll just sample to compensate
	set.seed(as.numeric(Sys.time()))
	tempSampleSize <- min(c(nrow(dataPoints), sampleSize))
	sampleDataPoints <- dataPoints[sample.int(nrow(dataPoints), tempSampleSize),]

	rvDict$dataPoints <- dataPoints
	rvDict$sampleDataPoints <- sampleDataPoints
	rvDict$vars <- tempvars
}

updateDataInner(dbQueries[[dbDriverNameDefault]], dbDriverNameDefault, valuesDefault)
