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

values <- reactiveValues()

updateDataInner <- function(query, dbDriverName, conn=NULL, fileContents=NULL) {
	# cat(file=stderr(), "db", dbDriverName, "\n")
	if (dbDriverName == "JSON") {
		rawlocdata <- fromJSON(query)
	} else if (dbDriverName == "CSV") {
		rawlocdata <- read.csv(text=query, header=TRUE)
	} else if (dbDriverName == "JSONFile") {
		# json_string <- readLines(query)
		rawlocdata <- fromJSON(fileContents)
	} else if (dbDriverName == "CSVFile") {
		rawlocdata <- fileContents
	} else if (dbDriverName == "MongoDB") {
		rawlocdata <- conn$find(query)
	} else if (dbDriverName == "Neo4j") {
		rawlocdata <- cypher(conn, query)
	} else if (grepl("SQL", dbDriverName)) {
		# rs <- dbSendQuery(conn, query)
		# rawlocdata <- fetch(rs, n=-1)
		rawlocdata <- dbGetQuery(conn, query)
	}
	rawlocdata$rowid_<-seq.int(nrow(rawlocdata))

	tempvars <- c()
	for(i in names(rawlocdata)) tempvars <- c(tempvars, i)
	names(tempvars) <- names(rawlocdata)
	tempvars <- c(tempvars, CONSTANT="CONSTANT")

	# Leaflet bindings are a bit slow; for now we'll just sample to compensate
	set.seed(as.numeric(Sys.time()))
	tempSampleSize <- min(c(nrow(rawlocdata), sampleSize))
	locdata <- rawlocdata[sample.int(nrow(rawlocdata), tempSampleSize),]

	values[["rawlocdata"]] <- rawlocdata
	values[["locdata"]] <- locdata
	values[["vars"]] <- tempvars
}

updateDataInner(dbQueries[[dbDriverNameDefault]], dbDriverNameDefault)
