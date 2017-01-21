source("constants.R")

library(dplyr)

if (dbDriverName == "MySQL") {
	library(RMySQL)
	conn <- dbConnect(dbDriver(dbDriverName), user=dbDetails$user, password=dbDetails$pw, dbname=dbDetails$name, host=dbDetails$host)
} else if (dbDriverName == "PostgreSQL") {
	library(RPostgreSQL)
	conn <- dbConnect(dbDriver(dbDriverName), user=dbDetails$user, password=dbDetails$pw, dbname=dbDetails$name, host=dbDetails$host, port=dbDetails$port)
} else if (dbDriverName == "SQLite") {
	library(RSQLite)
	conn <- dbConnect(dbDriver(dbDriverName), dbDetails$file)
} else if (grepl("JSON", dbDriverName)) {
	library(jsonlite)
} else if (dbDriverName == "MongoDB") {
	library(mongolite)
	conn <- mongo(url=dbDetails$url, db=dbDetails$db, collection=dbDetails$collection)
} else if (dbDriverName == "Neo4j") {
	library(RNeo4j)
	conn <- startGraph(dbDetails$url, username=dbDetails$user, password=dbDetails$pw)
}

values <- reactiveValues()

updateData <- function(query) {
	if (dbDriverName == "JSON") {
		rawlocdata <- fromJSON(query)
	} else if (dbDriverName == "JSONFile") {
		json_string <- readLines(query)
		rawlocdata <- fromJSON(json_string)
	} else if (dbDriverName == "MongoDB") {
		rawlocdata <- conn$find(query)
	} else if (dbDriverName == "Neo4j") {
		rawlocdata <- cypher(conn, query)
	} else {
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

updateData(dbQuery)
