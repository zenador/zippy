source("constants.R")

library(dplyr)
library(RMySQL)

values <- reactiveValues()

conn <- dbConnect(MySQL(), user=dbUser, password=dbPw, dbname=dbName, host=dbHost)

updateData <- function(query) {
	rs <- dbSendQuery(conn, query)
	rawlocdata <- fetch(rs, n=-1)

	tempvars <- c()
	for(i in names(rawlocdata)) tempvars <- c(tempvars, i)
	names(tempvars) <- names(rawlocdata)

	# Leaflet bindings are a bit slow; for now we'll just sample to compensate
	set.seed(as.numeric(Sys.time()))
	locdata <- rawlocdata[sample.int(nrow(rawlocdata), sampleSize),]

	values[["rawlocdata"]] <- rawlocdata
	values[["locdata"]] <- locdata
	values[["vars"]] <- tempvars
}

updateData(dbQuery)
