dbDrivers <- c("MySQL", "PostgreSQL", "SQLite", "MongoDB", "Neo4j", "JSON", "JSONFile")
dbDriverName <- dbDrivers[7]
# if (dbDriverName %in% c("MySQL", "PostgreSQL", "SQLite")) {
if (grepl("SQL", dbDriverName)) {
	if (dbDriverName == "MySQL") {
		dbDetails <- list(
			"user" = "user",
			"pw" = "password",
			"name" = "table",
			"host" = "url"
		)
	} else if (dbDriverName == "PostgreSQL") {
		dbDetails <- list(
			"user" = "user",
			"pw" = "password",
			"name" = "table",
			"host" = "url",
			port = 5432
		)
	} else if (dbDriverName == "SQLite") {
		dbDetails <- list(
			"file" = "test.db"
		)
	}
	dbQuery <- "SELECT latitude, longitude, level, name FROM tests GROUP BY latitude, longitude;"
} else if (dbDriverName == "MongoDB") {
	dbDetails <- list(
		"url" = "mongodb://localhost",
		"db" = "efw_db",
		"collection" = "test"
	)
	dbQuery <- "{}"
} else if (dbDriverName == "Neo4j") {
	dbDetails <- list(
		"url" = "http://localhost:7474/db/data/",
		"user" = "neo4j",
		"pw" = "n"
	)
	dbQuery <- "MATCH (o:TestNode) RETURN o.latitude as latitude, o.longitude as longitude, o.level as level, o.name as name;"
} else if (dbDriverName == "JSON") {
	dbQuery <- paste(readLines("test.json"), collapse='\n')
} else if (dbDriverName == "JSONFile") {
	dbQuery <- "test.json"
} else {
	dbQuery <- ""
}
sampleSize <- 100 #how many random points to show, limits the number to avoid lag
defaultLatLong <- c(0, 0) #starting location to show on the map
defaultZoom <- 12
