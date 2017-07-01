dbDrivers <- c("MySQL", "PostgreSQL", "SQLite", "MongoDB", "Neo4j", "JSON", "JSONFile", "CSV", "CSVFile")
dbDriverNameDefault <- dbDrivers[6]
sampleSize <- 100 #how many random points to show, limits the number to avoid lag
defaultLatLong <- c(0, 0) #starting location to show on the map
defaultZoom <- 12

dbDetails <- list(
	"SQL" = list(
		"user" = "",
		"pw" = "",
		"name" = "",
		"host" = "",
		"port" = 5432 # postgres
	),
	"MongoDB" = list(
		"url" = "mongodb://localhost",
		"db" = "",
		"coll" = ""
	),
	"Neo4j" = list(
		"user" = "neo4j",
		"pw" = "",
		"url" = "http://localhost:7474/db/data/"
	)
)
sqlQuery <- "SELECT latitude, longitude, level, name FROM tests GROUP BY latitude, longitude;"
dbQueries <- list(
	"MySQL" = sqlQuery,
	"PostgreSQL" = sqlQuery,
	"SQLite" = sqlQuery,
	"MongoDB" = "{}",
	"Neo4j" = "MATCH (o:TestNode) RETURN o.latitude as latitude, o.longitude as longitude, o.level as level, o.name as name;",
	"JSON" = paste(readLines("test.json"), collapse='\n'),
	"CSV" = paste(readLines("test.csv"), collapse='\n'),
	"JSONFile" = "N/A",
	"CSVFile" = "N/A"
)
