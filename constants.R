dbUser <- "user"
dbPw <- "password"
dbName <- "table"
dbHost <- "url"
dbQuery <- "SELECT latitude, longitude, COUNT(latitude) as level FROM last_location GROUP BY latitude, longitude;"
sampleSize <- 100 #how many random points to show, limits the number to avoid lag
defaultLat <- 0 #starting location to show on the map
defaultLong <- 0
defaultZoom <- 1
