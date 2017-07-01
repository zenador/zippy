# source("constants.R")

vars <- isolate(valuesDefault$vars)
graphTypes <- c("leaflet", "ggvis", "plotly")

shinyUI(navbarPage("Zippy", id="nav",

	tabPanel("Interactive graph",
		div(class="outer",

			tags$head(
				# Include our custom CSS
				includeCSS("styles.css"),
				includeScript("gomap.js")
			),

			conditionalPanel(
				condition = "input.graphType == 'leaflet'",
				id = "leafletPanel", class = "graphPanel",
				leafletOutput("map", width="100%", height="100%")
			),
			conditionalPanel(
				condition = "input.graphType == 'ggvis'",
				id = "ggvisPanel", class = "graphPanel",
				ggvisOutput("chartG")
			),
			conditionalPanel(
				condition = "input.graphType == 'plotly'",
				id = "plotlyPanel", class = "graphPanel",
				plotlyOutput("chartP", width="80%", height="100%")
			),

			# Shiny versions prior to 0.11 should use class="modal" instead.
			absolutePanel(id = "controls", class = "panel panel-default controls", fixed = TRUE,
				draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
				width = 330, height = "auto",

				h3("Graph Options"),

				selectInput("graphType", "Type", graphTypes, selected = "leaflet"),

				conditionalPanel(
					condition = "input.graphType == 'leaflet'",
					selectInput("colorL", "Color", vars),
					selectInput("sizeL", "Size", vars)
				),
				conditionalPanel(
					condition = "input.graphType == 'ggvis'",
					selectInput("xvarG", "X", vars, selected="longitude"),
					selectInput("yvarG", "Y", vars),
					selectInput("colorG", "Color", vars),
					selectInput("strokeG", "Stroke", vars),
					selectInput("sizeG", "Size", vars)
				),
				conditionalPanel(
					condition = "input.graphType == 'plotly'",
					selectInput("xvarP", "X", vars, selected="longitude"),
					selectInput("yvarP", "Y", vars),
					selectInput("zvarP", "Z", vars, selected="CONSTANT"),
					selectInput("colorP", "Color", vars),
					selectInput("sizeP", "Size", vars),
					selectInput("shapeP", "Shape", vars)
				),

				# tags$textarea(id="query", rows=3, cols=30, dbQuery),
				textAreaInput("query", "Query", dbQueries[[dbDriverNameDefault]], rows=4),
				actionButton("runButton", "Run"),
			# ),

			# absolutePanel(id = "dbcontrols", class = "panel panel-default controls", fixed = TRUE,
			# 	draggable = TRUE, top = "auto", left = "auto", right = 20, bottom = 20,
			# 	width = 330, height = "auto",

				h3("Database Options"),

				selectInput("dbType", "Type", dbDrivers, selected = dbDriverNameDefault),

				conditionalPanel(
					condition = "input.dbType == 'MySQL' || input.dbType == 'PostgreSQL'",
					textInput("dbUserS", "User", dbDetails$SQL$user),
					passwordInput("dbPwS", "Password", dbDetails$SQL$pw),
					textInput("dbNameS", "DB Name", dbDetails$SQL$name),
					textInput("dbUrlS", "URL", dbDetails$SQL$host)
				),
				conditionalPanel(
					condition = "input.dbType == 'MongoDB'",
					textInput("dbUrlM", "URL", dbDetails$MongoDB$url),
					textInput("dbNameM", "DB Name", dbDetails$MongoDB$db),
					textInput("dbCollM", "Collection", dbDetails$MongoDB$coll),
					checkboxInput("dbAggM", "Aggregate Query?", FALSE)
				),
				conditionalPanel(
					condition = "input.dbType == 'Neo4j'",
					textInput("dbUserN", "User", dbDetails$Neo4j$user),
					passwordInput("dbPwN", "Password", dbDetails$Neo4j$pw),
					textInput("dbUrlN", "URL", dbDetails$Neo4j$url)
				),
				conditionalPanel(
					condition = "input.dbType == 'SQLite' || input.dbType == 'JSONFile' || input.dbType == 'CSVFile'",
					fileInput("file1", "Choose File")
				),

				actionButton("setButton", "Set")
			)
		)
	),

	tabPanel("Data explorer",
		DT::dataTableOutput("loctable")
	)
))
