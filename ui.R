source("constants.R")

library(shiny)
library(leaflet)
library(ggvis)
library(plotly)

vars <- isolate(values[["vars"]])
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
				plotlyOutput("chartP", width="80%")
			),

			# Shiny versions prior to 0.11 should use class="modal" instead.
			absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
				draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
				width = 330, height = "auto",

				# h2("ZIP explorer"),

				selectInput("graphType", "Graph Type", graphTypes, selected = "leaflet"),

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

				tags$textarea(id="query", rows=3, cols=40, dbQuery),
				actionButton("loadButton", "Run")
			)
		)
	),

	tabPanel("Data explorer",
		DT::dataTableOutput("loctable")
	)
))
