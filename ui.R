source("constants.R")

library(shiny)
library(leaflet)

vars <- isolate(values[["vars"]])

shinyUI(navbarPage("Superzip", id="nav",

	tabPanel("Interactive map",
		div(class="outer",

			tags$head(
				# Include our custom CSS
				includeCSS("styles.css"),
				includeScript("gomap.js")
			),

			leafletOutput("map", width="100%", height="100%"),

			# Shiny versions prior to 0.11 should use class="modal" instead.
			absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
				draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
				width = 330, height = "auto",

				h2("ZIP explorer"),

				selectInput("color", "Color", vars),
				selectInput("size", "Size", vars, selected = "level"),

				tags$textarea(id="query", rows=3, cols=40, dbQuery),
				actionButton("loadButton", "Run")
			),

			tags$div(id="cite",
				'Data compiled for ', tags$em('Coming Apart: The State of White America, 1960–2010'), ' by Charles Murray (Crown Forum, 2012).'
			)
		)
	),

	tabPanel("Data explorer",
		DT::dataTableOutput("loctable")
	),

	conditionalPanel("false", icon("crosshair"))
))
