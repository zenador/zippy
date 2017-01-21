source("constants.R")

library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(ggvis)

shinyServer(function(input, output, session) {

	## Leaflet ###########################################

	# Create the map
	output$map <- renderLeaflet({
		leaflet() %>%
			addTiles(
				urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
				attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
			) %>%
			setView(lat = defaultLatLong[1], lng = defaultLatLong[2], zoom = defaultZoom)
	})

	addShapestoLeaflet <- function() {
		if (isolate(input$graphType) != "leaflet") {
			return()
		}
		colorBy <- input$colorL
		sizeBy <- input$sizeL

		locdata <- values$locdata

		if (colorBy == "CONSTANT") {
			reps <- nrow(locdata)
			colorData <- rep("a", each=reps)
			# colorData <- (1:reps)
		} else {
			colorData <- locdata[[colorBy]]
		}
		if (is.numeric(colorData)) {
			pal <- colorBin("Spectral", colorData, 7, pretty = FALSE)
		} else {
			pal <- colorFactor(topo.colors(length(colorData)), colorData)
		}

		sizeData <- locdata[[sizeBy]]
		if (sizeBy == "CONSTANT" || !is.numeric(sizeData)) {
			radius <- 1500
		} else {
			radius <- sizeData / max(sizeData) * 1500
		}

		map <- leafletProxy("map", data = locdata) %>%
			clearShapes() %>%
			clearControls() %>%
			addCircles(~longitude, ~latitude, radius=radius, stroke=FALSE, fillOpacity=0.4, fillColor=~pal(colorData))

		if (colorBy != "CONSTANT") {
			map <- map %>% addLegend("bottomleft", pal=pal, values=colorData, title=colorBy, layerId="colorLegend")
		}

		return(map)
	}

	# This observer is responsible for maintaining the circles and legend,
	# according to the variables the user has chosen to map to color and size.
	observe({
		input$colorL
		input$sizeL

		addShapestoLeaflet()
	})

	# Show a popup at the given location
	showItemPopup <- function(itemid, lat, lng) {
		locdata <- values$rawlocdata

		selectedItem <- locdata[locdata$longitude == lng & locdata$latitude == lat,]

		x <- list()
		for(i in names(selectedItem)) {
			taggy <- tags$h5(paste(i, ":", sep=""), selectedItem[[i]])
			x <- list(x, taggy)
		}

		content <- as.character(tagList(x))
		leafletProxy("map") %>% addPopups(lng, lat, content, layerId = itemid)
	}

	# When map is clicked, show a popup with city info
	observe({
		leafletProxy("map") %>% clearPopups()
		event <- input$map_shape_click
		if (is.null(event))
			return()

		isolate({
			showItemPopup(event$id, event$lat, event$lng)
		})
	})

	observeEvent(input$loadButton, {
		updateData(input$query)
		vars <- values[["vars"]]
		for (varname in c("colorL", "sizeL", "xvarG", "yvarG", "colorG", "strokeG", "sizeG", "xvarP", "yvarP", "zvarP", "colorP", "sizeP", "shapeP")) {
			updateSelectInput(session = session, inputId = varname, choices = vars, selected = input[[varname]])
		}
	})

	# necessary because shapes are not populated if the graph starts in a mode other than leaflet
	observeEvent(input$graphType, {
		addShapestoLeaflet()
	})

	## ggvis ###########################################

	getOptVar <- function(varName, chosenInput, constant=0) {
		if (chosenInput == "CONSTANT") {
			result <- prop(varName, constant)
		} else {
			result <- prop(varName, as.symbol(chosenInput))
		}
		return(result)
	}

	vis_tooltip <- function(x) {
		if (is.null(x)) return(NULL)
		if (is.null(x$rowid_)) return(NULL)

		all_things <- values$rawlocdata
		this_thing <- all_things[all_things$rowid_ == x$rowid_, ]
		
		this_list <- c()
		for(i in names(this_thing)) {
			taggy <- paste0("<b>", i, "</b>: ", this_thing[[i]])
			this_list <- c(this_list, taggy)
		}
		this_string <- paste0(this_list, collapse="<br>")
		return(this_string)
	}

	vis <- reactive({
		xvar_name <- input$xvarG
		yvar_name <- input$yvarG
		stroke_name <- input$strokeG
		size_name <- input$sizeG
		xvar <- getOptVar("x", xvar_name, 0)
		yvar <- getOptVar("y", yvar_name, 0)
		fill <- getOptVar("fill", input$colorG, 50)
		stroke <- getOptVar("stroke", stroke_name, 50)
		size <- getOptVar("size", size_name, 100)

		graphy <-
			values$rawlocdata %>%
			ggvis(xvar, yvar) %>%
			layer_points(
				size,
				fill,
				stroke,
				# shape = ~factor(level),
				fillOpacity := 0.5,
				fillOpacity.hover := 1.0,
				key := ~rowid_
			) %>%
			add_tooltip(vis_tooltip, "hover") %>%
			add_axis("x", title = xvar_name) %>%
			add_axis("y", title = yvar_name) %>%
			set_options(width = 1200, height = 600, duration = 0)

		if (stroke_name != "CONSTANT") {
			graphy <- graphy %>% add_legend(scales = "stroke", properties = legend_props(legend = list(y = 60)))
		}
		if (size_name != "CONSTANT") {
			graphy <- graphy %>% add_legend(scales = "size", properties = legend_props(legend = list(y = 120)))
		}

		return(graphy)
	})

	vis %>% bind_shiny("chartG")

	## plotly ###########################################

	output$chartP <- renderPlotly({
		xvar_name <- input$xvarP
		yvar_name <- input$yvarP
		zvar_name <- input$zvarP

		data <- values$rawlocdata

		tooltip <- c()
		for(name in names(data)) {
			tooltip <- paste0(tooltip, "<b>", name, "</b>: ", data[[name]], "<br>")
		}

		sizeBy <- input$sizeP
		sizeData <- data[[sizeBy]]
		if (sizeBy == "CONSTANT" || !is.numeric(sizeData)) {
			sizeData <- 15
		} else {
			sizeData <- sizeData / max(sizeData) * 15
		}

		symbolData <- data[[input$shapeP]]
		symbols3d <- c("circle", "circle-open", "square", "square-open", "diamond", "diamond-open", "cross", "x")
		if (!is.numeric(symbolData)) {
			symbolData <- as.numeric(factor(symbolData))
		}
		if (zvar_name != "CONSTANT") {
			symbolData <- symbols3d[symbolData %% length(symbols3d)]
		}

		colorData <- data[[input$colorP]]
		if (!is.numeric(colorData)) {
			colorData <- as.numeric(factor(colorData))
		}

		plot_ly(
			data = data,
			x = if (xvar_name != "CONSTANT") data[[xvar_name]] else 0,
			y = if (yvar_name != "CONSTANT") data[[yvar_name]] else 0,
			z = if (zvar_name != "CONSTANT") data[[zvar_name]] else NULL,
			text = tooltip,
			marker = list(
				size = sizeData,
				symbol = symbolData,
				color = colorData,
				colorscale = "Blues",
				opacity = 0.5
			),
			type = if (zvar_name != "CONSTANT") "scatter3d" else "scatter",
			mode = "markers"
		) %>%
		layout(
			title = "",
			xaxis = list(
				"title" = xvar_name
			),
			yaxis = list(
				"title" = yvar_name
			),
			scene = list(
				xaxis = list(
					"title" = xvar_name
				),
				yaxis = list(
					"title" = yvar_name
				),
				zaxis = list(
					"title" = zvar_name
				)
			)
		)
	})

	## Data Explorer ###########################################

	output$loctable <- DT::renderDataTable({
		cleantable <- values$rawlocdata
		action <- DT::dataTableAjax(session, cleantable)
		DT::datatable(cleantable, options = list(ajax = list(url = action)), escape = FALSE)
	})
})
