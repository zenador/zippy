source("constants.R")

library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

shinyServer(function(input, output, session) {

	## Interactive Map ###########################################

	# Create the map
	output$map <- renderLeaflet({
		leaflet() %>%
			addTiles(
				urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
				attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
			) %>%
			setView(lat = defaultLat, lng = defaultLong, zoom = defaultZoom)
	})

	# This observer is responsible for maintaining the circles and legend,
	# according to the variables the user has chosen to map to color and size.
	observe({
		colorBy <- input$color
		sizeBy <- input$size

		locdata <- values$locdata

		colorData <- locdata[[colorBy]]
		pal <- colorBin("Spectral", colorData, 7, pretty = FALSE)

		radius <- locdata[[sizeBy]] / max(locdata[[sizeBy]]) * 1500

		leafletProxy("map", data = locdata) %>%
			clearShapes() %>%
			addCircles(~longitude, ~latitude, radius=radius, stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
			addLegend("bottomleft", pal=pal, values=colorData, title=colorBy, layerId="colorLegend")
	})

	# Show a popup at the given location
	showItemPopup <- function(itemid, lat, lng) {
		locdata <- values$rawlocdata

		selectedItem <- locdata[locdata$longitude == lng & locdata$latitude == lat,]

		x <- list()
		#j = 1
		for(i in names(selectedItem)) {
			taggy <- tags$h5(paste(i, ":", sep=""), selectedItem[[i]])
			x <- list(x, taggy)
			#x[j] <- taggy
			#j <- j + 1
		}

		# x <- list(tags$h5("longitude:", selectedItem$longitude),
		# 	tags$h5("latitude:", selectedItem$latitude),
		# 	tags$h5("level:", selectedItem$level))

		# x <- list(tags$h5("longitude:", selectedItem[["longitude"]]),
		# 	tags$h5("latitude:", selectedItem[["latitude"]]),
		# 	tags$h5("level:", selectedItem[["level"]]))

		# x <- list()
		# x <- list(x, tags$h5("longitude:", selectedItem$longitude))
		# x <- list(x, tags$h5("latitude:", selectedItem$latitude))
		# x <- list(x, tags$h5("level:", selectedItem$level))

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
		updateSelectInput(session = session, inputId = "color", choices = vars, selected = input$color)
		updateSelectInput(session = session, inputId = "size", choices = vars, selected = input$size)
	})

	## Data Explorer ###########################################

	output$loctable <- DT::renderDataTable({
		cleantable <- values$rawlocdata
		action <- DT::dataTableAjax(session, cleantable)
		DT::datatable(cleantable, options = list(ajax = list(url = action)), escape = FALSE)
	})
})
