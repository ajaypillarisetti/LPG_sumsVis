### Ajay Pillarisetti, University of California, Berkeley, 2015
### V1.0G

Sys.setenv(TZ="GMT")

shinyServer(function(input, output) {
	#read in data
	datasetInput <- reactive({
	    devid <- log.sheet[mid==input$mid, unique(device_id)]
	    devid <- laply(devid, function(x) substring(x, 2, nchar(x)))
		a <- llply(devid, function(x){grep(x, files, value=T)})[[1]]
		a <- as.data.table(ldply(a, function(x){fread(x)}))
		a[,device_id:=substring(device_id, 2, nchar(device_id))]
		all <- merge(a, log.sheet, by='device_id', all.x=T)
		all[,datetime:=ymd_hms(datetime)]
		all[,location:=as.character(location)]
		log.location[,location:=as.character(location)]
		setkey(all, 'location')
		setkey(log.location, 'location')
		all <- log.location[all]
		setkey(all)
		all[,serial:=substring(serial,1,16)]
		all <- unique(all)
		# all[as.Date(datetime)>=input$dateSelect[1] & as.Date(datetime)<=input$dateSelect[2]]
	})

	data_cleaned <- reactive({
		if (is.null(datasetInput())) return(NULL)
		data_d <- datasetInput()[,with=F]
	})

	####################
	##### datasets ##### 
	####################
	dataXTS.plainplot <- reactive({
		dta<-data_cleaned()
		dta.wide <- dcast.data.table(dta, datetime~description+device_id, value.var='temp')
		cols <- colnames(dta.wide)[colnames(dta.wide)!='datetime']
		dta.wide <- dta.wide[, lapply(.SD, as.numeric), by=datetime]
		cols <- colnames(dta.wide)[colnames(dta.wide)!='datetime']
		as.xts(dta.wide)
	})

	####################
	##### dygraphs ##### interactivity - to subset data to user-selected range
	####################
	output$graphTitle <- renderText({paste("Time Series Graph:", paste(from(), to(), sep=" to ")," ")}) 

	#UI OUTPUTS
	fileMin <- reactive({data_cleaned()[,min(temp)]})
	fileMax <- reactive({data_cleaned()[,max(temp)]})
	fileSamplingInterval <- reactive({as.numeric(data_cleaned()[10,'datetime',with=F]-data_cleaned()[9,'datetime',with=F])})
	output$selectMID <- renderUI({selectInput('mid', "Maternal ID", log.sheet[!is.na(mid),sort(unique(mid))])})
	output$selectedMID <- renderText({paste("Time-series plot (", input$mid, ")", sep="")})
	output$selectFiles <- renderUI({
    	if(is.null(input$mid)) return()
	    files <- log.sheet[mid==input$mid, unique(device_id)]
	    checkboxGroupInput("files", "Choose stoves", choices  = files, selected = files)
  	})

	####################
	####### Boxes ###### 
	####################
	# Overview Page 	
	output$maxTemp <- renderInfoBox({
		if (is.null(datasetInput())) return(NULL)
		maxTemp <- data_cleaned()[,max(temp)]
		maxThreshold <- 85
		deviceIDs <- data_cleaned()[temp>maxThreshold, unique(device_id)]
		infoBox(
			value = if (maxTemp >= maxThreshold) paste(deviceIDs, description, collapse=", ") else formatC(maxTemp, digits = 2, format = "f"),
			title = if (maxTemp >= maxThreshold) "Warning: 85C Exceeded" else "Max Temp",
			icon = if (maxTemp >= maxThreshold)icon("warning") else icon("chevron-circle-up"),
			color = if (maxTemp >= maxThreshold) "red" else "green"
		)
	})

	output$minTemp <- renderInfoBox({
		if (is.null(datasetInput())) return(NULL)
		minTemp <- data_cleaned()[,min(temp)]
		minThreshold <- 0
		deviceIDs <- data_cleaned()[temp<=minThreshold,unique(device_id)]
		infoBox(
			value = if (minTemp <= minThreshold) paste(deviceIDs, description, collapse=", ") else formatC(minTemp, digits = 2, format = "f"),
			title = if (minTemp <= minThreshold) "Warning: Values Below 0" else "Min Temp",
			icon = if (minTemp <= minThreshold) icon("warning") else icon("chevron-circle-down"),
			color = if (minTemp <= minThreshold) "red" else "green"
		)
	})

	output$avgDailyRange <- renderInfoBox({
		if (is.null(datasetInput())) return(NULL)
		data_cleaned()[,yday:=yday(datetime)]
		data_cleaned()[,temp:=as.numeric(temp)]
		data_cleaned()[,dailyRange:=max(temp)-min(temp),by='yday']
		avgDailyRange <- data_cleaned()[location!=12,mean(dailyRange)]
		dailyRangeThreshold <- 15
		deviceIDs <- data_cleaned()[avgDailyRange<=dailyRangeThreshold,unique(device_id)]
		infoBox(
			value = if (avgDailyRange <= dailyRangeThreshold) paste(deviceIDs, collapse=", ") else formatC(avgDailyRange, digits = 2, format = "f"),
			title = if (avgDailyRange <= dailyRangeThreshold) paste("Warning: Avg Daily Range ", formatC(avgDailyRange, digits = 2, format = "f"), sep="") else "Avg Daily Range (non-ambient)",
			icon = if (avgDailyRange <= dailyRangeThreshold) icon("warning") else icon("chevron-circle-down"),
			color = if (avgDailyRange <= dailyRangeThreshold) "yellow" else "green"
		)
	})

	####################
	###### Tables ###### 
	####################
	diagnostics <- reactive({
		datasetInput()[, list(
			Start_Date =as.Date(min(datetime)),
			End_Date = as.Date(max(datetime)),
			Max_Temp = max(temp),
			Min_Temp = min(temp)
		), by='device_id,location']
	})

	output$diagnosticsOutput <- renderDataTable(diagnostics(), options=list(searchable = FALSE, searching = FALSE, pageLength = 20,rowCallback = I('function(row, data) {
			if (data[5] >= 85) 
				$("td", row).css({"background" : "red", "color" : "white"});
			else if (data[6] < 0) 
				$("td", row).css({"background" : "blue", "color" : "white"});
			;}')))

	####################
	####### PLOTS ###### 
	####################
	output$plainPlot<- 
	renderDygraph({
		# if (is.null(datasetInput())) return(NULL)
		dygraph(dataXTS.plainplot()) %>% 
	    dyOptions(axisLineWidth = 1.5, fillGraph = F, drawGrid = FALSE, useDataTimezone=T, strokeWidth=1, connectSeparatedPoints=T) %>%
	    dyAxis("y", label = "Temp C") %>%
	    dyAxis("x", label = "Date & Time") %>%
        dyLegend(labelsDiv = "legendARY")
	})
})