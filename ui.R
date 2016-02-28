dashboardPage(
  dashboardHeader(title = "CheckSUMs"),
  dashboardSidebar(
    sidebarMenu(
	    menuItem("Overview", tabName = "overview",  icon = icon("list-alt")),
	    menuItem("SUMs Diagnostics", tabName = "diagnostics",  icon = icon("area-chart"))
    	# menuItem("Raw data", tabName = "rawdata",  icon = icon("tasks"))
        # menuItem("Merge Files", tabName = "merge",  icon = icon("files-o"))
    )
  ),
	dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
            tags$script(type='text/javascript', src='dygraph-extra.js'),
            tags$script(type='text/javascript', src='scripties.js')
            ),

    	tabItems(
			tabItem("overview",
                fluidRow(
                    box(width=4, collapsible = TRUE, status="info", solidHeader=TRUE, title="Introduction",
                        h5('Welcome to CheckSUM!'),
                        p("CheckSUM is a simple tool to visualize and quickly check the status of stove use monitors in the field. Begin by selecting the date range of interest and maternal ID at right.")
                    ),
                    column(width=4,
                        box(width=NULL, collapsible = FALSE, status='info', solidHeader=T, title='Filter Data',
                            dateRangeInput("dateSelect", label='Select a Date', start = Sys.Date()-7, end=Sys.Date()),
                            uiOutput('selectMID')
                        )
                        # box(width=NULL, collapsible = FALSE, status='info', solidHeader=T, title='Select an MID.')
                    ),
                    box(width=4, collapsible = FALSE, status='info', solidHeader=T, title='Select files', uiOutput('selectFiles'), HTML('<small><em>parentheses contain total date range available for a specific stove in a specific home</em></small>'))
                ),
                fluidRow(
                    box(
                        width = 8, height='510px',
                        status = "info", solidHeader = TRUE,
                        title = textOutput("selectedMID"),
                        dygraphOutput('plainPlot', height='445px')
                    ),
                    box(textOutput("legendARY"), title = "Legend", collapsible = TRUE, status='info', solidHeader=TRUE, width=4),
                    infoBoxOutput('maxTemp'),
                    infoBoxOutput('minTemp'),
                    infoBoxOutput('avgDailyRange')

                )

			),      
    		tabItem("diagnostics",
                fluidRow(
                    box(title = "SUMs Placement Summary",
                        width = 12,
                        status="info", solidHeader = TRUE,
                        # downloadButton("downloadThresholdCSV", "Download CSV"),
                        dataTableOutput('diagnosticsOutput')
                    )
	      	    )
            ),
	    	tabItem("rawdata",
    	  		downloadButton("downloadCSV", "Download as CSV"),
    	  		HTML("<BR><BR>"),
				verbatimTextOutput("allDataTable")
	      	)               
    	)	
	)
)