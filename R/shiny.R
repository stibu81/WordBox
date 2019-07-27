# Define the UI #####
ui <- fluidPage(

    titlePanel("WordBox Vokabeltrainer"),

    # Sidebar with inputs
    sidebarLayout(
        sidebarPanel(
            fileInput("wordlist_file",
                      "W\u00f6rterliste laden",
                      width = "300px",
                      accept = ".csv",
                      buttonLabel = "Suche...",
                      placeholder = "keine W\u00f6rterliste geladen"),
            selectInput("direction",
                        "Richtung ausw\u00e4hlen",
                        "keine W\u00f6rterliste geladen",
                        width = "300px"),
            selectInput("mode",
                        "Modus ausw\u00e4hlen",
                        c("schriftlich" = "written",
                          "m\u00fcndlich" = "oral"),
                        selected = "written",
                        width = "300px"),
            actionButton("run",
                         "Start",
                         width = "100px")
        ),

        # main panel
        mainPanel(
            textOutput("current_box"),
            textOutput("current_group")
            #
            #  # written mode
            #  conditionalPanel(
            #      condition = "input.mode == 'written'",
            #      tableOutput("wordlist")
            # ),
            # # oral mode
            # conditionalPanel(
            #      condition = "input.mode == 'oral'",
            #      tableOutput("wordlist")
            # )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    # load a file
    wl <- reactive({
        file <- input$wordlist_file
        if (is.null(file)) {
            return(NULL)
        } else {
            return(read_wordlist(file$datapath))
        }
    })

    # update selection box for direction
    observe({
        if (!is.null(wl())) {
            langs <- get_languages(wl())
            choices <- magrittr::set_names(paste0("direction", 1:2),
                                           paste0(langs, " > ", rev(langs)))
            updateSelectInput(session, "direction", choices = choices)
        }
    })

    current <- reactive({
        wl()[1, ]
    })

    output$current_box <- reactive({
        if (input$run > 0) {
            if (input$direction == "direction1") {
                paste("Fach:", current()$box1)
            } else {
                paste("Fach:", current()$box2)
            }
        } else {
            "Fach:"
        }
    })
    output$current_group <- reactive({
        if (input$run > 0) {
            paste("Gruppe:", current()$group)
        } else {
            "Gruppe:"
        }
    })

    # create diagnostic output, if the run
    # button is clicked for the first time
    observe({
        if (input$run == 1) {
            cat("Starting exercise with direction",
                input$direction, "and mode",
                input$mode, file = stderr())
        }
    })
}

#' Run the application
#'
#' @export

run_wordbox <- function() {
    shiny::shinyApp(ui = ui, server = server)
}
