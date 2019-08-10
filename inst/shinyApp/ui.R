ui <- fluidPage(

  shinyjs::useShinyjs(),

  titlePanel("WordBox Vokabeltrainer"),

  sidebarLayout(
    # Sidebar with inputs ######
    sidebarPanel(
      selectInput("wordlist_file",
                  "W\u00f6rterliste laden",
                  "kein Verzeichnis ausgew\u00e4hlt",
                  width = "300px"),
      actionButton("load",
                   "Laden",
                   width = "100px"),
      br(), br(),
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
      checkboxInput("training", "Trainingsmodus"),
      selectInput("group",
                  "Gruppe ausw\u00e4hlen",
                  "keine W\u00f6rterliste geladen",
                  width = "300px"),
      shinyjs::disabled(
        actionButton("run",
                     "Start",
                     width = "100px")
      )
    ),

    # main panel #####
    mainPanel(
      fluidRow(
        column(8,
               "Anzahl verbleibende W\u00f6rter:",
               div(style = "display:inline-block",
                   textOutput("n_words")),
               br(), "Anzahl richtige Antworten: ",
               div(style = "display:inline-block",
                   textOutput("n_correct")),
               br(), "Anzahl falsche Antworten: ",
               div(style = "display:inline-block",
                   textOutput("n_wrong")),
               br(), br(),
               "Fach: ",
               div(style = "display:inline-block",
                   textOutput("current_box")),
               br(), "Gruppe: ",
               div(style = "display:inline-block",
                   textOutput("current_group")),
               br(), br()),
        column(4, plotOutput("dot", height = "200px"))
      ),
      fluidRow(
        strong("Aufgabe"),
        textOutput("question"),
        br(),
        uiOutput("exerciseUI")
      )
    )
  )
)
