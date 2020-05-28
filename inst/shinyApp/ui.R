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
      radioButtons("direction",
                  "Richtung ausw\u00e4hlen",
                  c(">" = "direction1",
                    "<" = "direction2")),
      radioButtons("mode",
                  "Modus ausw\u00e4hlen",
                  c("schriftlich" = "written",
                    "m\u00fcndlich" = "oral")),
      radioButtons("quiztype",
                   "Quizart auswählen:",
                   choices = c("Normal" = "standard",
                               "Training" = "training",
                               "Neue W\u00f6rter" = "newwords")),
      selectInput("groups",
                  "Gruppen ausw\u00e4hlen",
                  choices = NULL,
                  multiple = TRUE,
                  width = "300px"),
      sliderInput("n_words",
                  "Anzahl W\u00f6rter",
                  min = 5,
                  max = 95,
                  step = 5,
                  value = getOption("wordbox_n_words_default")),
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
               br(),
               # only show the wrong answers if set by option
               if (getOption("wordbox_show_errors")) {
                 "Anzahl falsche Antworten: "
                 },
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
