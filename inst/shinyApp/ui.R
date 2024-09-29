ui <- fluidPage(

  theme = bslib::bs_theme(version = 5, preset = "darkly", font_scale = 0.82),

  # icon colours taken from the bootstrap theme
  # .btn-danger:hover, background-color
  tags$style(".fa-circle-xmark {color:#e12e1c}"),
  # .btn-success:hover, background-color
  tags$style(".fa-circle-check {color:#009670}"),
  # .btn-info:hover, background-color
  tags$style(".fa-circle-question {color:#2384c6}"),
  # .btn-warning:hover, background-color
  tags$style(".fa-triangle-exclamation {color:#d4860b}"),

  # function to set focus on an element
  shinyjs::extendShinyjs(
    text = "shinyjs.refocus = function(e_id) {
              document.getElementById(e_id).focus();
            }",
    functions = "refocus"
  ),

  shinyjs::useShinyjs(),

  titlePanel("WordBox Vokabeltrainer"),

  sidebarLayout(
    # Sidebar with inputs ######
    sidebarPanel(
      selectInput("wordlist_file",
                  tags$b("W\u00f6rterliste laden"),
                  "kein Verzeichnis ausgew\u00e4hlt",
                  width = "300px"),
      actionButton("load",
                   "Laden",
                   width = "100px",
                   class = "btn btn-primary"),
      br(), br(),
      shinyWidgets::awesomeRadio("direction",
                                 tags$b("Richtung ausw\u00e4hlen"),
                                 c(">" = "direction1",
                                   "<" = "direction2")),
      shinyWidgets::awesomeRadio("mode",
                                 tags$b("Modus ausw\u00e4hlen"),
                                 c("schriftlich" = "written",
                                   "m\u00fcndlich" = "oral")),
      shinyWidgets::awesomeRadio("quiztype",
                                 tags$b("Quizart ausw\u00e4hlen:"),
                                 choices = c("Normal" = "standard",
                                             "Training" = "training",
                                             "Neue W\u00f6rter" = "newwords")),
      tags$p(tags$b("Filter:")),
      shinyWidgets::awesomeCheckbox("core_only",
                                    "Nur Kernwortschatz",
                                    value = FALSE),
      shinyWidgets::awesomeCheckbox("exam_only",
                                    "Pr\u00fcfungsvorbereitung",
                                    value = FALSE),
      selectInput("groups",
                  tags$b("Gruppen ausw\u00e4hlen"),
                  choices = NULL,
                  multiple = TRUE,
                  width = "300px"),
      sliderInput("n_words",
                  tags$b("Anzahl W\u00f6rter"),
                  min = 5,
                  max = 95,
                  step = 5,
                  value = getOption("wordbox_n_words_default")),
      fluidRow(
        column(8,
               shinyjs::disabled(
                 actionButton("run",
                              "Start",
                              width = "100px",
                              class = "btn btn-primary")
               )),
        column(4,
               actionLink("dashboard",
                          "Dashboard",
                          class = "btn text-info")
        )
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
               br(), br())
      ),
      fluidRow(
        column(8,
          strong("Aufgabe"),
          textOutput("question"),
          br(), br(),
          uiOutput("exerciseUI")
        )
      )
    )
  )
)
