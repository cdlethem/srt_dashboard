require(jsonlite)
require(dplyr)
require(reshape)
require(ggplot2)
require(ggthemes)
require(shiny)
require(DT)
require(shinythemes)

pipeline_statuses <- c(
  "Signed",
  "ANS",
  "ANS - Expired",
  "Declined",
  "Decision Pending",
  "Waitlist",
  "Reject",
  "Interview Pending",
  "Warm lead",
  "Dead lead"
)

shinyUI(fluidPage(theme = shinytheme("lumen"),
                  sidebarLayout(
                    sidebarPanel(
                      width = 3,
                      tags$head(tags$style(
                        HTML(
                          ".multicol{font-size:12px;
                          height:auto;
                          -webkit-column-count: 2;
                          -moz-column-count: 2;
                          column-count: 2;
                          }
                          
                          div.checkbox {margin-top: 0px;}
                          div.radio {margin-top: 0px}
                          hr {margin-top: 5px;
                              margin-bottom: 5px}"
                              
                        )
                        )),
h3("Filters"),
helpText("These filters control which prospies appear on the call lists"),

hr(),

h4("SRT Liaison"),

tags$div(
  align = "left",
  class = "multicol",
  radioButtons(
  inputId = "srt_liaison",
  label = NULL,
  choices = list("All",
                 "rita",
                 "Amanda Guo",
                 "Louis Von",
                 "Melinda Yang"),
  selected = "All"
)),

hr(),

h4("Graduation Year"),

tags$div(
  align = "left",
  class = "multicol",
  checkboxGroupInput(
    label = NULL,
    inputId =  "grad_year",
    choices = list("2017",
                   "2018",
                   "2019",
                   "2020",
                   "2021",
                   "2022",
                   "2023"),
    selected = c("2017",
                 "2018",
                 "2019",
                 "2020",
                 "2021",
                 "2022",
                 "2023")
  )
),

hr(),

h4("Market"),

tags$div(
  align = "left",
  class = "multicol",
  checkboxGroupInput(
  inputId = 'market',
  label = NULL,
  choices = list(
    "Beijing/Tianjin",
    "Other China",
    "International Boarding School",
    "Graduate School Applicant",
    "Transfer Student"
  ),
  selected = c(
    "Beijing/Tianjin",
    "Other China",
    "International Boarding School",
    "Graduate School Applicant",
    "Transfer Student"
  ))),

uiOutput("region_ui"),

hr(),

h4("Pipeline Status"),

tags$div(
  align = "left",
  class = "multicol",
  checkboxGroupInput(
  inputId = "pipeline_status",
  label = NULL,
  choiceNames = pipeline_statuses,
  choiceValues = c(1:10),
  selected = 9
)),

hr(),

uiOutput("events_attended_ui")


                        ),

mainPanel(width = 9,
          tabsetPanel(tabPanel(
            "Call Lists",
            DTOutput("call_list")
          )))
                      )))