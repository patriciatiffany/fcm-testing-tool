#helper.R
#Author: Patricia Angkiriwang, University of British Columbia; based on code by Brian Gregor, Oregon Systems Analytics LLC
#Copyright: 2016, Oregon Department of Transportation 2016
#Modifications copyright: 2019, Patricia Angkiriwang
#License: Apache 2

#LOAD RESOURCES
#--------------
#Packages
library(shiny)
library(shinyBS)
library(DT)
library(ggplot2)
library(DiagrammeR)
#Function to support text area inputs
textareaInput <- function(id, label, value="", rows=5, cols=40, class="form-control"){
  tags$div(
    class="form-group shiny-input-container",
    tags$label('for'=id,label),
    tags$textarea(id=id,class=class,rows=rows,cols=cols,value))
}


#SHINY UI FUNCTION
#-----------------
shinyUI(
  navbarPage(
    "Qualitative Systems Modeller",
    
    #Introduction Screen
    #-------------------
    tabPanel("About",
      fluidPage(
        titlePanel(span("About")),
        #br(),
        h3("What does this tool do?"),
        p("The Qualitative Systems Modeller is an application for modelling qualitative, directed graphs. A directed graph is composed of 'nodes' and 'edges', where ", strong("nodes are concepts"), " that are being modeled and ", strong("edges specify relationships between concepts"), ". The following figure illustrates a simple qualitative systems model."),
        #img(src = "demo_graph.png", height = 200, width = 275, style = "display: block; margin-left: auto; margin-right: auto"),
        p("The direction of each edge (i.e. the direction of the arrow) specifies the relationship between causal and affected concepts. Edge weights specify the strength (Very Low to Very High) and directionality (+ or -) of causal effects. A positive sign for an edge weight means that an increase in the causing concept causes an increase in the affected concept."),
        hr(),
        h4("Copyright and License"),
        p("This application was built based off of The Logic Laboratory, an R Shiny application developed by Brian Gregor (Oregon Systems Analytics) with funding from the Oregon Department of Transportation (Copyright 2016). The original software is licensed with the Apache 2 open source license. Terms of the license are included in the LICENSE text file. Additional notices are included in the NOTICE text file.")
      )
    ),
    tabPanel("0) Enter User Info",
             mainPanel(
               h4("User Information"),
               hr(),
               p("The user information entered below is used to attribute model creation and editing."),
               textInput("firstName", "First Name"),
               textInput("lastName", "Last Name"),
               textInput("organization", "Organization (optional)"),
               checkboxInput("anonymous", "Keep me anonymous", FALSE)
             )
    ),
    tabPanel( "1) The Model",
              navbarPage("Set up your model",
                         tabPanel( "Upload model",
                                   sidebarLayout(
                                     sidebarPanel(
                                       radioButtons(
                                         inputId = "modelAction", 
                                         label = "Model Action",
                                         choices = list("Upload an Excel File" = "select_xl",
                                                        "Select an Existing Model" = "select_existing")
                                       ),
                                       
                                       # Upload a model from an excel file
                                       #--------------------
                                       conditionalPanel(condition = "input.modelAction == 'select_xl'",
                                                        p("Upload an excel file (.xlsx) here. The single workbook file should contain 2 spreadsheets:"),
                                                        p("The first sheet should contain the list of concepts/elements, with columns:"),
                                                        tags$ol(
                                                          tags$li("ID"), 
                                                          tags$li("Concept name"), 
                                                          tags$li("Group or Type (optional)"), 
                                                          tags$li("Description (optional)"), 
                                                          tags$li("Min (optional; default = 0)"), 
                                                          tags$li("Max (optional; default = 100))")),
                                                        p("The second sheet within the same workbook should contain the list of relationships/ connections, with columns:"),
                                                        tags$ol(
                                                          tags$li("From (ID of the causal concept)"), 
                                                          tags$li("To (ID of the affected concept)"), 
                                                          tags$li("Direction (e.g. Positive or Negative)"), 
                                                          tags$li("Weight/ Importance (L,M,H)"),
                                                          tags$li("Description (optional)"),
                                                          tags$li("Label (e.g. + or -)")),
                                                        hr(),
                                                        textInput("modelName", "Model Name"),
                                                        # -------- interface to upload files
                                                        fileInput("xlFileName", "Choose Excel File",
                                                                  accept = c(
                                                                    ".xls",".xlsx")
                                                        ),
                                                        checkboxInput("uploadheader", "My file contains a header", TRUE)
                                       ),
                                       conditionalPanel(
                                         condition = "input.modelAction == 'select_existing'",
                                         uiOutput("selectExistingModelFile")
                                       ),
                                       actionButton("startModeling", "Start Working on Model"),
                                       bsAlert("nofileAlert"),
                                       bsAlert("nonameAlert"),
                                       bsAlert("duplicateAlert")
                                     ),
                                     mainPanel(
                                       tabsetPanel(
                                       tabPanel("Concepts", hr(),DT::dataTableOutput("conceptsTable"), value = "table"),
                                       tabPanel("Relations", hr(), DT::dataTableOutput("relationsTable"), value = "table"))
                                     )
                                     
                                   ) # sidebarLayout
                         ), # tabPanel: upload model 
                         tabPanel("Edit Concepts",
                                   sidebarLayout(
                                     sidebarPanel(
                                       h4("Edit Concepts"),
                                       hr(),
                                       textInput("conceptName", "Concept Name"),
                                       textInput("conceptID", "ID (one-word identifier)"),
                                       textareaInput("conceptDesc", "Concept Description (optional)"),
                                       textInput("minValue", "Minimum Value"),
                                       textInput("maxValue", "Maximum Value"),
                                       textareaInput("valuesDesc", "Values Description (optional)"),
                                       textInput("conceptGroup", "Concept Group"),
                                       conditionalPanel(
                                         condition = "input.modelAction != 'runModel'",
                                         wellPanel(
                                           actionButton("addConcept", "New"),
                                           actionButton("updateConcept", "Update"),
                                           actionButton("deleteConcept", "Delete"),
                                           actionButton("undoConceptAction", "Undo"),
                                           bsAlert(
                                             "duplicateConceptName"
                                           ),
                                           bsAlert(
                                             "duplicateConceptVariable"
                                           )
                                         ),
                                         actionButton("saveModel1","Save Model")
                                       ) #end: conditionalPanel
                                     ),
                                     mainPanel(
                                       tabPanel("Concepts", DT::dataTableOutput("conceptsTableEditing"), value = "table")
                                     )
                                   )
                                   
                         ), # tabPanel: edit concepts
                         tabPanel("Edit Relationships",
                                  sidebarLayout(
                                    sidebarPanel(
                                      h4("Edit Relationships"),
                                      hr(),
                                      uiOutput("selectCausalConcept"),
                                      uiOutput("selectAffectedConcept"),
                                      selectInput(inputId = "causalDirection", 
                                                  label = "Causal Direction", 
                                                  choices = c("" ,"Positive", "Negative")),
                                      selectInput(inputId = "causalStrength", 
                                                  label = "Causal Strength", 
                                                  choices = c("", "VL", "L", "ML", "M", "MH", "H", "VH")),
                                      textareaInput("causalDesc", "Causal Description"),
                                      conditionalPanel(
                                        condition = "input.modelAction != 'runModel'",
                                        wellPanel(
                                          #actionButton("addRelation", "New"), #inactive - currently, all assumed to have something
                                          actionButton("updateRelation", "Add/ Update"),
                                          actionButton("deleteRelation", "Delete"),
                                          actionButton("undoRelationAction", "Undo")
                                        ), # end: wellPanel
                                        actionButton("saveModel2","Save Model")
                                      ) #end: conditionalPanel
                                    ), # sidebarPanel
                                    mainPanel(
                                      tabPanel("Relations", DT::dataTableOutput("relationsTableEditing"), value = "table")
                                    ) #mainPanel
                                  ) # sidebarLayout
                         ), # tabPanel: edit relationships
                         tabPanel(
                           title = "Visualize Model",
                           grVizOutput('relations_graph', width = "100%", height = "100%")#"800px")
                         ) #tabPanel:Visualize Model
              ) # navbarPage: model setup                   
    ) # tab (1 - model)
)) # shinyUI, navbarPage
