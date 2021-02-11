#helper.R
# Author: Patricia Angkiriwang, University of British Columbia; 
# based on code by Brian Gregor, Oregon Systems Analytics LLC

# === LOAD RESOURCES ==================
# Packages
library(shiny)
library(shinyBS)
library(plotly)
library(DT)
library(ggplot2)
library(DiagrammeR)
# Function to support text area inputs
textareaInput <- function(id, label, value="", rows=5, cols=40, class="form-control"){
  tags$div(
    class="form-group shiny-input-container",
    tags$label('for'=id,label),
    tags$textarea(id=id,class=class,rows=rows,cols=cols,value))
}


# === SHINY UI FUNCTION ==================
shinyUI(
  navbarPage(
    "Qualitative Systems Modeller",
    # Introduction Screen -------------------
    tabPanel("About",
             fluidPage(
               titlePanel(span("About")),
               #br(),
               h3("What does this tool do?"),
               p("The Qualitative Systems Modeller is an application for modelling qualitative, directed graphs. A directed graph is composed of 'nodes' and 'edges', where ", strong("nodes are concepts"), " that are being modeled and ", strong("edges specify relationships between concepts"), "."),
               #img(src = "demo_graph.png", height = 200, width = 275, style = "display: block; margin-left: auto; margin-right: auto"),
               p("The direction of each edge (i.e. the direction of the arrow) specifies the relationship between causal and affected concepts. Edge weights specify the strength (Very Low to Very High) and directionality (+ or -) of causal effects."), # A positive sign for an edge weight means that an increase in the causing concept causes an increase in the affected concept."),
               hr(),
               h4("Copyright and License"),
               p("This application was loosely built based on The Logic Laboratory, an R Shiny application developed by Brian Gregor (Oregon Systems Analytics) with funding from the Oregon Department of Transportation (2016). The original software is licensed with the Apache 2 open source license.")
             )
    ),
    # User Info -------------------
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
              # Model upload -------------------
                         tabPanel( "Upload model",
                                   sidebarLayout(
                                     sidebarPanel(
                                       radioButtons(
                                         inputId = "modelAction", 
                                         label = "Model Action",
                                         choices = list("Select an Existing Model" = "select_existing"),
                                         selected = "select_existing"
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
                                         tabPanel("Concepts", hr(),DT::dataTableOutput("conceptsTable"), value = "concepts_table"),
                                         tabPanel("Relations", hr(), DT::dataTableOutput("relationsTable"), value = "relations_table"))
                                     )
                                   ) # sidebarLayout
                         ), # tabPanel: upload model 
              # Model edit -------------------
                         tabPanel("Edit Concepts",
                                  sidebarLayout(
                                    sidebarPanel(
                                      h4("Edit Concepts"),
                                      hr(),
                                      textInput("conceptName", "Concept Name"),
                                      textInput("conceptID", "ID (one-word identifier)"),
                                      textareaInput("conceptDesc", "Concept Description (optional)"),
                                      # textInput("minValue", "Minimum Value"),
                                      # textInput("maxValue", "Maximum Value"),
                                      # textareaInput("valuesDesc", "Values Description (optional)"),
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
                                      hr(), 
                                      sliderInput(inputId = "relK", label = "Inertia of affected concept (k)",
                                                  min=0, max=1, step = 0.1, value = 1),
                                      selectInput(inputId = "relType", label = "Logical conjunction (Type)",
                                                  choices = c("ADD"="add","REQ"="req")),
                                      numericInput(inputId = "relGrouping", label = "Relation group number", value = 1),
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
                                      tabPanel("Relations", DT::dataTableOutput("relationsTableEditing"), value = "relations_edit")
                                    ) #mainPanel
                                  ) # sidebarLayout
                         ), # tabPanel: edit relationships
                         tabPanel("Edit Weight Values",
                           sidebarLayout(
                             sidebarPanel(
                               h4("Edit numerical values for edge weights"),
                               hr(),
                               selectInput(inputId = "qualWeight", 
                                           label = "Qualitative Causal Strength", 
                                           choices = c("", "VL", "L", "ML", "M", "MH", "H", "VH"),
                                           selected = "M"),
                               sliderInput(inputId = "quantWeight",
                                           label = "Numerical value",
                                           min=0, max=1, step = 0.05, value = 1),
                               actionButton("updateWeight", "Update"),
                             ),
                             mainPanel(
                               tableOutput("weightsTable")
                             )
                           )
                         ), # tabPanel: edit edge weight values (qualitative -> quantitative values)
                         tabPanel(
                           title = "Visualize Model",
                           grVizOutput('relations_graph', height = "80%")#"800px") width = "80%", 
                         ) #tabPanel:Visualize Model
              ) # navbarPage: model setup                   
    ), # tab (1 - model)
    # Run model with varying parameters --------
    tabPanel("2) FCM Exploration",
             sidebarLayout(
               sidebarPanel(
                 grVizOutput('relations_graph2', height = "150px"),
                 numericInput("numIterations", 
                              "Timesteps to run", 
                              value = 30),
                 hr(),
                 tabsetPanel(type = "tabs",
                     tabPanel("Set Parameters",
                              hr(),
                              selectInput("selectFCM_fn", label = "FCM thresholding function",
                                          choices = c("Sigmoid (Logistic)"="sigmoid-exp","Sigmoid (Tanh)"="sigmoid-tanh","Linear (f(x) = x-h)"="linear"), selected="sigmoid-exp"),
                              plotOutput("FCMFunction", height = "250px"),
                              sliderInput("sliderFCM_h", "Choose h", min=-1, max=1, step = 0.25, value = 0), 
                              uiOutput("initSlider"),
                              conditionalPanel(
                                condition = "input.selectFCM_fn != 'linear'",
                                sliderInput("sliderFCM_lambda", "Choose lambda", min=0, max=10, step = 0.25, value = 3)
                              )
                     ),
                     tabPanel("Add Constraints",
                              hr(),
                              p('Current constraints:'),
                              tableOutput("constraintsTable"),
                              hr(),
                              p("Add constraints:"),
                              uiOutput("selectScenVar"),
                              uiOutput("clampSlider"),
                              actionButton("addFCMConstraint", "Add/ modify constraint"),
                              actionButton("deleteFCMConstraint", "Delete constraint"),
                              actionButton("clearAllFCMConstraints", "Clear all constraints")
                     )
                 )
               ), #sidebarPanel
               mainPanel(
                 tabsetPanel(type = "tabs",
                            tabPanel("Run Results",
                                     br(),
                                     h4("FCM Run results"),
                                     fluidRow(
                                       column(4, p("Run single simulation using parameters selected"),
                                              actionButton("runFCMAction", "Run simulation")),
                                       column(5, conditionalPanel(condition = "input.runFCMAction",
                                        textInput("scenarioName", label="Save this scenario", value="scenario-label"),
                                        actionButton("addScenario", "Add to scenario comparisons")))
                                              
                                     ),
                                     hr(),
                                     # p("The logistic sigmoid inference squashing function takes the form of 
                                     #   $f(x;\lambda,h) = \frac{1}{1+e^{-\lambda(x-h)}}$. This means that h controls the location 
                                     #   of the threshold, and lambda is an indication of the steepness of the sigmoid.The 'req' inference takes the minimum of all influences to feed in the 
                                     # squashing function ($x$ above), while the 'add' inference takes in the mean of the influences."),
                                     p("Note: In the current implementation of the model, k's are automatically set to 1 for all concepts
                                       that do not have any incoming influences."),
                                     hr(),
                                     fluidRow(
                                       column(3, p("Parameters used:")), column(9, tableOutput("paramsTable")),
                                     ),
                                     p("Simulation results:"),
                                     plotlyOutput(outputId = "resultsPlotSim"),
                                     DT::dataTableOutput("resultsTable")
                                     ), #tabPanel: Run Results
                            tabPanel("Configure Multiple Runs",
                                     br(),
                                     h4("Parameter Sweep"),
                                      p("Launch multiple runs (parameter sweep will override selected value(s) in the sidebar)"),
                                      uiOutput("sweepParam"),
                                      textInput("sweepingVals", "Values", placeholder = "Enter values separated by a comma...", value="0.5, 1, 3, 5"),
                                      textOutput("sweepText"),
                                      br(),
                                      fluidRow(
                                        column(6,actionButton("runFCMSweepAction", "Run parameter sweep")),
                                        column(6, actionButton("addSweep", "Add runs to scenario comparison view"))
                                      ),
                                      hr(),
                                      DT::dataTableOutput("sweepEquilTable"),
                                      hr(),
                                      fluidRow(
                                        plotlyOutput(outputId = "sweepPlot")
                                      ),
                                      br(),
                                      fluidRow(
                                        plotlyOutput(outputId = "sweepPlotBars")
                                      )
                          ), # tabPanel: Configure multiple runs
                         tabPanel("Configure Multiple Constraints",
                                  br(),
                                  h4("Set multiple constrained runs"),
                                  p("Launch multiple runs, each with a constraint added (choices override any constraints set in the sidebar)"),
                                  uiOutput("conceptsForScenarios"),
                                  actionButton("runFCMMultipleConstraints", "Launch runs and save to scenario list for comparison")
                         ), # tabPanel: Parameter Sweep
                          tabPanel("Compare Scenarios",
                                   br(),
                                   h4("Scenario comparison views"),
                                   fluidRow(
                                     column(7, uiOutput("scenariosToPlot")),
                                     column(5, actionButton("resetScenarios", "Reset scenario list"),hr(),
                                            fileInput("scenFileToLoad", label="Load a saved set of scenarios"), accept=".Rmd")
                                   ),
                                   actionButton("launchScenarioView", "Launch scenario comparison view"),
                                   hr(),
                                   br(),
                                   uiOutput('selectScenarioYVar'),
                                   br(),
                                   conditionalPanel(condition="input.scenarioPlotY != 'value'",
                                                    span( textOutput('scenarioPlotWarning'), style="color:red")),
                                   plotlyOutput(outputId = "scenarioPlot", height = "650px"),
                                   hr(),
                                   plotlyOutput(outputId = "scenarioPlotBars", height = "650px"),
                                   hr(),
                                   fluidRow(
                                     column(6, textInput("scenFileName", label="File Name", value="saved_scenarios.Rmd"),
                                            actionButton("saveScenarios", "Save these scenarios for later")),
                                     column(6,textInput("scenDataFileName", label="File Name", value="scenario_comparison_data.Rmd"),
                                            actionButton("saveScenarioData", "Save the comparison results as a data frame"))
                                   ), br()
                           ) # tabPanel: Compare Scenarios
                 ) # tabsetPanel
               ) # mainPanel
             ) # sidebarLayout
    ) # tab (2 - fcm)
  )) # shinyUI, navbarPage
