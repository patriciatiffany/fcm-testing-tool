#helper.R
# Author: Patricia Angkiriwang, University of British Columbia - 2019-2021 
# with code initially adapted from FSDM, an open-source R Shiny app by Brian Gregor, Oregon Systems Analytics LLC

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
               h4("Credits"),
               p("This application was loosely built based on The Logic Laboratory (FSDM), an open-source R Shiny application developed by Brian Gregor (Oregon Systems Analytics), 2016.")
             )
    ),
    # User Info -------------------
    tabPanel("0) Enter User Info",
             mainPanel(
               h4("User Information"),
               hr(),
               p("The user information entered below is used to attribute model creation and editing."),
               textInput("firstName", "First Name"),
               bsTooltip(id = "firstName", placement = "right", title = "Enter your first name"),
               textInput("lastName", "Last Name"),
               bsTooltip(id = "lastName", placement = "right", title = "Enter your last name"),
               textInput("organization", "Organization (optional)"),
               bsTooltip(id = "organization", placement = "right", title = "Enter your affiliation"),
               checkboxInput("anonymous", "Keep me anonymous", FALSE),
               bsTooltip(id = "anonymous", placement = "right", title = 'This uses "Anonymous" in lieu of your name and organization'),
             )
    ),
    tabPanel( "1) Model Setup",
              navbarPage("Set up your model",
              # Model upload -------------------
                         tabPanel( "Upload model",
                                   sidebarLayout(
                                     sidebarPanel(
                                       radioButtons(
                                         inputId = "modelAction", 
                                         label = "Model Action",
                                         choices = list("Select an existing model" = "select_existing",
                                                        "Create a new model" = "create_new"),
                                         selected = "select_existing"
                                       ),
                                       conditionalPanel(
                                         condition = "input.modelAction == 'select_existing'",
                                         uiOutput("selectExistingModelFile"),
                                         bsTooltip(id = "selectExistingModelFile", title = "These are the models that exist in the /models folder")
                                         
                                       ),
                                       conditionalPanel(
                                         condition = "input.modelAction == 'create_new'",
                                         textInput("newModelFileName", "Model Name", ""),
                                         bsTooltip(id = "newModelFileName", title = "This will be name of the folder in which your new model will be stored. Pick a unique name.")
                                       ),
                                       actionButton("startModeling", "Start Working on Model"),
                                       bsTooltip(id = "startModeling", placement = "right", title = "Click here to initialize the model"),
                                       bsAlert("noModelName"),
                                       bsAlert("duplicateModelName")
                                     ),
                                     mainPanel(
                                       tabsetPanel(
                                         tabPanel("Concepts", hr(),DT::dataTableOutput("conceptsTable"), value = "concepts_table"),
                                         tabPanel("Relations", hr(), DT::dataTableOutput("relationsTable"), value = "relations_table"),
                                         tabPanel("Model save history", hr(), verbatimTextOutput(outputId = "modelStatusDisplay"))),
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
                                      bsTooltip(id = "conceptName", placement = "right", title = "What is this concept?"),
                                      textInput("conceptID", "ID (one-word identifier)"),
                                      bsTooltip(id = "conceptID", placement = "right", title = "Enter a unique ID for this concept (A-Z, no spaces)"),
                                      textareaInput("conceptDesc", "Concept Description (optional)"),
                                      bsTooltip(id = "conceptDesc", placement = "right", title = "Add extra notes and clarifications here"),
                                      # textInput("minValue", "Minimum Value"),
                                      # textInput("maxValue", "Maximum Value"),
                                      # textareaInput("valuesDesc", "Values Description (optional)"),
                                      textInput("conceptCategory", "Category (optional)"),
                                      bsTooltip(id = "conceptCategory", placement = "right", title = "The category this concept belongs to"),
                                      conditionalPanel(
                                        condition = "input.modelAction != 'runModel'",
                                        wellPanel(
                                          actionButton("addConcept", "Add"),
                                          bsTooltip(id = "addConcept", title = "Adds as new concept"),
                                          actionButton("updateConcept", "Update"),
                                          bsTooltip(id = "updateConcept", title = "Updates the concept matching this ID (or, if entering a new ID, updates the highlighted concept)"),
                                          actionButton("deleteConcept", "Delete"),
                                          bsTooltip(id = "deleteConcept", title = "Deletes highlighted concept"),
                                          actionButton("undoConceptAction", "Undo"),
                                          bsTooltip(id = "undoConceptAction", title = "Toggles last action"),
                                          bsAlert(
                                            "duplicateConceptVariable"
                                          )
                                        ),
                                        actionButton("saveModel1","Save Model"),
                                        bsTooltip(id = "saveModel1", placement = "right", title = "Saves all edits to the /models folder (including any changes on other tabs)")
                                      ) #end: conditionalPanel
                                    ),
                                    mainPanel(
                                      tabPanel("Concepts", DT::dataTableOutput("conceptsTableEditing"), value = "table"),
                                      br(),
                                      p("Note: Highlight/ select concepts by clicking the rows in the table")
                                    )
                                  )
                                  
                         ), # tabPanel: edit concepts
                         tabPanel("Edit Relationships",
                                  sidebarLayout(
                                    sidebarPanel(
                                      h4("Edit Relationships"),
                                      hr(),
                                      uiOutput("selectCausalConcept"),
                                      bsTooltip(id = "selectCausalConcept", placement = "right", title = 'Concept causing the influence'),
                                      uiOutput("selectAffectedConcept"),
                                      bsTooltip(id = "selectAffectedConcept", placement = "right", title = 'Concept receiving the influence'),
                                      selectInput(inputId = "causalDirection", 
                                                  label = "Direction", 
                                                  choices = c("" ,"Positive", "Negative")),
                                      bsTooltip(id = "causalDirection", placement = "right", title = "Is this a positive (+) or negative (-) influence?"),
                                      selectInput(inputId = "causalStrength", 
                                                  label = "Strength (Weight)", 
                                                  choices = c("", "VL", "L", "ML", "M", "MH", "H", "VH")),
                                      bsTooltip(id = "causalStrength", placement = "right", title = 'What is the strength of this influence? ("Very Low" to "Very High")'),
                                      textareaInput("causalDesc", "Description"),
                                      bsTooltip(id = "causalDesc", placement = "right", title = "Add extra notes and clarifications here"),
                                      hr(), 
                                      p("Properties concerning the affected concept:"),
                                      br(),
                                      sliderInput(inputId = "relK", label = "Inertia of affected concept (k)",
                                                  min=0, max=1, step = 0.1, value = 1),
                                      bsTooltip(id = "relK", placement = "right", title = "To what extent is the affected concept influenced by its previous value?"),
                                      selectInput(inputId = "relType", label = "Relational aggregation (Type)",
                                                  choices = c("ADD"="add","REQ"="req")),
                                      bsTooltip(id = "relType", title = "How should the influences affecting this concept be aggregated? Are they additive (ADD)? Are they all required to cause an effect (REQ)?<br><br>Note that changing this will modify the type of all relationships influencing the affected concept"),
                                      # - The following references to relation "groups" are commented out, as multiple aggregation types (groups) per affected concept are still unsupported
                                      # bsTooltip(id = "relType", title = "How should the influences affecting this concept be aggregated? Are they additive (ADD)? Are they all required to cause an effect (REQ)?<br><br>Note that changing this will change all relationships in the same group"),
                                      # numericInput(inputId = "relGrouping", label = "[ Relation group number ]", value = 1),
                                      # bsTooltip(id = "relGrouping", title = "(Auto-completes) An ID assigned to each group of influences affecting this concept, each with a common relation type"),
                                      conditionalPanel(
                                        condition = "input.modelAction != 'runModel'",
                                        wellPanel(
                                          #actionButton("addRelation", "New"), #inactive - currently, all assumed to have something
                                          actionButton("updateRelation", "Add/ Update"),
                                          bsTooltip(id = "updateRelation", title = "Updates properties corresponding to this relationship, and adds the relation if it does not yet exist"),
                                          actionButton("deleteRelation", "Delete"),
                                          bsTooltip(id = "deleteRelation", title = "Removes this relationship"),
                                          actionButton("undoRelationAction", "Undo"),
                                          bsTooltip(id = "undoRelationAction", title = "Toggles last action")
                                        ), # end: wellPanel
                                        actionButton("saveModel2","Save Model"),
                                        bsTooltip(id = "saveModel2", placement = "right", title = "Saves all edits to the /models folder (including any changes on other tabs)")
                                      ) #end: conditionalPanel
                                    ), # sidebarPanel
                                    mainPanel(
                                      tabPanel("Relations", DT::dataTableOutput("relationsTableEditing"), value = "relations_edit"),
                                      br(),
                                      p("Note: Highlight/ select relationships by clicking the rows in the table")
                                    ) #mainPanel
                                  ) # sidebarLayout
                         ), # tabPanel: edit relationships
                         tabPanel("Edit Weight Values",
                           sidebarLayout(
                             sidebarPanel(
                               h4("Edit numerical values for edge weights"),
                               hr(),
                               selectInput(inputId = "qualWeight", 
                                           label = "Qualitative Strength of Influence", 
                                           choices = c("", "VL", "L", "ML", "M", "MH", "H", "VH"),
                                           selected = "M"),
                               bsTooltip(id = "qualWeight", placement = "right", title = "Select the weight to modify"),
                               sliderInput(inputId = "quantWeight",
                                           label = "Numerical value",
                                           min=0, max=1, step = 0.05, value = 1),
                               bsTooltip(id = "quantWeight", placement = "right", title = "What numerical value should this strength correspond to?"),
                               actionButton("updateWeight", "Update"),
                               bsTooltip(id = "updateWeight", placement = "right", title = "Update weight pairing"),
                               hr(),
                               actionButton("saveModel3","Save Model"),
                               bsTooltip(id = "saveModel3", placement = "right", title = "Saves all edits to the /models folder (including any changes on other tabs)")
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
                 uiOutput("initSlider"),
                 hr(),
                 tabsetPanel(type = "tabs",
                     tabPanel("Set Parameters",
                              hr(),
                              selectInput("selectFCM_fn", label = "FCM thresholding function",
                                          choices = c("Sigmoid (Logistic)"="sigmoid-exp","Sigmoid (Tanh)"="sigmoid-tanh","Linear (f(x) = x-h)"="linear"), selected="sigmoid-exp"),
                              plotOutput("FCMFunction", height = "250px"),
                              sliderInput("sliderFCM_h", "Choose h", min=-1, max=1, step = 0.25, value = 0), 
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
                              actionButton("clearAllFCMConstraints", "Clear all constraints"),
                              hr(),
                              p("Note: These constraints will override any initial values.")
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
                         ) # tabPanel: Parameter Sweep
                 ) # tabsetPanel
               ) # mainPanel
             ) # sidebarLayout
    ), # tab (2 - fcm)
    tabPanel("3) Compare Scenario Results",
             mainPanel(
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
               plotOutput(outputId = "scenarioPlot", height = "650px"), #plotlyOutput
               hr(),
               plotOutput(outputId = "scenarioPlotBars", height = "650px"),
               hr(),
               plotOutput(outputId = "scenarioPlotSlope", height = "650px"),
               hr(),
               fluidRow(
                 column(6, textInput("scenFileName", label="File Name", value="saved_scenarios"),
                        actionButton("saveScenarios", "Save these scenarios for later")),
                 column(6,textInput("scenDataFileName", label="File Name", value="scenario_comparison_data"),
                        actionButton("saveScenarioData", "Save the comparison results as a data frame"))
               ), br()
        ) # mainPanel
    ) # tab: 3 - Compare Scenarios
  )) # shinyUI, navbarPage
