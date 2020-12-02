#server.R
#Author: Patricia Angkiriwang, University of British Columbia; based on code by Brian Gregor, Oregon Systems Analytics LLC
#Copyright: 2016, Oregon Department of Transportation 2016
#Modifications copyright: 2019, Patricia Angkiriwang
#License: Apache 2

# ORIGINAL DATA FORMAT:FOR MODELS
# A model (named Model_ls) is a list of 2
# concepts: a data frame with columns - name, variable, description, values, group (all chr)
# relations: a list of _, each a list of 2
#   name: (chr - name of causal variable)
#   affects: (list of[#things it affects]), each a list of 4: variable, direction, weight, description

# TO CHANGE POTENTIALLY/ CHECK MARKED WITH "BEEP"

# 2019/07/24 - getting rid of "conceptstable" and "relationslist" reactive values - switch to model$concepts and model$relations

#LOAD RESOURCES
#--------------
#Packages
library(shiny)
library(shinyBS)
library(jsonlite)
library(DT)
library(ggplot2)
library(DiagrammeR)
library(readxl)
##Helper.R script
source("helper.R") # <- Still need to check helper.R


#SHINY SERVER FUNCTION
#---------------------
shinyServer(function(input, output, session) {
  
  #------------------------------------------------
  #CREATE OBJECTS TO STORE MODEL (AND SCENARIO STATE)
  #------------------------------------------------
  #Reactive object to store current state that interface responds to
  model <- reactiveValues(status = NULL, concepts = NULL, relations = NULL)
  #Reactive object to store model history (unlimited undo)
  history <- reactiveValues(status = NULL, concepts = NULL, relations = NULL)#,  previousRowNum = NULL) #note: attempts to keep rows from resetting have failed 2019/05/22
  #Global variable that flags whenever a new relationship is defined
  flag_newRelation <- FALSE
  #Create a reactive object to store current selected effects
  effects <- 
    reactiveValues(
      to = "",
      from = "",
      direction = "",
      strength = "",
      description = "")
  # #Create a reactive object to store scenario data in
  # scenario <- 
  #   reactiveValues(status = NULL, values = NULL, history = NULL)
  # #Create a reactive object to represent scenario table
  # scenariotable <- reactiveValues(values = NULL)
  # #Create a reactive object to store names of scenarios
  # scenariolist <- reactiveValues(valid = "", invalid = "", run = "", all = "")
  #Create a reactive object to keep track of various conditions
  is <- reactiveValues(newconcept = FALSE)

  #-----------------------------------------------------
  #DEFINE COMMON FUNCTIONS FOR MODIFYING REACTIVE VALUES
  #-----------------------------------------------------
  #Function to save the model in history
  saveLastState <- function() {
    history$status <- model$status
    history$concepts <- model$concepts
    history$relations <- model$relations
  }
  #Function to swap model and history (i.e. undo)
  swapState <- function() {
    Status <- model$status
    Concepts <- model$concepts
    Relations <- model$relations
    model$status <- history$status
    model$concepts <- history$concepts
    model$relations <- history$relations
    history$status <- Status
    history$concepts <- Concepts
    history$relations <- Relations
  }
  #Function to undo concept edit
  undoConceptEdit <- function() {
    swapState()
  }
  #Function to undo relation edit
  undoRelationEdit <- function() {
    swapState()
  }
  # #Function to save last scenario state
  # saveLastScenarioState <- function() {
  #   scenario$history <- scenario$values
  # }
  # #Function to swap scenario history and present values
  # swapScenarioState <- function() {
  #   Values <- scenario$values
  #   scenario$values <- scenario$history
  #   scenario$history <- Values
  # }
  # #Function to undo scenario edit
  # undoScenarioEdit <- function() {
  #   swapScenarioState()
  # }
  # #Function to update scenario table with scenario values
  # updateScenarioTable <- function() {
  #   scenariotable$values <- scenario$values
  # }
  #Function to update concept form inputs
  updateConceptForm <- function(RowNum) {
    updateTextInput(session, "conceptName",
                    value = model$concepts$concept[RowNum])
    updateTextInput(session, "conceptID",
                    value = model$concepts$concept_id[RowNum])
    updateTextInput(session, "conceptDesc",
                    value = model$concepts$description[RowNum])
    updateTextInput(session, "minValue",
                    value = model$concepts$values$min[RowNum])
    updateTextInput(session, "maxValue",
                    value = model$concepts$values$max[RowNum])
    updateTextInput(session, "valuesDesc",
                    value = model$concepts$values$description[RowNum])
    updateTextInput(session, "conceptGroup",
                    value = model$concepts$group[RowNum])
  }
  
  #Function to update relation form causal and affected variables -- the rest updates based on other observes
  updateRelationForm <- function(RowNum) { 
    causal_vars <- extract_rel(model$relations, "concept_id")
    affects_vars <- extract_rel(model$relations, "concept_id", level="affected")
    updateSelectInput(session, inputId = "causalConcept",
                      selected = causal_vars[RowNum]
    )
    updateSelectInput(session, inputId = "affectedConcept",
                      selected = affects_vars[RowNum]
    )
  }
  
  # #Function to update scenario concept form inputs
  # updateScenarioForm <- function(RowNum) {
  #   output$scenarioConcept <- renderText({scenariotable$values$name[RowNum]})
  #   updateTextInput(session, "conceptStartValue",
  #                   value = scenariotable$values$startvalue[RowNum])
  #   updateTextInput(session, "conceptStartChange",
  #                   value = scenariotable$values$startchange[RowNum])
  #   updateTextInput(session, "conceptValuesDescription",
  #                   value = scenariotable$values$description[RowNum])
  # }
  #Function to clear reactive data when when changing model
  resetModel <- function(){
    history$status = NULL
    history$concepts = NULL
    history$relations = NULL
    model$concepts = NULL
    model$relations = NULL
    model$status = NULL
    effects$to <- ""
    effects$from <- ""
    effects$direction <- ""
    effects$strength <- ""
    effects$description <- ""
    # scenario$status <- NULL
    # scenario$values <- NULL
    # scenario$history <- NULL
    # scenariotable$values <- NULL
    # scenariolist$valid <- ""
    # scenariolist$invalid <- ""
    # scenariolist$all <- ""
    # scenariolist$run <- ""
  }
  
  #--------------------------------------
  #IMPLEMENT INTERFACE FOR STARTING MODEL
  #--------------------------------------
  #Define GUI element to select model from a list
  output$selectExistingModelFile <- renderUI({
    selectInput(
      inputId = "modelFileName",
      selected = NULL,
      label = NULL,
      choices = dir(path = "./models")[dir(path = "./models") != "templates"]
    )
  })
  
  #Define author name
  ModelAuthor <- reactive({
    if (input$anonymous){
      author <- "Anonymous"
    } else{
      author <- ifelse(input$organization=="", paste0(input$firstName, " ", input$lastName), paste0(input$firstName, " ", input$lastName, " (", input$organization, ")"))
    }
    return(author)
  })
  
  #Choose model start option and initialize model
  observeEvent(
    input$startModeling,
    {
      if (input$modelAction == "select_xl") {
        #Check that a model name exists and is not a duplicate
        ExistingModels_ <- dir("./models")
        if (input$modelName == ""){ #Check that there is a model name
          createAlert(session = session, anchorId = "nonameAlert", alertId = "noname",
                      title = "Missing Name", 
                      content = "Model name is missing. Enter a name above.")
          return()
        } else if (tolower(input$modelName) %in% tolower(ExistingModels_)) { #Check that model name does not duplicate an existing model name
          createAlert(session = session, anchorId = "duplicateAlert", alertId = "duplicate",
                      title = "Duplicate Model",
                      content = "Model name is same as existing model name. Enter a different name.")
          return()
        }
        
        if (is.null(input$xlFileName)){
          createAlert(session = session, anchorId = "nofileAlert", alertId = "nofile",
                      title = "Missing File", 
                      content = "Please upload an Excel file.")
          return()
        }
        
        # Do some stuff here that initializes the model (copies and saves template)
        resetModel()
        model$status <- initializeNewModel(input$modelName,ModelAuthor()) # function from helper.R
        
        # Read in excel files
        xl_fn <- input$xlFileName$datapath
        xl_concepts<- readxl::read_excel(xl_fn, sheet=1,col_names = input$uploadheader)
        xl_relations <- readxl::read_excel(xl_fn, sheet=2,col_names = input$uploadheader)
        
        # Reconfigure into new dataframes, rename columns to match legacy code (mostly)
        # changed 2019/07/18 ("concept" instead of "name", "id" instead of "variable")
        concepts <- data.frame(concept=xl_concepts[[2]],id=xl_concepts[[1]],description=NA,values=NA,group=NA)
        
        # Configure so that values is a data frame nested within the concept dataframe
        concepts_template <- loadModelConcepts("templates")
        v <- concepts_template$values
        concepts$values <- v[rep(1,each=dim(concepts)[1]),]
        concepts$values[["min"]] <- 0 
        concepts$values[["max"]] <- 100
        concepts$values[["description"]] <- NA
        rownames(concepts$values) <- c() # remove any row names that might exist
        
        # Add in optional inputs, if they exist
        if (length(xl_concepts)>=3){
          concepts$group <- xl_concepts[[3]]
          if (length(xl_concepts)>=4){
            concepts$description <- xl_concepts[[4]]
            if(length(xl_concepts)>=5){
              concepts$values[["min"]] <- xl_concepts[[5]]
              if(length(xl_concepts)>=6){
                concepts$values[["max"]] <- xl_concepts[[6]]
              }
            }
          }
        }
        rownames(concepts) <- c() # remove any row names that might exist
        #View(concepts)
        
        relations_df <- data.frame(from=xl_relations[[1]],to=xl_relations[[2]],description=NA,direction=xl_relations[[3]],weight=xl_relations[[4]],label=NA) # (label column unused) BEEP
        # Add in optional inputs, if they exist
        if(length(xl_relations)>=5){
          relations_df$description <- xl_relations[[5]]
          if(length(xl_relations)>=6){
            relations_df$label <- xl_relations[[6]]
          }
        }
        # From initial data frame created, split the data twice into nested lists. Drop the name columns, then rename (again, to match legacy code)
        relations <- lapply(split(relations_df,relations_df$from), # first split the data frame with "from" column
                            function(x){
                              x_ <- setNames(split(x,seq(nrow(x))),x$to) # then split again with the "to" column
                              l <- list(lapply(x_,function(r){
                                as.list(subset(r,select=-c(from,to)))} # needs to be a list or toJSON won't write it correctly
                                ))
                              names(l) <- "affects"
                              return(l)
                              })
        
        addname <- function(n,list,namevar="name"){
          list[[n]][[namevar]] <- n
          list[[n]]}
        
        # Add names to each data frame in nested list  
        relations <- lapply(relations,function(x){
          l <- list(sapply(names(x$affects),addname,x$affects,"concept_id",simplify=FALSE)) #USE.NAMES =TRUE,
          names(l) <- "affects" # name again (gets erased in previous step)
          l})
        # Add names to nested lists
        relations <- sapply(names(relations),addname,relations,"concept_id",USE.NAMES =TRUE,simplify=FALSE)
        
        model$concepts <- concepts
        model$relations <- relations
        #View(relations)
        saveModel(model)
        
        startmessage <- "Model loaded from Excel file and saved in /models folder"
        
      } else if(input$modelAction == "select_existing") { 
        model$status <- loadModelStatus(input$modelFileName)
        model$concepts <- loadModelConcepts(input$modelFileName)
        model$relations <- loadModelRelations(input$modelFileName) 
        #View(model$relations)
        
        startmessage <- "Model loaded from /models folder"
        
      } # end if input$modelAction (select_xl or select_existing)
      closeAlert(session,alertId = "nofile")
      closeAlert(session,alertId = "noname")
      closeAlert(session,alertId = "duplicate")
      
      showNotification(
        ui = startmessage,
        duration = 2, 
        closeButton = TRUE,
        type = "message"
      )
      
      updateConceptForm(1)
      updateRelationForm(1)
      saveLastState()
      # scenariolist$runs <- listScenarios(model$status$name)$runs
      # clearScenarioForm()
      
    }) # end: observeEvent - input$startModeling
  
  
  
  #----------------------------------------------
  #IMPLEMENT INTERFACE FOR EDITING MODEL CONCEPTS
  #----------------------------------------------
  #Update concept form based on what is selected in table
  observeEvent(
    c(input$conceptsTableEditing_rows_selected,input$undoConceptAction), # fixed broken undo by adding additional argument here 2019/05/22
    {
      if(!is.null(input$conceptsTableEditing_rows_selected)){
        RowNum <- input$conceptsTableEditing_rows_selected
        updateConceptForm(RowNum)
      }
    }
  )
  #Implement the new concept button
  observeEvent(
    input$addConcept,
    {
      is$newconcept <- TRUE
      model$concepts <- model$concepts[c(1,1:nrow(model$concepts)),]
      model$concepts$concept[1] <- ""
      model$concepts$concept_id[1] <- ""
      model$concepts$description[1] <- ""
      model$concepts$values$min[1] <- ""
      model$concepts$values$max[1] <- ""
      model$concepts$values$description[1] <- ""
      model$concepts$group[1] <- ""
      RowNum <- input$conceptsTableEditing_rows_selected
      updateConceptForm(RowNum)
    }
  )
  #Implement the update concept button
  observeEvent(
    input$updateConcept,
    {
      #Check whether if new concept and duplicate name or variable
      IsDupName <- input$conceptName %in% model$concepts$concept
      IsDupVar <- input$conceptID %in% model$concepts$concept_id
      if (is$newconcept & IsDupName) {
        createAlert(session = session, anchorId = "duplicateConceptName", 
                    title = "Duplicate Concept Name", 
                    content = "New concept name is the same as name for an existing concept. Rename before updating.")
        return()        
      }
      if (is$newconcept & IsDupVar) {
        createAlert(session = session, anchorId = "duplicateConceptVariable", 
                    title = "Duplicate Concept Nickname", 
                    content = "New concept nickname is the same as nickname for an existing concept. Rename before updating.")
        return()        
      }
      #Save state of current model
      saveLastState()
      #Update model concepts
      RowNum <- input$conceptsTableEditing_rows_selected
      model$concepts$concept[RowNum] <- input$conceptName
      model$concepts$concept_id[RowNum] <- input$conceptID
      model$concepts$description[RowNum] <- input$conceptDesc
      model$concepts$values$min[RowNum] <- input$minValue
      model$concepts$values$max[RowNum] <- input$maxValue
      model$concepts$values$description[RowNum] <- input$valuesDesc
      model$concepts$group[RowNum] <- input$conceptGroup
      model$status$lastedit <- as.character(Sys.time())
      #Reset Concept$IsNew
      is$newconcept <- FALSE
      #Initialize model relations for new concept
      AnyRelations <- length(model$relations) != 0
      if (AnyRelations) {
        ExistingRelations_ <- unlist(lapply(model$relations, function(x) x$concept_id))
        if (!(input$conceptID %in% ExistingRelations_)) {
          model$relations[[length(model$relations) + 1]] <- 
            list(name = input$conceptID, affects = list())
        }
      } else {
        model$relations[[1]] <- list(name = input$conceptID, affects = list())
      }
      
      showNotification(
        ui = "Concept updated",
        duration = 1, 
        closeButton = TRUE,
        type = "message"
      )
    }
  )
  #Implement the undo button
  observeEvent(
    input$undoConceptAction,
    {
      undoConceptEdit()
      model$status$lastedit <- as.character(Sys.time())
    }
  )
  #Implement the delete concept button
  observeEvent(
    input$deleteConcept,
    {
      #Save last model state in redobuffer
      saveLastState()
      #Modify/ update model concepts
      RowNum <- input$conceptsTableEditing_rows_selected
      Var <- model$concepts$concept_id[RowNum]
      model$concepts <- model$concepts[-RowNum,]
      model$status$lastedit <- as.character(Sys.time())

      #Update model relations
      Relations_ls <- model$relations
      RelationsNames_ <- 
        unlist(lapply(Relations_ls, function(x) x$concept_id))
      RelationIdx <- which(RelationsNames_ == Var)
      Relations_ls[[RelationIdx]] <- NULL
      for (i in 1:length(Relations_ls)) {
        Affects_ls <- Relations_ls[[i]]$affects
        IsVar <- unlist(lapply(Affects_ls, function(x) x$concept_id == Var))
        if (any(IsVar)) {
          Affects_ls[[which(IsVar)]] <- NULL
        }
        Relations_ls[[i]]$affects <- Affects_ls
      }
      model$relations <- Relations_ls
      #Update the input form
      updateConceptForm(RowNum)
    }) #observeEvent: deleteConcept
  
  
  observeEvent(
    c(input$saveModel1,input$saveModel2),
    {
      if (!is.null(model$status$name)){
        saveModel(model)
        showNotification(
          ui = "All updates have been saved in the /models folder",
          duration = 2, 
          closeButton = TRUE,
          type = "message"
        )
      }
    })
  
  
  
  #-----------------------------------------------
  #IMPLEMENT INTERFACE FOR EDITING MODEL RELATIONS
  #-----------------------------------------------
  #Update relations form based on what is selected in table
  observeEvent(
    c(input$relationsTableEditing_rows_selected,input$undoRelationAction),
    {
      if(!is.null(input$relationsTableEditing_rows_selected) || flag_newRelation==FALSE){
        RowNum <- input$relationsTableEditing_rows_selected
        updateRelationForm(RowNum)
      } 
    }
  )
  
  #Define dropdown element to select causal group (for graph display)
  output$selectCausalGroup <- renderUI({
    selectInput(
      inputId = "causalGroup",
      label = "Causal Group",
      choices = c("All", model$concepts$group)
    )
  })
  #Define dropdown element to select affected group (for graph display)
  output$selectAffectedGroup <- renderUI({
    selectInput(
      inputId = "affectedGroup",
      label = "Affected Group",
      choices = c("All", model$concepts$group)
    )
  })
  #Define dropdown element to select causal concept from a list (for editing)
  output$selectCausalConcept <- renderUI({
    selectInput(
      inputId = "causalConcept",
      label = "Causal Concept",
      choices = sort(model$concepts$concept_id)
      #choices = sort(model$concepts$name)
    )
  })
  #Define dropdown element to select affected concept from a list (for editing)
  output$selectAffectedConcept <- renderUI({
    selectInput(
      inputId = "affectedConcept",
      label = "Affected Concept",
      choices = sort(model$concepts$concept_id)
      #choices = sort(model$concepts$name)
    )
  })
  # Create reactive value that contains a formatted relations table, not for display but for use below
  relationstable <- reactive({
    # Note: right now the only difference (between this and the table that is displayed in the GUI 
    # is that full names are not used here. If eventually full names are used in the editing relations dropdowns
    # then this can be consolidated and used for the GUI too.
    if (!is.null(model$relations)){
      formatRelationTable(model$relations,model$concepts,use.full.names=FALSE,export=TRUE)
    }
  })

  
  #On change of selected causal concept, update causal info in GUI
  observeEvent(
    c(input$causalConcept,input$undoRelationAction),
    {
      # Deselect rows in relations table
      relationsTableEditing_proxy %>% selectRows(NULL)
      
      if (input$causalConcept != "" && length(input$causalConcept)>0){
         Effects_df <- relationstable()[relationstable()$From==input$causalConcept,]
        if (nrow(Effects_df)>0){#(!is.null(Effects_df)) {
          effects$to <- Effects_df$To #Effects_df$Affects
          effects$from <- Effects_df$From
          effects$direction <- Effects_df$Direction
          effects$strength <- Effects_df$Weight
          effects$description <- Effects_df$Description
          #in following if, change effects$from to effects$to
          if (input$affectedConcept %in% effects$to) {
            updateTextInput(session, "causalDirection",
                            value = effects$direction[effects$to == input$affectedConcept])
            updateTextInput(session, "causalStrength",
                            value = effects$strength[effects$to == input$affectedConcept])
            updateTextInput(session, "causalDesc",
                            value = effects$description[effects$to == input$affectedConcept])
            # and change the selected row in the table to match
            r <- which(relationstable()$From==input$causalConcept & relationstable()$To == input$affectedConcept)
            relationsTableEditing_proxy %>% selectRows(as.numeric(r))
          } else {
            updateTextInput(session, "causalDirection", value = "")
            updateTextInput(session, "causalStrength", value = "")
            updateTextInput(session, "causalDesc", value = "")
          }
        } else {
          effects$to <- ""
          effects$concept <- ""
          effects$direction <- ""
          effects$strength <- ""
          effects$description <- ""
          updateTextInput(session, "causalDirection", value = "")
          updateTextInput(session, "causalStrength", value = "")
          updateTextInput(session, "causalDesc", value = "")
        }
      }
    }
  )
  #On change of selected affected concept, update causal info in GUI
  observeEvent(
    input$affectedConcept,
    {
      #in following if, change effects$from to effects$to 
      if (input$affectedConcept %in% effects$to) {
        flag_newRelation <<- FALSE
        updateTextInput(session, "causalDirection",
                        value = effects$direction[effects$to == input$affectedConcept])
        updateTextInput(session, "causalStrength",
                        value = effects$strength[effects$to == input$affectedConcept])
        updateTextInput(session, "causalDesc",
                        value = effects$description[effects$to == input$affectedConcept])
        # and change the selected row in the table to match
        r <- which(relationstable()$From==input$causalConcept & relationstable()$To == input$affectedConcept)
        relationsTableEditing_proxy %>% selectRows(as.numeric(r))
      } else {
        flag_newRelation <<- TRUE
        updateTextInput(session, "causalDirection", value = "")
        updateTextInput(session, "causalStrength", value = "")
        updateTextInput(session, "causalDesc", value = "")   
        relationsTableEditing_proxy %>% selectRows(NULL) # deselect rows in table
      }
    }
  )
  #Implement the update relations button
  observeEvent(
    input$updateRelation,
    {
      #Save last model state in redobuffer
      saveLastState()
      
      #Update Relation
      #change concepts$name to concepts$concept_id
      CausalConcept <- 
        model$concepts$concept_id[model$concepts$concept_id == input$causalConcept]
      CausalConcepts_ <-
        unlist(lapply(model$relations, function(x) x$concept_id))
      CausalIdx <- which(CausalConcepts_ == CausalConcept)
      AffectedConcept <- 
        model$concepts$concept_id[model$concepts$concept_id == input$affectedConcept]
      NewEffect_ls <-
        list(concept_id = AffectedConcept,
             direction = input$causalDirection,
             weight = input$causalStrength,
             description = input$causalDesc)
      Effects_ls <- 
        model$relations[[CausalIdx]]$affects
      if (length(Effects_ls) != 0) {
        AffectedConcepts_ <- unlist(lapply(Effects_ls, function(x) x$concept_id))
        if (AffectedConcept %in% AffectedConcepts_) {
          Effects_ls[[which(AffectedConcepts_ == AffectedConcept)]] <-
            NewEffect_ls
        } else {
          Effects_ls[[length(Effects_ls) + 1]] <- NewEffect_ls
        }
        flag_newRelation <<- FALSE
      } else {
        Effects_ls[[1]] <- NewEffect_ls
      }
      model$relations[[CausalIdx]]$affects <- Effects_ls
      showNotification(
        ui = "Relationship updated",
        duration = 1, 
        closeButton = TRUE,
        type = "message"
      )
      model$status$lastedit <- as.character(Sys.time())
    }
  )
  #Implement the delete relation
  observeEvent(
    input$deleteRelation,
    {
      #Save last model state and relations inputs
      saveLastState()
      #Remove relation from model
      #change concepts$name to concepts$concept_id
      CausalConcept <- 
        model$concepts$concept_id[model$concepts$concept_id == input$causalConcept]
      CausalConcepts_ <-
        unlist(lapply(model$relations, function(x) x$concept_id))
      CausalIdx <- which(CausalConcepts_ == CausalConcept)
      AffectedConcept <- 
        model$concepts$concept_id[model$concepts$concept_id == input$affectedConcept]
      Effects_ls <- 
        model$relations[[CausalIdx]]$affects
      EffectIdx <- 
        which(unlist(lapply(Effects_ls, function(x) x$concept_id)) == AffectedConcept)
      if (length(EffectIdx) != 0) {
        Effects_ls[[EffectIdx]] <- NULL
        model$relations[[CausalIdx]]$affects <- Effects_ls
      }
      #Update text fields
      updateTextInput(session, "causalDirection", value = "")
      updateTextInput(session, "causalStrength", value = "")
      updateTextInput(session, "causalDesc", value = "")
      model$status$lastedit <- as.character(Sys.time())
    }
  )
  #Undo relations edit
  observeEvent(
    input$undoRelationAction,
    {
      #change effects$from to effects$to
      undoRelationEdit()
      updateTextInput(session, "causalDirection",
                      value = effects$direction[effects$to == input$affectedConcept])
      updateTextInput(session, "causalStrength",
                      value = effects$strength[effects$to == input$affectedConcept])
      updateTextInput(session, "causalDesc",
                      value = effects$description[effects$to == input$affectedConcept])
      model$status$lastedit <- as.character(Sys.time())
    }
  )

  #-----------------------------------------------
  # OUTPUT TABLES FOR UI
  #-----------------------------------------------
  
  #Output model concepts table (for preview)
  output$conceptsTable <- DT::renderDataTable(
    formatConceptTable(model$concepts), 
    server = FALSE, 
    options = list(pageLength = 20)
  )
  #Output model concepts table FOR EDITING
  output$conceptsTableEditing <- DT::renderDataTable(
    formatConceptTable(model$concepts), 
    server = FALSE, 
    selection = list(mode = 'single', target = 'row', selected = 1),
    options = list(pageLength = 20)
  )
  
  #Output model relations table (for preview)
  #https://rstudio.github.io/DT/010-style.html
  output$relationsTable <- DT::renderDataTable(
    formatRelationTable(model$relations,model$concepts),  
    server = FALSE, 
    options = list(pageLength = 20)
  )
  
  #Output model relations table FOR EDITING
  output$relationsTableEditing <- DT::renderDataTable(
    formatRelationTable(model$relations,model$concepts), 
    server = FALSE, 
    selection = list(mode = 'single', target = 'row'),#, selected = history$previousRowNum),
    options = list(paging = FALSE)
  )
  
  relationsTableEditing_proxy <- dataTableProxy('relationsTableEditing') # for selecting rows

  #Implement relations graph
  output$relations_graph <- renderGrViz({
    if (length(model$concepts)>0){
      Dot_ <- makeDot(Relations_ls = model$relations,
                      Concepts_df = model$concepts,
                      RowGroup = "All",#input$causalGroup,
                      ColGroup = "All",#input$affectedGroup,
                      orientation = "Portrait",#Landscape",#input$graphOrientation,
                      rankdir = "Top-to-Bottom",#input$graphLayout,
                      shape = "box",#input$nodeShape,
                      Show = "Level")#input$edgeLabel)
      grViz(Dot_)
    }
  })
  
  
}) #end: shinyServer
