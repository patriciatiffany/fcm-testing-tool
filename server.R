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

# LOAD RESOURCES
#--------------#
# packages
library(shiny)
library(shinyBS)
library(plotly)
library(DT)
library(jsonlite)
library(ggplot2)
library(tidyr)
library(DiagrammeR)
library(readxl)
## helper.R Script
source("helper.R") 
## model algorithm
source('../../bcfn_seafood_access_modelling/fcm.R')


# Define global variables in server
weight_vals_default <- c(VL = 1, L = 1, ML = 1, M = 1, MH = 1, H = 1, VH = 1) # Use uniform weighting for now
#c(VL = 0.1, L = 0.25, ML = 0.375, M = 0.5, MH = 0.675, H = 0.75, VH = 0.95) 

#SHINY SERVER FUNCTION
#---------------------#
shinyServer(function(input, output, session) {
  
  #------------------------------------------------#
  #CREATE OBJECTS TO STORE MODEL (AND SCENARIO STATE)
  #------------------------------------------------#
  
  #Reactive object to store current state that interface responds to
  model <- reactiveValues(status = NULL, concepts = NULL, relations = NULL, weight_vals = weight_vals_default) # default defined in global.R
  #Reactive object to store model history (unlimited undo)
  history <- reactiveValues(status = NULL, concepts = NULL, relations = NULL, weight_vals = NULL)#,  previousRowNum = NULL) #note: attempts to keep rows from resetting have failed 2019/05/22
  #Global variable that flags whenever a new relationship is defined
  flag_newRelation <- FALSE
  #Create a reactive object to store current selected effects (what are the relations corresponding to the currently selected causal concept?)
  effects <- reactiveValues(
      to = "", # <- a vector
      from = "",
      direction = "",
      strength = "",
      description = "")
  
  run <- reactiveValues(results = NULL, parameters= NULL, constraints_list = NULL)

   # #Create a reactive object to store scenario data in
  # scenario <- 
  #   reactiveValues(status = NULL, values = NULL, history = NULL)
  # #Create a reactive object to represent scenario table
  # scenariotable <- reactiveValues(values = NULL)
  # #Create a reactive object to store names of scenarios
  # scenariolist <- reactiveValues(valid = "", invalid = "", run = "", all = "")
  #Create a reactive object to keep track of various conditions
  is <- reactiveValues(newconcept = FALSE)

  #-----------------------------------------------------#
  #DEFINE COMMON FUNCTIONS FOR MODIFYING REACTIVE VALUES
  #-----------------------------------------------------#
  #Function to save the model in history
  saveLastState <- function() {
    history$status <- model$status
    history$concepts <- model$concepts
    history$relations <- model$relations
    history$weight_vals <- model$weight_vals
  }
  #Function to swap model and history (i.e. undo)
  swapState <- function() {
    Status <- model$status
    Concepts <- model$concepts
    Relations <- model$relations
    Weights <- model$weight_vals
    model$status <- history$status
    model$concepts <- history$concepts
    model$relations <- history$relations
    model$weight_vals <- history$weight_vals
    history$status <- Status
    history$concepts <- Concepts
    history$relations <- Relations
    history$weight_vals <- Weights
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
    history$weight_vals = NULL
    model$concepts = NULL
    model$relations = NULL
    model$status = NULL
    model$weight_vals = weight_vals_default
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
  
  #--------------------------------------#
  #IMPLEMENT INTERFACE FOR STARTING MODEL
  #--------------------------------------#
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
  
  
  
  #----------------------------------------------#
  ### IMPLEMENT INTERFACE FOR EDITING MODEL CONCEPTS
  #----------------------------------------------#
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
      ExistingAffected <- 
        unlist(lapply(model$relations, function(x) x$concept_id))
      idx <- which(ExistingAffected == Var)
      if (length(idx)!=0){
        # Delete relations where the deleted concept is affected
        model$relations[[idx]] <- NULL
      }
      
      c_idxs <- which(relationstable()$From==Var)
      if (length(c_idxs)>0){
        for (i in c_idxs){
          g <- relationstable()[i,"Grouping"]
          a_idx <- which(ExistingAffected == relationstable()[i,"To"])
        
          links <- model$relations[[a_idx]]$affected_by[[g]]$links
          ExistingLinked <- unlist(lapply(links, function(x) x$concept_id))
          c_idx <- which(ExistingLinked == Var)
          # Delete relations where the deleted concept is the causal concept
          model$relations[[a_idx]]$affected_by[[g]]$links[[c_idx]] <- NULL
          if (length(model$relations[[a_idx]]$affected_by[[g]]$links) == 0){
            model$relations[[a_idx]]$affected_by[[g]] <- NULL
            if (length(model$relations[[a_idx]]$affected_by) == 0){
              model$relations[[a_idx]] <- NULL
            }
          }
        }
      }
      
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
  
  
  
  #-----------------------------------------------#
  #IMPLEMENT INTERFACE FOR EDITING MODEL RELATIONS
  #-----------------------------------------------#
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

  
  # On change of selected causal concept, update GUI and current effect ("effects") 
  # to match the relations table
  observeEvent(
    c(input$causalConcept,input$undoRelationAction),
    {
      # First deselect rows in relations table
      relationsTableEditing_proxy %>% selectRows(NULL)
      if (input$causalConcept != "" && length(input$causalConcept)>0){
        # Get all the relations that stem from this causal concept
         Effects_df <- relationstable()[relationstable()$From==input$causalConcept,]
        if (nrow(Effects_df)>0){
          effects$to <- Effects_df$To
          effects$from <- Effects_df$From
          effects$direction <- Effects_df$Direction
          effects$strength <- Effects_df$Weight
          effects$description <- Effects_df$Description
          effects$grouping <- Effects_df$Grouping
          effects$k <- Effects_df$k
          effects$type <- Effects_df$Type
          # Then, match the one that corresponds to the affected concept selected (if applicable)
          if (input$affectedConcept %in% effects$to) {
            updateTextInput(session, "causalDirection",
                            value = effects$direction[effects$to == input$affectedConcept])
            updateTextInput(session, "causalStrength",
                            value = effects$strength[effects$to == input$affectedConcept])
            updateTextInput(session, "causalDesc",
                            value = effects$description[effects$to == input$affectedConcept])
            updateTextInput(session, "relGrouping",
                            value = effects$grouping[effects$to == input$affectedConcept])
            updateTextInput(session, "relK",
                            value = effects$k[effects$to == input$affectedConcept])
            updateTextInput(session, "relType",
                            value = effects$type[effects$to == input$affectedConcept])
            # and change the selected row in the table to match
            r <- which(relationstable()$From==input$causalConcept & relationstable()$To == input$affectedConcept)
            relationsTableEditing_proxy %>% selectRows(as.numeric(r))
          } else {
            updateTextInput(session, "causalDirection", value = "")
            updateTextInput(session, "causalStrength", value = "")
            updateTextInput(session, "causalDesc", value = "")
            # note: relations info doesn't reset on purpose
          }
        } else { # If no pair of concepts are selected in UI
          effects$to <- ""
          effects$concept <- ""
          effects$direction <- ""
          effects$strength <- ""
          effects$description <- ""
          updateTextInput(session, "causalDirection", value = "")
          updateTextInput(session, "causalStrength", value = "")
          updateTextInput(session, "causalDesc", value = "")
          # note: relations info doesn't reset on purpose
        }
      }
    } 
  )
  # On change of selected affected concept, update GUI and current effect ("effects") 
  # to match the relations table
  observeEvent(
    input$affectedConcept,
    {
      # (Assume causal concept already selected): match the one that corresponds to the affected concept selected (if applicable)
      if (input$affectedConcept %in% effects$to) {
        flag_newRelation <<- FALSE
        updateTextInput(session, "causalDirection",
                        value = effects$direction[effects$to == input$affectedConcept])
        updateTextInput(session, "causalStrength",
                        value = effects$strength[effects$to == input$affectedConcept])
        updateTextInput(session, "causalDesc",
                        value = effects$description[effects$to == input$affectedConcept])
        updateTextInput(session, "relGrouping",
                        value = effects$grouping[effects$to == input$affectedConcept])
        updateTextInput(session, "relK",
                        value = effects$k[effects$to == input$affectedConcept])
        updateTextInput(session, "relType",
                        value = effects$type[effects$to == input$affectedConcept])
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
      CausalConcept <- 
        model$concepts$concept_id[model$concepts$concept_id == input$causalConcept]
      AffectedConcept <- 
        model$concepts$concept_id[model$concepts$concept_id == input$affectedConcept]
      
      ExistingAffected<- unlist(lapply(model$relations, function(x) x$concept_id))

      NewEffect_ls <-
        list(concept_id = CausalConcept,
             direction = input$causalDirection,
             weight = input$causalStrength,
             description = input$causalDesc)
      
      if ((length(ExistingAffected)>0) && (AffectedConcept %in% ExistingAffected)) {
            # Find where the new data should go
            a_idx <- which(ExistingAffected == AffectedConcept)
            group_idx <- input$relGrouping
            links <- model$relations[[a_idx]]$affected_by[[group_idx]]$links
            ExistingLinked <- unlist(lapply(links, function(x) x$concept_id))
            if (length(ExistingLinked)>0 && CausalConcept %in% ExistingLinked){
              c_idx <- which(ExistingLinked == CausalConcept)
            } else{
              c_idx <- length(ExistingLinked) + 1
            }
            # Insert new values in existing slot
            model$relations[[a_idx]]$affected_by[[group_idx]]$links[[c_idx]] <- NewEffect_ls
            model$relations[[a_idx]]$affected_by[[group_idx]]$type <- input$relType
            model$relations[[a_idx]]$k <- input$relK
      } else {
            # Create new slot for new affected concept at the end of the list
            model$relations[[length(ExistingAffected) + 1]] <-
              list(concept_id = AffectedConcept, 
                   affected_by = list(list(links = list(NewEffect_ls), type = input$relType)),
                   k = input$relK)
      }
      # Reset flag for new relation
      flag_newRelation <<- FALSE
      # Show notification
      showNotification(
        ui = "Relationship added/ updated",
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
      CausalConcept <- 
        model$concepts$concept_id[model$concepts$concept_id == input$causalConcept]
      AffectedConcept <- 
        model$concepts$concept_id[model$concepts$concept_id == input$affectedConcept]
      
      ExistingAffected<- unlist(lapply(model$relations, function(x) x$concept_id))
      
      if ((length(ExistingAffected)>0) && (AffectedConcept %in% ExistingAffected)){
        # Find where to delete
        a_idx <- which(ExistingAffected == AffectedConcept)
        links <- model$relations[[a_idx]]$affected_by[[effects$grouping]]$links
        ExistingLinked <- unlist(lapply(links, function(x) x$concept_id))
        if (length(ExistingLinked)>0 && CausalConcept %in% ExistingLinked){
          c_idx <- which(ExistingLinked == CausalConcept)
        } else{
          c_idx <- length(ExistingLinked) + 1
        }
        # Remove link, and if that was the only link, remove link group or/ and affected concept from the list of relations
        model$relations[[a_idx]]$affected_by[[effects$grouping]]$links[[c_idx]] <- NULL
        if (length(model$relations[[a_idx]]$affected_by[[effects$grouping]]$links) == 0){
          model$relations[[a_idx]]$affected_by[[effects$grouping]] <- NULL
          if (length(model$relations[[a_idx]]$affected_by) == 0){
            model$relations[[a_idx]] <- NULL
          }
        }
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
  
  # Change edge weight values
  # Save constraint
  observeEvent(
    input$updateWeight,
    {
      model$weight_vals[input$qualWeight] <- input$quantWeight
    }
  )
  
  
  #-----------------------------------------------#
  # IMPLEMENT MODEL RUNS
  #-----------------------------------------------#
  
  # Get run parameters from UI
  run_params <- reactive({
    k_df <- merge(model$concepts["concept_id"], 
                  data.frame(concept_id=sapply(model$relations, "[[", "concept_id"), 
                             k=sapply(model$relations, "[[", "k")), all.x = TRUE)
    ks <- as.numeric(k_df$k)
    names(ks) <- k_df$concept_id
    
    list(h = input$sliderFCM_h, lambda = input$sliderFCM_lambda, k= ks, 
         init = input$sliderFCM_init, infer_type = input$selectFCM_fn,
         iter = 30)
  })
  
  #Define dropdown element to select concept for constraining / clamping
  output$selectScenVar <- renderUI({
    selectInput(
      inputId = "scenVar",
      label = "Select value to constrain/ clamp",
      choices = sort(model$concepts$concept_id)
    )
  })
  
  # Save constraint
  observeEvent(
    input$addFCMConstraint,
    {
      run$constraints_list[input$scenVar] <- input$scenVal
    }
  )
  
  # Delete constraint
  observeEvent(
    input$deleteFCMConstraint,
    {
      run$constraints_list <- run$constraints_list[which(names(run$constraints_list)!=input$scenVar)]
    }
  )
  
  # Run model
  observeEvent(
    input$runFCMAction,
    {
      if (is.null(model$relations)){
        showNotification(
          ui = "No model loaded. Please load a model before proceeding.",
          duration = 2, 
          closeButton = TRUE,
          type = "message"
        )
      } else{
        run$parameters <- run_params()
        run$results <- run_model(model, run$parameters, run$constraints_list)
      }
    }
  )
  

  #-----------------------------------------------#
  # OUTPUT TABLES FOR UI
  #-----------------------------------------------#
  
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

  # Output display of numerical equivalents of weights in model
  output$weightsTable <- renderTable({
    data.frame("Qualitative weight" = names(model$weight_vals),
               "Numerical value" = model$weight_vals, check.names = FALSE)
  })
  
  #Implement relations graph
  output$relations_graph <- renderGrViz({
    if (length(model$concepts)>0){
      Dot_ <- makeDot(Model = model,
                      RowGroup = "All",#input$causalGroup,
                      ColGroup = "All",#input$affectedGroup,
                      orientation = "Portrait",#Landscape",#input$graphOrientation,
                      rankdir = "Top-to-Bottom",#input$graphLayout,
                      shape = "box",#input$nodeShape,
                      Show = "Level")#input$edgeLabel)
      grViz(Dot_)
    }
  })
  
  # Add a second (identical) graph for display in the FCM exploration tab
  output$relations_graph2 <- renderGrViz({ 
    if (length(model$concepts)>0){
      Dot_ <- makeDot(Model = model,
                      RowGroup = "All",#input$causalGroup,
                      ColGroup = "All",#input$affectedGroup,
                      orientation = "Portrait",#Landscape",#input$graphOrientation,
                      rankdir = "Top-to-Bottom",#input$graphLayout,
                      shape = "box",#input$nodeShape,
                      Show = "Level")#input$edgeLabel)
      grViz(Dot_)
    }
  })
  
  # Model parameters: FCM function
  output$FCMFunction <- renderPlot({
    h <- input$sliderFCM_h
    lambda <- input$sliderFCM_lambda
    switch(input$selectFCM_fn,
           "sigmoid-exp" =  curve(1/(1 + exp(-lambda * (x - h))), from = -1, to = 1, ylim = c(0,1)),
           "sigmoid-tanh" = curve(tanh(lambda * (x - h)), from = -1, to = 1, ylim = c(-1,1)),
           "linear" = curve((x), from = -1, to = 1, ylim = c(-1,1))
    )
  })
  
  # Display parameters for run
  output$paramsTable <- renderTable(
    if (!is.null(run$parameters)){
      params <- run$parameters
      if (params$infer_type == "linear"){
        data.frame("Function type" = params$infer_type, "h" = NA, "Lambda" = NA, 
                   check.names = FALSE)
      } else{
        data.frame("Function type" = params$infer_type, "h" = params$h, "Lambda" = params$lambda, 
                   check.names = FALSE)
      }
    }
  )
  
  # Display values constrained
  output$constraintsTable <- renderTable(
    if (is.null(run$constraints_list)){
      data.frame(Variable = c(), Value = c())
    } else{
      data.frame(Variable = names(run$constraints_list), Value = run$constraints_list)
    }
  )
  
  # Model output: table
  output$resultsTable <- DT:: renderDataTable(
    run$results,
    server = FALSE,
    options = list(dom='tp', pageLength = 15)
  )
  
  output$resultsPlotSim <- renderPlotly({
    if (!is.null(run$results)){
      df <- run$results
      df$timestep <- 1:nrow(df)
      df <- tidyr::pivot_longer(df, !timestep, names_to = "concept", values_to = "value")
      if (run$parameters$infer_type == "sigmoid-exp"){
        ylims <- c(0,1)
      } else {
        ylims <- c(-1,1)
      }
      plot_ly(df, x = ~timestep, y = ~value) %>%
        add_lines(linetype = ~concept) %>%
        layout(yaxis = list(range = ylims))
    }
  })
  
}) #end: shinyServer
