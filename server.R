# server.R
# Author: Patricia Angkiriwang, University of British Columbia - 2019-2021
# with code initially adapted from open-source R Shiny app by Brian Gregor, Oregon Systems Analytics LLC

## NOTES ON DATA FORMAT:
# > model: A model is a list of 2
# model$concepts: a data frame with columns (chr/string) - name, variable, description, values, group 
# model$relations: a list of [# of concepts with incoming links], each a list of 2
#   concept_name: (chr - name of affected variable)
#   affected_by: (list of[#things that affect it])


## NOTES ON THIS VERSION:
# (2021/02) This version of the model does not use min/max concept values (commented out) -- a relic of the original Logic Laboratory app

# Load packages and necessary scripts ---------------
library(shiny) 
library(shinyBS) # bootstrap for formatting
library(plotly) # for interactive plots
library(DT) # data tables
library(grid) # arranging multiple plots (unused?)
library(jsonlite) # for reading in/out data
library(dplyr) # for data wrangling
library(ggplot2) # plots
library(tidyr) # tidying data
library(DiagrammeR) # for visualizing graph
source("helper.R") # load helper.R script (contains necessary functions)
source('fcm.R') # load model algorithm
# source('../../bcfn_seafood_access_modelling/fcm.R')

# SHINY SERVER FUNCTION
shinyServer(function(input, output, session) {
  
  # Define some settings and global variables here --------------
  weight_vals_default <- c(VL = 0.1, L = 0.25, ML = 0.375, M = 0.5, MH = 0.625, H = 0.75, VH = 0.9) 
  #c(VL = 1, L = 1, ML = 1, M = 1, MH = 1, H = 1, VH = 1) 
  
  
  # === CREATE OBJECTS TO STORE MODEL, ETC. ------ ==================
  # Reactive object to store current state that interface responds to
  model <- reactiveValues(status = NULL, concepts = NULL, relations = NULL, weight_vals = weight_vals_default)
  # Reactive object to store model history (unlimited undo)
  history <- reactiveValues(status = NULL, concepts = NULL, relations = NULL, weight_vals = NULL)#,  previousRowNum = NULL) #note: attempts to keep rows from resetting have failed 2019/05/22
  # Global variable that flags whenever a new relationship is defined
  flag_newRelation <- FALSE
  # Reactive object to keep track of various conditions
  is <- reactiveValues(newconcept = FALSE)
  
  # Reactive object to store current selected effects (what are the relations corresponding to the currently selected causal concept?)
  selectedfx <- reactiveValues(
      to = "", # <- a vector
      from = "",
      direction = "",
      strength = "",
      description = "")
  
  # Reactive object to store the settings for the current model simulation/run
  run <- reactiveValues(results = NULL, parameters= NULL, constraints = NULL, sweep_params = NULL, sweep_results = NULL)

  # Create a reactive object to store scenario data in
  scenarios <- reactiveValues(results = NULL, constraints = NULL, parameters = NULL)

  # === DEFINE COMMON FUNCTIONS FOR MODIFYING REACTIVE VALUES ----- ===================
  # Function to save the model in history
  saveLastState <- function() {
    history$status <- model$status
    history$concepts <- model$concepts
    history$relations <- model$relations
    history$weight_vals <- model$weight_vals
  }
  # Function to swap model and history (i.e. undo)
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
  # Function to undo concept edit
  undoConceptEdit <- function() {
    swapState()
  }
  # Function to undo relation edit
  undoRelationEdit <- function() {
    swapState()
  }
  
  # Function to update concept form inputs
  updateConceptForm <- function(RowNum) {
    updateTextInput(session, "conceptName",
                    value = model$concepts$name[RowNum])
    updateTextInput(session, "conceptID",
                    value = model$concepts$concept_id[RowNum])
    updateTextInput(session, "conceptDesc",
                    value = model$concepts$description[RowNum])
    # updateTextInput(session, "minValue",
    #                 value = model$concepts$values$min[RowNum])
    # updateTextInput(session, "maxValue",
    #                 value = model$concepts$values$max[RowNum])
    # updateTextInput(session, "valuesDesc",
    #                 value = model$concepts$values$description[RowNum])
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
  
  # Function to clear reactive data when when changing model
  resetModel <- function(){
    history$status = NULL
    history$concepts = NULL
    history$relations = NULL
    history$weight_vals = NULL
    model$concepts = NULL
    model$relations = NULL
    model$status = NULL
    model$weight_vals = weight_vals_default
    selectedfx$to <- ""
    selectedfx$from <- ""
    selectedfx$direction <- ""
    selectedfx$strength <- ""
    selectedfx$description <- ""
    run$results <- NULL
    run$parameters <- NULL
    run$constraints <- NULL
    run$sweep_params <- NULL
    run$sweep_results <- NULL
    run$sweep_constraints <- NULL
    scenarios$results <- NULL
    scenarios$constraints <- NULL
    scenarios$parameters <- NULL
  }
  
  resetRun <- function(){
    run$results <- NULL
    run$parameters <- NULL
    run$constraints <- NULL
    run$sweep_params <- NULL
    run$sweep_results <- NULL
    run$sweep_constraints <- NULL
    updateTextInput(session, "scenarioName", value = "")
  }
  
  resetScenarios <- function(){
    scenarios$results <- NULL
    scenarios$constraints <- NULL
    scenarios$parameters <- NULL
  }
  
  # === IMPLEMENT INTERFACE FOR INITIALIZING MODEL ------ =========================
  # Output: Define GUI element to select model from a list -----------------
  output$selectExistingModelFile <- renderUI({
    selectInput(
      inputId = "modelFileName",
      selected = NULL,
      label = NULL,
      choices = dir(path = "./models")[dir(path = "./models") != "templates"]
    )
  })
  
  # Define author name -----------------
  ModelAuthor <- reactive({
    if (input$anonymous){
      author <- "Anonymous"
    } else{
      author <- ifelse(input$organization=="", paste0(input$firstName, " ", input$lastName), paste0(input$firstName, " ", input$lastName, " (", input$organization, ")"))
    }
    return(author)
  })
  
  # Choose model start option and initialize model -----------------
  observeEvent(
    input$startModeling,
    {
      if(input$modelAction == "select_existing") { 
        model$status <- loadModelStatus(input$modelFileName)
        model$concepts <- loadModelConcepts(input$modelFileName)
        model$relations <- loadModelRelations(input$modelFileName) 
        #View(model$relations)
        startmessage <- "Model loaded from /models folder"
        
      } # end if input$modelAction 
      
      showNotification(
        ui = startmessage,
        duration = 2, 
        closeButton = TRUE,
        type = "message"
      )
      
      updateConceptForm(1)
      updateRelationForm(1)
      saveLastState()
      resetRun()
      resetScenarios()
    }) # end: observeEvent - input$startModeling
  
  
  
  # === IMPLEMENT INTERFACE FOR EDITING MODEL CONCEPTS ------ ===============================
  # Update concept form based on what is selected in table ------------
  observeEvent(
    c(input$conceptsTableEditing_rows_selected,input$undoConceptAction), # fixed broken undo by adding additional argument here 2019/05/22
    {
      if(!is.null(input$conceptsTableEditing_rows_selected)){
        RowNum <- input$conceptsTableEditing_rows_selected
        updateConceptForm(RowNum)
      }
    }
  )
  # Implement the new concept button -----------------
  observeEvent(
    input$addConcept,
    {
      if (input$conceptID %in% model$concepts$concept_id) {
        createAlert(session = session, anchorId = "duplicateConceptVariable", 
                    title = "Duplicate ID", 
                    content = "New concept ID is the same as an existing concept. Rename before updating.")
        return()        
      }
      model$concepts <- model$concepts[c(1,1:nrow(model$concepts)),]
      model$concepts$name[1] <- input$conceptName
      model$concepts$concept_id[1] <- input$conceptID
      model$concepts$description[1] <- input$conceptDesc
      model$concepts$group[1] <- input$conceptGroup
      # model$concepts$values$min[1] <- ""
      # model$concepts$values$max[1] <- ""
      # model$concepts$values$description[1] <- ""
      RowNum <- input$conceptsTableEditing_rows_selected
      updateConceptForm(RowNum)
    }
  )
  # Implement the update concept button -----------------
  observeEvent(
    input$updateConcept,
    {
      saveLastState() # save state of current model
      
      # If ID already exists, just update the rest
      if (input$conceptID %in% model$concepts$concept_id) {
        idx <- model$concepts$concept_id == input$conceptID
        # Update model concepts corresponding to that ID
        model$concepts[idx,"name"] <- input$conceptName
        model$concepts[idx,"description"] <- input$conceptDesc
        model$concepts[idx, "group"] <- input$conceptGroup
        # model$concepts[idx, "values.min"] <- input$minValue
        # model$concepts[idx, "values.max"] <- input$maxValue
        # model$concepts[idx, "values.description"] <- input$valuesDesc
      } else {
        # If not, update everything based on the row selected
        
        # Update model concepts
        RowNum <- input$conceptsTableEditing_rows_selected
        model$concepts$name[RowNum] <- input$conceptName
        model$concepts$concept_id[RowNum] <- input$conceptID
        model$concepts$description[RowNum] <- input$conceptDesc
        model$concepts$group[RowNum] <- input$conceptGroup
        # model$concepts$values$min[RowNum] <- input$minValue
        # model$concepts$values$max[RowNum] <- input$maxValue
        # model$concepts$values$description[RowNum] <- input$valuesDesc
      }

      model$status$lastedit <- as.character(Sys.time())
      
      showNotification(
        ui = "Concept updated",
        duration = 1, 
        closeButton = TRUE,
        type = "message"
      )
    }
  )
  # Implement the undo button ----------------
  observeEvent(
    input$undoConceptAction,
    {
      undoConceptEdit()
      model$status$lastedit <- as.character(Sys.time())
    }
  )
  # Implement the delete concept button -----------------
  observeEvent(
    input$deleteConcept,
    {
      saveLastState() # save last model state
      
      # Get concept to be deleted
      RowNum <- input$conceptsTableEditing_rows_selected
      Var <- model$concepts$concept_id[RowNum]
      
      # Remove concept from model concepts table
      model$concepts <- model$concepts[-RowNum,]
      model$status$lastedit <- as.character(Sys.time())
      
      # Remove concept from model relations list
      tbl <- isolate(relationstable()) # Get relationstable for reference
      items_to_delete <- c() # Define vector for deleting items in relations list
      
      # Get variables in the model that are affected by something (in relations list)
      ExistingAffected <-
        unlist(lapply(model$relations, function(x) x$concept_id))
      
      # Add to delete list: relations where the deleted concept is affected
      idx <- which(ExistingAffected == Var) # Find index in relations list that corresponds to the deleted concept
      if (length(idx)!=0){
        items_to_delete <- c(items_to_delete, idx)
      }
      
      # Add to delete list / delete relations where the deleted concept affects something else
      c_idxs <- which(tbl$From==Var) # Get indices of relevant rows (where deleted concept affects another)
      if (length(c_idxs)>0){
        # For each relevant row...
        for (i in c_idxs){ 
          # Find the element in the relations list corresponding to the relation to be deleted
          g <- tbl[i,"Grouping"] # Get the group number corresponding to the link
          LinkTo <- tbl[i,"To"] # What is the affected concept?
          a_idx <- which(ExistingAffected == LinkTo)  # Get index of the affected concept
          links <- model$relations[[a_idx]]$affected_by[[g]]$links # Get links in that grouping
          ExistingLinked <- unlist(lapply(links, function(x) x$concept_id)) # All links to the affected concept
          c_idx <- which(ExistingLinked == Var) # Which one corresponds to the deleted concept?
          
          # Delete relations where the deleted concept is the causal concept
          model$relations[[a_idx]]$affected_by[[g]]$links[[c_idx]] <- NULL
          if (length(model$relations[[a_idx]]$affected_by[[g]]$links) == 0){
            model$relations[[a_idx]]$affected_by[[g]] <- NULL
            # If the affected concept no longer has any more links, them remove the whole thing from the relations list (add to delete list)
            if (length(model$relations[[a_idx]]$affected_by) == 0){
              items_to_delete <- c(items_to_delete, a_idx)
            }
          }
        }
      }
      # Now delete primary elements in relations list (do this later and all at once so the indices don't change after each deletion)
      if (length(items_to_delete)>0){
        model$relations <- model$relations[-items_to_delete]
      }
      # Update the input form
      updateConceptForm(RowNum)
    }) #observeEvent: deleteConcept
  
  # Notification when model saved ---------
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
  
  
  # === IMPLEMENT INTERFACE FOR EDITING MODEL RELATIONS ------ ===============================
  # Update relations form based on what is selected in table ----------------
  observeEvent(
    c(input$relationsTableEditing_rows_selected,input$undoRelationAction),
    {
      if(!is.null(input$relationsTableEditing_rows_selected) || flag_newRelation==FALSE){
        RowNum <- input$relationsTableEditing_rows_selected
        updateRelationForm(RowNum)
      } 
    }
  )

  # Output: Define dropdown element to select causal concept from a list (for editing) ----------------
  output$selectCausalConcept <- renderUI({
    selectInput(
      inputId = "causalConcept",
      label = "Causal Concept",
      choices = sort(model$concepts$concept_id)
      #choices = sort(model$concepts$name)
    )
  })
  # Output: Define dropdown element to select affected concept from a list (for editing) ----------------
  output$selectAffectedConcept <- renderUI({
    selectInput(
      inputId = "affectedConcept",
      label = "Affected Concept",
      choices = sort(model$concepts$concept_id)
      #choices = sort(model$concepts$name)
    )
  })
  
  # Create reactive value that contains a formatted relations table (for internal use, not display) ----------------
  relationstable <- reactive({
    # Note: right now the only difference (between this and the table that is displayed in the GUI 
    # is that full names are not used here. If eventually full names are used in the editing relations dropdowns
    # then this can be consolidated and used for the GUI too.
    if (length(model$relations)!=0){
      formatRelationTable(model$relations,model$concepts,use.full.names=FALSE,export=TRUE)
    }
  })
  
  # Update GUI (text input + dropdowns) and current effect ("selectedfx") to match the relations table ------------------------
  observeEvent( # On change of selected causal concept
    c(input$causalConcept,input$undoRelationAction),
    {
      # First deselect rows in relations table
      relationsTableEditing_proxy %>% selectRows(NULL)
      if (input$causalConcept != "" && length(input$causalConcept)>0){
        # Get relations table to find indices
        tbl <- isolate(relationstable())
        # Get all the relations that stem from this causal concept
         Effects_df <- tbl[tbl$From==input$causalConcept,]
        if (nrow(Effects_df)>0){
          selectedfx$to <- Effects_df$To
          selectedfx$from <- Effects_df$From
          selectedfx$direction <- Effects_df$Direction
          selectedfx$strength <- Effects_df$Weight
          selectedfx$description <- Effects_df$Description
          selectedfx$grouping <- Effects_df$Grouping
          selectedfx$k <- Effects_df$k
          selectedfx$type <- Effects_df$Type
          # Then, match the one that corresponds to the affected concept selected (if applicable)
          if (input$affectedConcept %in% selectedfx$to) {
            updateTextInput(session, "causalDirection",
                            value = selectedfx$direction[selectedfx$to == input$affectedConcept])
            updateTextInput(session, "causalStrength",
                            value = selectedfx$strength[selectedfx$to == input$affectedConcept])
            updateTextInput(session, "causalDesc",
                            value = selectedfx$description[selectedfx$to == input$affectedConcept])
            updateTextInput(session, "relGrouping",
                            value = selectedfx$grouping[selectedfx$to == input$affectedConcept])
            updateTextInput(session, "relK",
                            value = selectedfx$k[selectedfx$to == input$affectedConcept])
            updateTextInput(session, "relType",
                            value = selectedfx$type[selectedfx$to == input$affectedConcept])
            # and change the selected row in the table to match
            r <- which(tbl$From==input$causalConcept & tbl$To == input$affectedConcept)
            relationsTableEditing_proxy %>% selectRows(as.numeric(r))
          } else {
            updateTextInput(session, "causalDirection", value = "")
            updateTextInput(session, "causalStrength", value = "")
            updateTextInput(session, "causalDesc", value = "")
            # note: relations info doesn't reset on purpose
          }
        } else { # If no pair of concepts are selected in UI
          selectedfx$to <- ""
          selectedfx$concept <- ""
          selectedfx$direction <- ""
          selectedfx$strength <- ""
          selectedfx$description <- ""
          updateTextInput(session, "causalDirection", value = "")
          updateTextInput(session, "causalStrength", value = "")
          updateTextInput(session, "causalDesc", value = "")
          # note: relations info doesn't reset on purpose
        }
      }
    } 
  )

  observeEvent( # On change of selected affected concept
    input$affectedConcept,
    {
      # (Assume causal concept already selected): match the one that corresponds to the affected concept selected (if applicable)
      if (input$affectedConcept %in% selectedfx$to) {
        flag_newRelation <<- FALSE
        updateTextInput(session, "causalDirection",
                        value = selectedfx$direction[selectedfx$to == input$affectedConcept])
        updateTextInput(session, "causalStrength",
                        value = selectedfx$strength[selectedfx$to == input$affectedConcept])
        updateTextInput(session, "causalDesc",
                        value = selectedfx$description[selectedfx$to == input$affectedConcept])
        updateTextInput(session, "relGrouping",
                        value = selectedfx$grouping[selectedfx$to == input$affectedConcept])
        updateTextInput(session, "relK",
                        value = selectedfx$k[selectedfx$to == input$affectedConcept])
        updateTextInput(session, "relType",
                        value = selectedfx$type[selectedfx$to == input$affectedConcept])
        # and change the selected row in the table to match
        r <- which(isolate(relationstable())$From==input$causalConcept & isolate(relationstable())$To == input$affectedConcept)
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
  # Implement the update relations button --------------------
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
  # Implement the delete relation feature -----------------------------
  observeEvent(
    input$deleteRelation,
    {
      # Save last model state and relations inputs
      saveLastState()
      # Remove relation from model
      CausalConcept <- 
        model$concepts$concept_id[model$concepts$concept_id == input$causalConcept]
      AffectedConcept <- 
        model$concepts$concept_id[model$concepts$concept_id == input$affectedConcept]
      
      ExistingAffected<- unlist(lapply(model$relations, function(x) x$concept_id))
      
      if ((length(ExistingAffected)>0) && (AffectedConcept %in% ExistingAffected)){
        # Find where to delete
        a_idx <- which(ExistingAffected == AffectedConcept)
        links <- model$relations[[a_idx]]$affected_by[[selectedfx$grouping]]$links
        ExistingLinked <- unlist(lapply(links, function(x) x$concept_id))
        if (length(ExistingLinked)>0 && CausalConcept %in% ExistingLinked){
          c_idx <- which(ExistingLinked == CausalConcept)
        } else{
          c_idx <- length(ExistingLinked) + 1
        }
        # Remove link, and if that was the only link, remove link group or/ and affected concept from the list of relations
        model$relations[[a_idx]]$affected_by[[selectedfx$grouping]]$links[[c_idx]] <- NULL
        if (length(model$relations[[a_idx]]$affected_by[[selectedfx$grouping]]$links) == 0){
          model$relations[[a_idx]]$affected_by[[selectedfx$grouping]] <- NULL
          if (length(model$relations[[a_idx]]$affected_by) == 0){
            model$relations[[a_idx]] <- NULL
          }
        }
      }
      
      # Update text fields
      updateTextInput(session, "causalDirection", value = "")
      updateTextInput(session, "causalStrength", value = "")
      updateTextInput(session, "causalDesc", value = "")
      model$status$lastedit <- as.character(Sys.time())
    }
  )
  # Undo relations edit -----------------------------
  observeEvent(
    input$undoRelationAction,
    {
      undoRelationEdit()
      updateTextInput(session, "causalDirection",
                      value = selectedfx$direction[selectedfx$to == input$affectedConcept])
      updateTextInput(session, "causalStrength",
                      value = selectedfx$strength[selectedfx$to == input$affectedConcept])
      updateTextInput(session, "causalDesc",
                      value = selectedfx$description[selectedfx$to == input$affectedConcept])
      model$status$lastedit <- as.character(Sys.time())
    }
  )
  
  # Change edge weight values -----------------------------
  observeEvent(
    input$updateWeight,
    {
      model$weight_vals[input$qualWeight] <- input$quantWeight
    }
  )
  
  # === IMPLEMENT MODEL RUNS ------- ================================================
  # Get run parameters from UI -------------
  run_params <- reactive({
    k_df <- merge(model$concepts["concept_id"], 
                  data.frame(concept_id=sapply(model$relations, "[[", "concept_id"), 
                             k=sapply(model$relations, "[[", "k")), all.x = TRUE)
    ks <- as.numeric(k_df$k)
    names(ks) <- k_df$concept_id
    
    list(h = input$sliderFCM_h, lambda = input$sliderFCM_lambda, k= ks, 
         init = input$sliderFCM_init, infer_type = input$selectFCM_fn,
         iter = input$numIterations)
  })
  
  # Output: Define slider to select initial starting values --------
  output$initSlider <- renderUI({
    sliderInput("sliderFCM_init", "Initial values for simulation", min=clampSliderMin(), max=1, step = 0.25, value = 1)
  })
  
  # Output: Define dropdown element to select concept for constraining / clamping --------------
  output$selectScenVar <- renderUI({
    selectInput(
      inputId = "scenVar",
      label = "Select value to constrain/ clamp",
      choices = sort(model$concepts$concept_id)
    )
  })
  
  # Output: Define slider to select clamp value ------------
  clampSliderMin <- reactive({
    ifelse(input$selectFCM_fn == 'sigmoid-tanh',-1,0)
  })
  
  output$clampSlider <- renderUI({
    sliderInput(
      inputId = "scenVal", 
      label = "Value (fixed) throughout simulation", 
      min=clampSliderMin(), max=1, step = 0.5, value = 1)
  })
  
  # Save constraint for FCM  ------------------
  observeEvent(
    input$addFCMConstraint,
    {
      if (is.null(run$constraints)){
        run$constraints <- c()
      }
      run$constraints[input$scenVar] <- input$scenVal
    }
  )
  
  # Delete constraint  ------------------
  observeEvent(
    input$deleteFCMConstraint,
    {
      run$constraints <- run$constraints[which(names(run$constraints)!=input$scenVar)]
    }
  )
  
  # Clear all constraints ------------------
  observeEvent(
    input$clearAllFCMConstraints,
    {
      run$constraints <- c()
    }
  )
  
  # Run model ------------------
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
        run$results <- run_model(model, run$parameters, run$constraints, encode = TRUE)
        
        # Change scenario name text when a new set of constraints/parameters are run
        updateTextInput(session, "scenarioName",
                        value = isolate(scenarioNameString(run$parameters, run$constraints)))
        
        # For reference, parameter list looks like:
        # list(h = input$sliderFCM_h, lambda = input$sliderFCM_lambda, k= ks,
        #      init = input$sliderFCM_init, infer_type = input$selectFCM_fn,
        #      iter = 30)
      }
    }
  )
  
  # Run model with multiple parameters ------------------
  # -Note: Right now this is set to run one set of parameter values-- eventually could do combinations of parameters, perhaps
  
  extract <- function(text) {
    text <- gsub(" ", "", text)
    split <- strsplit(text, ",", fixed = FALSE)[[1]]
    as.numeric(split)
  }
  
  output$sweepText <- renderText({
    nums <- extract(input$sweepingVals)
    if (anyNA(nums)) {
      "Invalid input"
    } else {
      paste(c(input$sweepingParam,": ", paste(nums, collapse = ", ")), collapse = " ")
    }
  })
  
  varying_params <- reactive({ # Default: list(lambda = c(0.5, 1, 3, 5)) - matching input$sweepingVals specified in ui.R
    l <- list()
    l[[input$sweepingParam]] <- extract(input$sweepingVals)
    return(l)
  })
  
  output$sweepParam <-  renderUI({
    if (input$selectFCM_fn == "linear"){
      param_choices <- c("h", "init")
    } else {
      param_choices <- c("lambda","h", "init")
    }
    selectInput("sweepingParam", label = "Parameter to sweep", choices = param_choices)
  })
  
  observeEvent(
    input$runFCMSweepAction,
    {
      if (is.null(model$relations)){
        showNotification(
          ui = "No model loaded. Please load a model before proceeding.",
          duration = 2, 
          closeButton = TRUE,
          type = "message"
        )
      } else{
        varying_params <- isolate(varying_params())
        param_vals <- isolate(run_params()) # Get parameters from UI
        
        # Store information in sweep_results and sweep_params (keep independent from normal runs)
        N <- prod(sapply(varying_params,length)) # calculate how many runs there will be
        run$sweep_params <- vector("list",  N) # create new list that will contain all runs (permutations) of sweeps
        run$sweep_results <- vector("list",  N)
        run$sweep_constraints <- run$constraints
        
        n <- 1
        for (p in 1:length(varying_params)){
          pn = names(varying_params)[p]
          pvals = varying_params[[p]]
          # run$parameters[pn] = "multiple"
          for (i in 1:length(pvals)){
            params_run = replace(param_vals, pn, pvals[i]) # store all parameters here, using the value in the parameter sweep
            res <- run_model(model, params_run, run$sweep_constraints, encode = TRUE)
            run$sweep_params[[n]] <- params_run
            run$sweep_results[[n]] <- res
            n <- n + 1
          }
        }
      }
    }
  )
  
  # Define UI element to select concepts to constrain for set of runs -----
  output$conceptsForScenarios <- renderUI({
    selectizeInput(
      inputId = "conceptsForScenarios",
      label = "Test high/low scenarios for these concepts",
      choices = sort(model$concepts$concept_id),
      multiple = TRUE
    )
  })
  
  # Run model with multiple constraints -----------
  
  observeEvent(
    input$runFCMMultipleConstraints,     {
      if (is.null(model$relations)){
        showNotification(
          ui = "No model loaded. Please load a model before proceeding.",
          duration = 2, 
          closeButton = TRUE,
          type = "message"
        )
      } else{
        conceptsToConstrain <- isolate(input$conceptsForScenarios)
        # Create new list that will contain all runs (high and low for each concept selected)
        clampRunResults <- vector("list",  length(conceptsToConstrain) * 2) 
        for (cn in conceptsToConstrain){
          run$constraints <- c()
          run$parameters <- run_params()
          
          # Baseline
          run$results <- run_model(model, run$parameters, run$constraints, encode = TRUE)
          scenName <- isolate(scenarioNameString(run$parameters, run$constraints))
          
          scenarios$results[[scenName]] <- run$results 
          scenarios$constraints[[scenName]] <- "none"
          scenarios$parameters[[scenName]] <- run$parameters
          
          # Low scenario
          run$constraints[cn] <- clampSliderMin()
          run$results <- run_model(model, run$parameters, run$constraints, encode = TRUE)
          scenName <- isolate(scenarioNameString(run$parameters, run$constraints))
          
          scenarios$results[[scenName]] <- run$results 
          scenarios$constraints[[scenName]] <- run$constraints
          scenarios$parameters[[scenName]] <- run$parameters
          
          # High scenario
          run$constraints[cn] <- 1
          
          run$results <- run_model(model, run$parameters, run$constraints, encode = TRUE)
          scenName <- isolate(scenarioNameString(run$parameters, run$constraints))
          
          scenarios$results[[scenName]] <- run$results 
          scenarios$constraints[[scenName]] <- run$constraints
          scenarios$parameters[[scenName]] <- run$parameters
          
        }
        showNotification(
          ui = paste0("Set of runs (high/ low for each concept) saved to scenario comparison list"),
          duration = 2, 
          closeButton = TRUE,
          type = "message"
        )
        run$constraints <- c() # clear constraints again
      }
    })
  
  # === SCENARIO SAVE/ LAUNCH COMPARISON VIEW --------- ====================================================
  
  # Add current run to scenario comparison view ----
  observeEvent(
    input$addScenario,{
      
      if (is.null(model$relations)){
        showNotification(
          ui = "No model loaded. Please load a model before proceeding.",
          duration = 2, 
          closeButton = TRUE,
          type = "message"
        )
      } else if (is.null(run$results)){
        showNotification(
          ui = paste0("Please run the model before proceeding."),
          duration = 2, 
          closeButton = TRUE,
          type = "message"
        )
      } else {
        scenarios$results[[input$scenarioName]] <- run$results 
        if (length(run$constraints)>0){
          scenarios$constraints[[input$scenarioName]] <- c(run$constraints)
        } else {
          scenarios$constraints[[input$scenarioName]] <- "none"
        }
        scenarios$parameters[[input$scenarioName]] <- run$parameters
        
        showNotification(
          ui = paste0("Current run saved to scenario comparison list \n (", input$scenarioName, ")"),
          duration = 2, 
          closeButton = TRUE,
          type = "message"
        )
      }
    }
  )
  
  # Add current parameter sweep runs to scenario comparison view ----
  observeEvent(
    input$addSweep,{
      if (is.null(model$relations)){
        showNotification(
          ui = "No model loaded. Please load a model before proceeding.",
          duration = 2, 
          closeButton = TRUE,
          type = "message"
        )
      } else if (is.null(run$sweep_results)){
        showNotification(
          ui = paste0("Please run before proceeding."),
          duration = 2, 
          closeButton = TRUE,
          type = "message"
        )
      } else {
        
        if (length(run$sweep_constraints)>0){
          constraints <- c(run$constraints)
        } else {
          constraints <- "none"
        }
        
        # Loop over all parameters run/ saved
        for (i in 1:length(run$sweep_params)){
          scenName <- scenarioNameString(run$sweep_params[[i]], run$sweep_constraints)
          scenarios$results[[scenName]] <- run$sweep_results[[i]]
          scenarios$constraints[[scenName]] <- constraints
          scenarios$parameters[[scenName]] <- run$parameters
        }
        showNotification(
          ui = paste0("Runs saved to scenario comparison list"),
          duration = 2, 
          closeButton = TRUE,
          type = "message"
        )
      }
    }
  )
  
  # Define UI element to select scenarios to plot
  output$scenariosToPlot <- renderUI({
    selectizeInput(
      inputId = "scenariosToPlot",
      label = "Compare these scenarios",
      choices = sort(names(scenarios$results)),
      multiple = TRUE
    )
  })
  
  # Reset scenario view -----
  observeEvent(
    input$resetScenarios,{
      scenarios$results <- NULL
      scenarios$constraints <- NULL
      scenarios$parameters <- NULL
      # Clear output?
    }
  )
  
  # Save current scenarios into a file ----
  observeEvent(
    input$saveScenarios,{
      scenarios_save <- list(results = scenarios$results, 
                             constraints = scenarios$constraints, 
                             parameters = scenarios$parameters)
      saveRDS(scenarios_save, file = input$scenFileName)
      
      showNotification(
        ui = paste("File saved:", input$scenFileName),
        duration = 2, 
        closeButton = TRUE,
        type = "message"
      )
    }
  )
  
  # Load saved scenarios (overrides any scenarios that exist) ----
  observeEvent(
    input$scenFileToLoad,{
      scenarios_loaded <- readRDS(input$scenFileToLoad$datapath)
      scenarios$results <- scenarios_loaded$results
      scenarios$parameters <- scenarios_loaded$parameters
      scenarios$constraints <- scenarios_loaded$constraints
    }
  )

  
  # === OUTPUT TABLES FOR UI --------- ====================================================
  # See https://rstudio.github.io/DT/010-style.html for more options on renderDataTable()
  
  # (1) Model preview and editing
  # Output model concepts table (for preview) -------------------- 
  output$conceptsTable <- DT::renderDataTable(
    formatConceptTable(model$concepts), 
    server = FALSE, 
    options = list(pageLength = 20)
  )
  # Output model concepts table FOR EDITING -------------------- 
  output$conceptsTableEditing <- DT::renderDataTable(
    formatConceptTable(model$concepts), 
    server = FALSE, 
    selection = list(mode = 'single', target = 'row', selected = 1),
    options = list(pageLength = 20)
  )
  
  # Output model relations table (for preview) -------------------- 
  output$relationsTable <- DT::renderDataTable(
    formatRelationTable(model$relations,model$concepts),  
    server = FALSE, 
    options = list(pageLength = 20)
  )
  
  # Output model relations table FOR EDITING -------------------- 
  output$relationsTableEditing <- DT::renderDataTable(
    formatRelationTable(model$relations,model$concepts), 
    server = FALSE, 
    selection = list(mode = 'single', target = 'row'),#, selected = history$previousRowNum),
    options = list(paging = FALSE)
  )
  relationsTableEditing_proxy <- dataTableProxy('relationsTableEditing') # needed for ability to select rows

  # Output display of numerical equivalents of weights in model -------------------- 
  output$weightsTable <- renderTable({
    data.frame("Qualitative weight" = names(model$weight_vals),
               "Numerical value" = as.character(model$weight_vals), check.names = FALSE) # use as.character to display decimal points correctly
  })
  
  # Output relations graph  -------------------- 
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
  
  # (2) FCM Exploration
  # Output a second (identical) graph for display in the FCM exploration tab -------------------- 
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
  
  # Output display of FCM function corresponding to model parameters -------------------- 
  output$FCMFunction <- renderPlot({
    h <- input$sliderFCM_h
    lambda <- input$sliderFCM_lambda
    switch(input$selectFCM_fn,
           "sigmoid-exp" =  curve(1/(1 + exp(-lambda * (x - h))), from = -1, to = 1, ylim = c(0,1)),
           "sigmoid-tanh" = curve(tanh(lambda * (x - h)), from = -1, to = 1, ylim = c(-1,1)),
           "linear" = curve((x - h), from = -1, to = 1, ylim = c(-1,1))
    )
  })
  
  # Output table displaying parameters used for run -------------------- 
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
  
  # Output table displaying values constrained -------------------- 
  output$constraintsTable <- renderTable(
    if (length(run$constraints)>0){
      data.frame(Variable = names(run$constraints), Value = run$constraints)
    } else{
      data.frame(Variable = c(), Value = c())
    }
  )
  
  # Output table of model run/ simulation results  -------------------- 
  output$resultsTable <- DT:: renderDataTable(
    if(!is.null(run$results)){run$results %>% select(-c("init","lambda","h","infer_type"))} else {run$results},
    server = FALSE,
    options = list(dom='tp', pageLength = 15)
  )
  
  # Output plot of run results  -------------------- 
  output$resultsPlotSim <- renderPlotly({
    if (!is.null(run$results)){
      df <- run$results
      # df$timestep <- 1:nrow(df) # now taken care of beforehand
      df <- tidyr::pivot_longer(df, !c(timestep, infer_type, lambda, h), names_to = "concept", values_to = "value")
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
  
  # (2b) Sweep FCM parameters
  # Output table of equlibrium results (for multiple runs)  -------------------- 
  output$sweepEquilTable <- DT:: renderDataTable(
    dplyr::bind_rows(lapply(run$sweep_results, function(x) x[nrow(x),])), # Get last row of each run
    server = FALSE,
    options = list(dom='tp')
  )
  
  # Output plot of multiple run results (parameter sweep) -------------------- 
  output$sweepPlot <- renderPlotly({
    if (!is.null(run$sweep_results)){
      df <- bind_rows(run$sweep_results)
      # Convert to long format (note: this line needs to match the extra columns added in the runFCMSweepAction function above
      df <- tidyr::pivot_longer(df, !c(timestep, infer_type, h, lambda, init), names_to = "concept", values_to = "value") 
      plot <- ggplot(df, aes(x = timestep, y = value, colour = concept)) + theme_minimal() + 
        geom_line() + facet_wrap(facets=c(isolate(input$sweepingParam)), labeller = label_both) #facet_grid(h ~ lambda, labeller = label_both) 
      
      gp <- ggplotly(plot) 
      # Move the axis labels further away from plot
      gp[['x']][['layout']][['annotations']][[1]][['y']] <- -0.1 # x axis label
      gp[['x']][['layout']][['annotations']][[2]][['x']] <- -0.1 # y axis label
      gp %>% layout(margin = list(l = 75, b = 75))
      
    }
  })
  
  output$sweepPlotBars <- renderPlotly({
    if (!is.null(run$sweep_results)){
      df <- bind_rows(run$sweep_results)
      # Convert to long format (note: this line needs to match the extra columns added in the runFCMSweepAction function above
      df <- tidyr::pivot_longer(df, !c(timestep, infer_type, h, lambda, init), names_to = "concept", values_to = "value") 
      plot <- ggplot(df %>% filter(timestep==max(timestep)), aes(x = concept, y = value, colour = concept)) + theme_minimal() + 
        geom_col() + facet_wrap(facets=c(isolate(input$sweepingParam)), labeller = label_both) #facet_grid(h ~ lambda, labeller = label_both) 
      
      gp <- ggplotly(plot) 
      # Move the axis labels further away from plot
      gp[['x']][['layout']][['annotations']][[1]][['y']] <- -0.1 # x axis label
      gp[['x']][['layout']][['annotations']][[2]][['x']] <- -0.1 # y axis label
      gp %>% layout(margin = list(l = 75, b = 75))
      
    }
  })
  
  # Collate scenario comparison data for all scenarios selected  -------------------- 
  scenarioComparison <- eventReactive(
    c(input$launchScenarioView, input$resetScenarios, input$startModeling),{ # only evaluate when button is pressed and/or things are reset
      if (length(input$scenariosToPlot)>0 && length(scenarios$results)>0){
        df <- bind_rows(scenarios$results[input$scenariosToPlot], .id = "scenario_name") # single brackets to preserve names
        # Convert to long format (column names: scenario name, timestep, concept, value)
        df <- tidyr::pivot_longer(df, !c(timestep, scenario_name, h, lambda, infer_type, init), names_to = "concept", values_to = "value") 
        # Extract baseline only data and join it back to the relevant rows (join on timestep, concept, infer_type, params)
        baseline <- df %>% filter(grepl("^baseline",scenario_name)) %>% # Scenario name starts with baseline
          select(timestep, concept, baseline=value, infer_type, h, lambda, init) # Select relevant columns
        joined_df <- df %>% left_join(baseline, by=c("timestep", "concept", "infer_type", "h", "lambda", "init")) # Join original with extracted baseline
        joined_df <- joined_df %>% 
          mutate(difference = (value-baseline)) %>% 
          mutate(percent_diff = difference/abs(baseline)*100) %>%
          replace_na(list(difference = 0, percent_diff = 0))  # replace NAs with 0s so the legend colours stay the same (workaround)
        # print(joined_df)
        return(joined_df)
      } else {
        NULL
      }
    }
  )
  
  # Notification when scenario comparison data saved -------------------- 
  observeEvent(input$saveScenarioData,{
    saveRDS(scenarioComparison(), file = input$scenDataFileName)
    showNotification(
      ui = paste("File saved:", input$scenDataFileName),
      duration = 2, 
      closeButton = TRUE,
      type = "message"
    )
  })
  
  output$selectScenarioYVar <- renderUI({
    selectInput("scenarioPlotY", "Y Value Plotted", 
                choices = c("Concept value" = "value", 
                            "Difference from baseline scenario" = "difference", 
                            "Percentage difference from baseline" = "percent_diff"))
  })
  
  output$scenarioPlotWarning <- renderText({
    if (!is.null(scenarioComparison())){
      df <- scenarioComparison()
      missingBaseline <- df %>% filter(is.na(baseline)) %>% select(scenario_name) %>% unique()
      if (nrow(missingBaseline)>0){
        paste("WARNING - Scenarios missing baseline values:", missingBaseline, collapse =  ", ")
      } else {
        ""
      }
    }
  })
  
  # Output plot of scenario comparisons (line) -------------------- 
  output$scenarioPlot <- renderPlotly({
    if (!is.null(scenarioComparison())){
      plot <- ggplot(scenarioComparison(), aes_(x = ~timestep, y = as.name(input$scenarioPlotY), colour = ~scenario_name)) + 
        geom_line(show.legend = TRUE) + facet_wrap(vars(concept)) + theme_minimal() + 
        theme(panel.spacing.y = unit(2, "lines")) 
      
      gp <- ggplotly(plot) 
      # Move the axis labels further away from plot
      gp[['x']][['layout']][['annotations']][[1]][['y']] <- -0.1 # x axis label
      gp[['x']][['layout']][['annotations']][[2]][['x']] <- -0.1 # y axis label
      gp %>% layout(margin = list(l = 75, b= 100))
    }
  })
  
  # Output plot of scenario comparisons (bar) -------------------- 
  output$scenarioPlotBars <- renderPlotly({
    if (!is.null(scenarioComparison())){
      plot <- ggplot(scenarioComparison() %>% filter(timestep==max(timestep)), aes_(x = ~scenario_name, y = as.name(input$scenarioPlotY), fill = ~scenario_name)) + 
        geom_col(show.legend = TRUE) + facet_wrap(vars(concept)) + theme_minimal() + 
        theme(panel.spacing.y = unit(2, "lines")) + 
        theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
              axis.ticks.x=element_blank()) + 
        geom_hline(yintercept = 0, color = "black")
      
      gp <- ggplotly(plot) 
      # Move the axis labels further away from plot
      # gp[['x']][['layout']][['annotations']][[1]][['y']] <- -0.1 # x axis label
      gp[['x']][['layout']][['annotations']][[2]][['x']] <- -0.1 # y axis label
      gp %>% layout(margin = list(l = 75, b= 100))
    }
  })
  
}) #end: shinyServer
