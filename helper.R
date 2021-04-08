# helper.R
# Author: Patricia Angkiriwang, University of British Columbia - 2019-2021
# with code initially adapted from open-source R Shiny app by Brian Gregor, Oregon Systems Analytics LLC

# Note: the templates folder in the models folder needs to exist

# === INITIALIZING, LOADING, AND SAVING MODELS ==================

#----------------------
#Initialize a New Model
#----------------------
#' Initialize a new model.
#'
#' \code{initializeNewModel} initializes a new model by creating a directory
#' with the model name. Creates and saves a model status list.
#'
#' This function initializes a new model with the given model name. It does this
#' by creating a directory with the model name and a list to store the model
#' status. This list is saved in the model directory in JSON format and is
#' also returned by the function. The status list contains the name of the
#' model, the date and time is was created, and the date
#' and time it was last edited (same as creation time).
#'
#' @param ModelsDir a string identifying the path to the models folder in which
#' the model is located.
#' @param ModelName a string identifying the model name.
#' @param Author a string identifying the author's name.
#' @return a list containing values for name, parent, created, and lastedit.
#' @importFrom jsonlite toJSON
#' @export
initializeNewModel <- function(modelName, authorName) {
  if (modelName == ""){
    return(NULL)
  }
  #Create directory for model
  newDir <-  file.path("models", modelName)
  dir.create(newDir)
  #Create and save a status list
  attribution <- 
    paste0("Model: ", modelName, " | Author: ", authorName, " | Created: ", as.character(Sys.time()))
  status_ls <- list(name = modelName,
                    created = as.character(Sys.time()),
                    lastedit = as.character(Sys.time()),
                    attribution = attribution,
                    notes = character(0))
  
  writeLines(toJSON(status_ls), file.path(newDir, "status.json"))
  #Copy and save the concept and relations template files
  file.copy(
    file.path("models/templates/concepts.json"), newDir
  )
  file.copy(
    file.path("models/templates/relations.json"), newDir
  )
  #Create scenarios directory if does not exist
  scenarioPath <- file.path(newDir, "scenarios")
  if (!dir.exists(scenarioPath)) {
    dir.create(scenarioPath)
  }
  #Create analysis directory if does not exist
  analysisPath <- file.path(newDir, "analysis")
  if (!dir.exists(analysisPath)) {
    dir.create(analysisPath)
  }
  #Return the status list
  status_ls
}


#--------------------------------#
# Load Model Status File
#--------------------------------#
#' Load the model status file for a model.
#'
#' \code{loadModelStatus} reads a model status file and returns a list
#' containing the status information.
#'
#' This function reads the model status JSON file for a specified model and 
#' creates a list containing the model status information.
#'
#' @param modelName a string representation of the model name.
#' @return a list containing values for name, parent, created, and lastedit.
#' @export
loadModelStatus <- function(modelName, authorName = NULL){
  dir <-  file.path("models", modelName)
  status_ls <- as.list(fromJSON(file.path(dir, "status.json")))
  if (!is.null(authorName)) {
    attribution <- 
      paste0("Model: ", modelName, " | Author: ", authorName, " | Edited: ", as.character(Sys.time()))
    status_ls$attribution <- c(attribution, status_ls$attribution)
    status_ls$notes <- status_ls$notes
  }
  status_ls
}

#--------------------------------#
# Load Model Concepts File
#--------------------------------#
#' Load the concept file for a model.
#'
#' \code{loadModelConcepts} reads the file that contains model concept information
#' and returns a data frame representation.
#'
#' This function reads the model concept file for a specified model and returns
#' a data frame containing the information.
#'
#' @param modelName a string representation of the model name.
#' @return a data frame containing the model concept information.
#' @export
loadModelConcepts <- function(modelName){
  dir <-  file.path("models", modelName)
  fromJSON(file.path(dir, "concepts.json"))
}

#--------------------------------#
# Load Model Relations File
#--------------------------------#
#' Load the relations file for a model.
#'
#' \code{loadModelRelations} reads the file that contains model relations
#' information and returns a data frame representation.
#'
#' This function reads the model relations file for a specified model and 
#' returns a data frame containing the information.
#'
#' @param modelName a string representation of the model name.
#' @return a data frame containing the model relations information.
#' @export
loadModelRelations <- function(modelName){
  dir <-  file.path("models", modelName)
  fromJSON(file.path(dir, "relations.json"), simplifyDataFrame = FALSE)
}

#--------------------------------#
# Save All Model Components
#--------------------------------#
#' Saves all the model components as JSON files.
#'
#' \code{saveModel} saves the model status, model concepts, and model relations
#' as JSON files.
#'
#' Models are composed of 3 objects: a model status list, a model concepts
#' data frame, and a model relations data frame. This function saves these 
#' objects as JSON-formatted files.
#'
#' @param modelData a model.
#' @return no return value. Has side effect of saving the model status list,
#' model concepts data frame, and model relations data frame as JSON-formatted
#' files.
#' @export
saveModel <- function(modelData) {
  modelName <- modelData$status$name
  dir <- file.path("models", modelName)
  writeLines(prettify(toJSON(modelData$status, auto_unbox=TRUE)), file.path(dir, "status.json"))
  writeLines(prettify(toJSON(modelData$concepts, auto_unbox=TRUE)), file.path(dir, "concepts.json"))
  writeLines(prettify(toJSON(modelData$relations, auto_unbox=TRUE)), file.path(dir, "relations.json"))
}


# === EDITING MODELS ==================
#----------------------------------#
# Format a Concept Table for Display
#----------------------------------#
#' Formats a concept table to be displayed in the GUI.
#'
#' \code{formatConceptTable} formats a concept data frame to be displayed as a
#' table in the GUI.
#'
#' The GUI summarizes information about model concepts in a table. Not all of 
#' the concept data needs to be shown and some of the data is difficult to
#' show in table form. This function extracts and formats the concept data that
#' is to be displayed in a table. 
#'
#' @param concepts_df a data frame containing the concepts data.
#' @param export a boolean indicating whether or not to format the table for export (e.g. save to another file)
#' @return a data frame containing the concepts data to be shown in a table.
#' @export 
formatConceptTable <- function(concepts_df,export=FALSE) {
  if (export==FALSE){
    df <- data.frame(Name = concepts_df$name,
                     ID = concepts_df$concept_id,
                     stringsAsFactors = FALSE)
  } else{ # untested; written 2019/05/20
    df <- data.frame(Name = concepts_df$name,
                     ID = concepts_df$concept_id,
                     #"Group or Type" = concepts_df$group,
                     Description = concepts_df$description,
                     check.names = FALSE,
                     stringsAsFactors = FALSE)
  }
  return(df)
}


#-----------------------------------#
# Extract vector of variables from relations list
#-----------------------------------#
#' Extracts a vector of variables from the relations list
#' @param relations_ls a list containing the relations data.
#' @param var a list containing the relations data.
#' @param level where in the nested list is this variable located? Is it a property of a causal variable, a group of causal vars, or an affected variable?
#' @return a vector containing the data of interest 
#' @export
extract_rel <- function(relations_ls, var, level = "causal_link"){
  switch(level, 
         "causal_link" = unlist(lapply(relations_ls, function(a){lapply(a$affected_by, function(x){sapply(x$links, "[[", var)})})),
         "causal_group" = unlist(lapply(relations_ls, function(a){lapply(a$affected_by, function(x){rep(x[[var]], length(x$links))})})),
         "affected" = unlist(lapply(relations_ls, function(a){rep(a[[var]], sum(sapply(a$affected_by, function(x){length(x$links)})))}))
  )
}

#----------------------------------#
# Format a Relation Table for Display
#----------------------------------#
#' Formats a relation table to be displayed in the GUI or for export.
#'
#' \code{formatRelationTable} formats a concept data frame to be displayed as a
#' table in the GUI.
#'
#' The GUI summarizes information about model concepts in a table. Not all of 
#' the concept data needs to be shown and some of the data is difficult to
#' show in table form. This function extracts and formats the concept data that
#' is to be displayed in a table. 
#'
#' @param relations_ls a list containing the relations data.
#' @return a data frame containing the relations data to be shown in a table.or exported
#' @export
formatRelationTable <- function(relations_ls,concepts_df,export=FALSE,use.full.names=TRUE) {
  name_key <- concepts_df$name
  names(name_key) <- concepts_df$concept_id
  
  causal_vars <- extract_rel(relations_ls, "concept_id")
  affects_vars <- extract_rel(relations_ls, "concept_id", level="affected")
  rel_ks <- extract_rel(relations_ls, "k", level="affected")
  rel_types <- extract_rel(relations_ls, "type", level="causal_group")
  rel_group <- unlist(lapply(relations_ls, function(a){mapply(function(x, y){rep(y, length(x$links))}, a$affected_by, seq_along(a$affected_by))}))
  if (use.full.names){
    df <- data.frame(From = name_key[causal_vars],
                     To = name_key[affects_vars],
                     k = rel_ks,
                     Grouping = rel_group,
                     Type = rel_types,
                     Direction = extract_rel(relations_ls, "direction"),
                     Weight = extract_rel(relations_ls, "weight"),
                     stringsAsFactors = FALSE, row.names = NULL)
  } else {
    df <- data.frame(From = causal_vars,
                     To = affects_vars,
                     k = rel_ks,
                     Grouping = rel_group,
                     Type = rel_types,
                     Direction = extract_rel(relations_ls, "direction"),
                     Weight = extract_rel(relations_ls, "weight"),
                     stringsAsFactors = FALSE, row.names = NULL)
  }
  if (export){ # untested; written 2019/05/21
    df$Description <- extract_rel(relations_ls, "description", level="causal_group")
  }
  return(df)
}


#----------------------------------------------#
# Make an Adjacency Matrix from a Relations List
#----------------------------------------------#
#' Creates an adjacency matrix 
#' \code{makeAdjacencyMatrix} creates a set  of adjacency matrices from a relations list
#' 
#' This function creates a list of adjacency matrices from a relations list. The
#' adjacency matrix is a square matrix with as many rows and columns as the
#' number of concepts in the model. The rows and columns are named with the
#' concept variable names. The rows represent the causal side of the
#' relationship and the columns represent the affected side. 
#' 
#' @param relations_ls a list of relations in the model (usually model$relations)
#' @param c_ids a vector of concepts in the system (usually model$concepts$ID)
#' @return a list of adjacency matrices, one corresponding to each edge variable, and additional ones like "type" and "rel_group" that identify a group of related links
#' @export
makeAdjacencyMatrix <- function(relations_ls, c_ids) {
    # Get all the variables associated with the edges 
    # (names of columns in any links data frame except "concept_id")
    if (length(relations_ls) > 0){
      edge_vars <- names(relations_ls[[1]][["affected_by"]][[1]]$links[[1]])
      edge_vars <- edge_vars[edge_vars != "concept_id"]
    } else {
      edge_vars <- c()
    }
    
    # Make a list of empty matrices, one for each of the edge variables in the data, plus the type of relationship (saved in "type" for each set of links)
    mx <- array(NA, dim = c(length(c_ids), length(c_ids)), 
                dimnames = list(c_ids, c_ids))
    mx_list <- rep(list(mx), length(edge_vars) + 2)
    names(mx_list) <- c(edge_vars, "type", "rel_group")
    
    # For number of relations present, find what is affecting it and for each edge variable
    # enter its value in the corresponding adjacency matrices
    if (length(relations_ls)>0){
      for (i in 1:length(relations_ls)){
        id <- relations_ls[[i]]$concept_id
        infl_list <- relations_ls[[i]]$affected_by
        # Loop over all sets of links (Note: tfn data considers only 1 set)
        for (j in 1:length(infl_list)){ 
          links <- infl_list[[j]]$links # list of links in that set of influences
          infls <- sapply(links, function(x) x$concept_id) 
          for (e in edge_vars){
            mx_list[[e]][infls, id] <- sapply(links, '[[', e)
          }
          mx_list[["type"]][infls, id] <- infl_list[[j]]$type # Corresponding type for that set of links (and/or etc.)
          mx_list[["rel_group"]][infls, id] <- j
        }
      }
    }
    
    return(mx_list)
}


#------------------------------#
# Initialize New Relations Entry
#------------------------------#
#' Initialize a initial relations entry for a new concept
#' 
#' \code{initRelationsEntry} creates a initial relations entry for concept
#' that is being added to the model
#' 
#' This function creates an initial relations entry data for a concept. This
#' entry data has the concept variable name and empty fields for all other data
#' items. The server script calls this function when a concept is created and
#' adds the resulting entry to the relations table.
#' 
#' @param var a string representation of the concept variable name
#' @return a data frame which includes all the mandatory relations fields with
#' the name field populated with the concept variable name and all other fields
#' populated with empty fields
#' @export
initRelationsEntry <- function(var){
  list(name = var,
       affects = list(data.frame(
         variable = "",
         direction = "",
         weight = "",
         description = ""
       ))
  )
}

#---------------------------------------------------------------#
# Define function to extract weight and direction
#---------------------------------------------------------------#
#' Calculate a weighted adjacency matrix from a list of matrices
#'
#' \code{adjMatrixCalc} creates an adjacency matrix that uses both (sign from direction + magnitude from weight) 
#' to get a single numerical value for each edge.
#' 
#' @param adj_mx_list a list of adjacency matrices, which includes direction and weight
#' @param vals a vector that defines what numbers the linguistic values (low/highs) are converted to
#' e.g. vals <- c(VL = 0.1, L = 0.25, ML = 0.375, M = 0.5, MH = 0.675, H = 0.75, VH = 0.95) 
#' @export
adjMatrixCalc <- function(adj_mx_list, vals){
  if (!("weight" %in% names(adj_mx_list)) || !("direction" %in% names(adj_mx_list))){
    warning("Weights and direction are required to generate a weighted adjacency matrix. Returning adj_mx_list$type.")
    return(adj_mx_list$type)
  }
  signs <- c(Positive = 1, Negative = -1)    
  adj_mx <- apply(adj_mx_list$weight, 2, function(x) vals[x]) * apply(adj_mx_list$direction, 2, function(x) signs[x])
  adj_mx[is.na(adj_mx)] <- 0 # replace NAs with 0s 
  rownames(adj_mx) <- colnames(adj_mx)
  
  return(adj_mx)
}

#---------------------------------------------------------------#
# Define function to create a dot file for plotting with GraphViz
#---------------------------------------------------------------#
#' Create a DOT file for GraphViz
#'
#' \code{makeDotFile} create and save a dot file to be displayed by GraphViz
#'
#' This function writes out a dot file to be rendered using GraphViz.
#' Taken from FSDM R Shiny app by Brian Gregor
#'
#' @param relations_ls a list of model relations.
#' @param concepts_df a data frame with model concepts.
#' @param RowGroup a string identifying the names of the name of the group that
#' selects rows from the relationship table to plot. The default 'All' selects
#' all the rows.
#' @param ColGroup a string identifying the name of the group that selects
#' columns from the relationship table to plot. The default 'All' selects all
#' the columns.
#' @param orientation a string identifying the GraphViz layout orientation
#' ('Portrait' or 'Landscape').
#' @param rankdir a string identifying the graph orientation:
#' 'TB' for top to bottom, 'LR' for left to right.
#' @param shape a string identifying the shape of the graph nodes (e.g. 'box').
#' @param Show a string identifying how to label the edges. The default value
#' "label" results in showing the fuzzy label (e.g. VL, L, M, H, VH). The
#' alternative, "value", results in showing the equivalent numeric value.
#' @return A string specification of a DOT.
#' @export
makeDot <-
  function(Model, RowGroup = "All", ColGroup = "All",
           orientation = "Portrait", rankdir = "Top-to-Bottom", shape = "box",
           Show = "label",
           Conjunctions = TRUE)
  {
    concepts_df <- Model$concepts
    relations_ls <- Model$relations
    weight_vals <- Model$weight_vals
    
    # Name to variable key to use for labels
    name_key <- concepts_df$concept
    names(name_key) <- concepts_df$concept_id
    
    #Make matrices of relations and labels
    Cn <- concepts_df$concept_id
    adj_mx_ls <- makeAdjacencyMatrix(relations_ls, Cn)
    adj_mx_ls$weight_num <- adjMatrixCalc(adj_mx_ls, weight_vals)
    #Create row and column indices for selected row and column groups
    if (RowGroup == "All") {
      Cr <- Cn
    } else {
      Cr <- Cn[concepts_df$group %in% RowGroup]
    }
    if (ColGroup == "All") {
      Cc <- Cn
    } else {
      Cc <- Cn[concepts_df$group %in% ColGroup]
    }
    #Select relations and labels matrices for selected rows and columns
    concepts_to_plot <- unique(Cr)
    rels_to_plot <- adj_mx_ls$weight_num[Cr,Cc]
    labels_to_plot <- adj_mx_ls$weight[Cr,Cc]
    types_to_plot <- adj_mx_ls$type[Cr,Cc]
    
    #Update Cr and Cc and identify unique concepts
    Cr <- rownames(rels_to_plot)
    Cc <- colnames(rels_to_plot)
    #Convert rankdir argument
    if (rankdir == "Top-to-Bottom") rankdir <- "TB"
    if (rankdir == "Left-to-Right") rankdir <- "LR"
    #Make DOT data
    Dot_ <-
      paste("digraph {\n orientation =", orientation, ";\n rankdir =", rankdir, ";\n")
    for (concept in concepts_to_plot) {
      c_ <- gsub("\\-","\\_",concept) # replace hyphens with underscores for proper grViz syntax
      l_ <- gsub("([[:punct:]])", "\\\\\\1", name_key[concept]) # escape punctuation characters in node labels
      Dot_ <- paste(Dot_, c_, "[ shape =", shape, ", label =\"", l_,"\"];\n")
    }
    for (cr in Cr) {
      for (cc in Cc) {
        Value <- rels_to_plot[cr,cc]
        if (!is.na(Value)) {
          if (Show == "label") {
            Label <- labels_to_plot[cr,cc]
          } else {
            Label <- Value
          }
          if (Conjunctions){
            Label <- paste(Label, toupper(types_to_plot[cr,cc]), sep="\n")
          }
          if (Value != 0) {
            if (Value > 0) {
              Dot_ <- paste0(Dot_, cr, " -> ", cc, "[ label='", Label, "'];\n")
            } else {
              Dot_ <-
                paste0(
                  Dot_, cr, " -> ", cc, "[ color=red, fontcolor=red, style=dashed, label='", Label, "'];\n"
                )
            }
          }
        }
      }
    }
    Dot_ <- paste(Dot_, "}")
    #Return the resulting DOT description
    Dot_
  }


# === RUNNING THE MODEL ==================
#---------------#
# Run simulation
#---------------#
run_model <- function(model, params, constraints, encode=FALSE){
  relations_ls <- model$relations
  c_ids <- model$concepts$concept_id
  adj_mx_ls <- makeAdjacencyMatrix(relations_ls, c_ids)
  adj_mx_ls$weight_num <- adjMatrixCalc(adj_mx_ls, model$weight_vals)
  
  if (length(constraints)>0){
    scen <- list(var = names(constraints),
                 val = constraints)
  } else{
    scen <- list(var = NULL, val = NULL)
  }
  
  run <- fcm.run(adj_mx_ls$weight_num, adj_mx_ls$type, adj_mx_ls$rel_group, 
                 cn = c_ids, iter = params$iter, k = params$k, init = params$init, 
                 infer_type = params$infer_type, h = params$h, lambda = params$lambda,
                 set.concepts = scen$var, set.values = scen$val)
  
  if (encode){ # Do we want to appent parameter info in the data frame?
    run <- run %>% 
      mutate(infer_type = params$infer_type,
             h = params$h,
             lambda = params$lambda,
             init = params$init,
             timestep = seq.int(params$iter))
  }
  return(run)
}

#------------#
# Run multiple parameters 
#------------#
run_parameter_sweep <- function(model, params, constraints, varying_params = c("lambda")){
  sweep <- list(params = NULL, results = NULL, constraints = NULL)
  
  # Store information in sweep_results and sweep_params (keep independent from normal runs)
  N <- prod(sapply(varying_params,length)) # calculate how many runs there will be
  sweep$params <- vector("list",  N) # create new list that will contain all runs (permutations) of sweeps
  sweep$results <- vector("list",  N)
  sweep$constraints <- constraints
  
  n <- 1
  for (p in 1:length(varying_params)){
    pn = names(varying_params)[p]
    pvals = varying_params[[p]]

    for (i in 1:length(pvals)){
      params_run = replace(params, pn, pvals[i]) # store all parameters here, using the value in the parameter sweep
      res <- run_model(model, params_run, constraints, encode = TRUE)
      sweep$params[[n]] <- params_run
      sweep$results[[n]] <- res
      n <- n + 1
    }
  }
  return(sweep)
}

#---------------#
# Create string from current parameters and constraints
#---------------#
scenarioNameString <- function(params, constraints_list){
  if (params[["infer_type"]]=="linear"){
    constr <- paste0("h",params[["h"]],"-linear","-init",params[["init"]])
  }
  prm <- paste0(params[["infer_type"]],"-h",params[["h"]],"-L",params[["lambda"]],"-i",params[["init"]])
  if (length(constraints_list)>0){
    constr <- paste(paste(names(constraints_list),constraints_list,sep="="),collapse="-")
  } else {
    constr <- "baseline"
  }
  
  return(paste(constr,prm,sep="_"))
  
}

#----------------#
# Run multiple constraints 
#----------------#

run_auto_scenarios <- function(model, params, conceptsToConstrain, lowVal = 0, highVal = 1){
  # Create new list that will contain all runs (high and low for each concept selected)
  clampRunResults <- vector("list",  length(conceptsToConstrain) * 2) 
  newScenarios <- list(results = list(), constraints = list(), parameters = list())
  
  for (cn in conceptsToConstrain){
    constraints <- c()
    
    # Baseline
    results <- run_model(model, params, constraints, encode = TRUE)
    scenName <- scenarioNameString(params, constraints)
    
    newScenarios$results[[scenName]] <- results 
    newScenarios$constraints[[scenName]] <- "none"
    newScenarios$parameters[[scenName]] <- params
    
    # Low scenario
    constraints[cn] <- lowVal
    results <- run_model(model, params, constraints, encode = TRUE)
    scenName <- scenarioNameString(params, constraints)
    
    newScenarios$results[[scenName]] <- results 
    newScenarios$constraints[[scenName]] <- constraints
    newScenarios$parameters[[scenName]] <- params
    
    # High scenario
    constraints[cn] <- highVal
    
    results <- run_model(model, params, constraints, encode = TRUE)
    scenName <- scenarioNameString(params, constraints)
    
    newScenarios$results[[scenName]] <- results 
    newScenarios$constraints[[scenName]] <- constraints
    newScenarios$parameters[[scenName]] <- params
  }
    return(newScenarios)
}

# DATA CLEANING =================
#------------#
# Parse scenario data and filter
#------------#
parse_filter_scenarios <- function(results_list, scenario_names = NULL){
    if (is.null(scenario_names)){
      df <- bind_rows(results_list, .id = "scenario_name") # single brackets to preserve names
    } else {
      df <- bind_rows(results_list[scenario_names], .id = "scenario_name") # single brackets to preserve names
    }
    # Convert to long format (column names: scenario name, timestep, concept, value)
    df <- tidyr::pivot_longer(df, !c(timestep, scenario_name, h, lambda, infer_type, init), names_to = "concept", values_to = "value") %>%
      mutate(is_baseline = grepl("^baseline",scenario_name))
    
    # Extract baseline only data and join it back to the relevant rows (join on timestep, concept, infer_type, params)
    baseline <- df %>% filter(is_baseline) %>% # Scenario name starts with baseline
      select(timestep, concept, baseline=value, infer_type, h, lambda, init) # Select relevant columns
    joined_df <- df %>% left_join(baseline, by=c("timestep", "concept", "infer_type", "h", "lambda", "init")) # Join original with extracted baseline
    joined_df <- joined_df %>% 
      mutate(difference = (value-baseline)) %>% 
      mutate(percent_diff = difference/abs(baseline)*100) %>%
      replace_na(list(difference = 0, percent_diff = 0))  # replace NAs with 0s so the legend colours stay the same (workaround)
    return(joined_df)
}

#------------#
# Utility function to take output of function above (joined_df) and create two types of data points (baseline, modified) for each scenario
#------------#
tidy_slope_graph <- function(joined_df){
  new_df <- joined_df %>% select(-c(difference, percent_diff)) %>% filter(!is_baseline) %>%
    tidyr::pivot_longer(c(baseline, value), names_to = "scenario_type", values_to = "value") #%>%
    # mutate(scenario_type = if_else(scenario_type=="value", "modified scenario", scenario_type))
    
    return(new_df)
}

# PLOTS =========================
#------------#
# Plot results
#------------#
plot_time_series <- function(df, infer_type = "sigmoid-exp"){
  # df$timestep <- 1:nrow(df) # now taken care of beforehand)
  df <- tidyr::pivot_longer(df, !c(timestep), names_to = "concept", values_to = "value")
  if (infer_type == "sigmoid-exp"){
    ylims <- c(0,1)
  } else {
    ylims <- c(-1,1)
  }
  plot <- plot_ly(df, x = ~timestep, y = ~value) %>%
    add_lines(linetype = ~concept) %>%
    layout(yaxis = list(range = ylims))
  
  return(plot)
}

#------------#
# Plot results for parameter sweep
#------------#
plot_facet_sweep <- function(df_list, sweepingParams = c("lambda")){
  df <- bind_rows(df_list)
  # Convert to long format (note: this line needs to match the extra columns added in the run_model function above
  df <- tidyr::pivot_longer(df, !c(timestep, infer_type, h, lambda, init), names_to = "concept", values_to = "value") 
  plot <- ggplot(df, aes(x = timestep, y = value, colour = concept)) + theme_minimal() + 
    geom_line() + facet_wrap(facets=sweepingParams, labeller = label_both) #facet_grid(h ~ lambda, labeller = label_both) 
  
  gp <- ggplotly(plot) 
  # Move the axis labels further away from plot
  gp[['x']][['layout']][['annotations']][[1]][['y']] <- -0.1 # x axis label
  gp[['x']][['layout']][['annotations']][[2]][['x']] <- -0.1 # y axis label
  gp %>% layout(margin = list(l = 75, b = 75))
}


plot_facet_sweep_bars <- function(df_list, sweepingParams = c("lambda")){
  df <- bind_rows(df_list)
  # Convert to long format (note: this line needs to match the extra columns added in the runFCMSweepAction function above
  df <- tidyr::pivot_longer(df, !c(timestep, infer_type, h, lambda, init), names_to = "concept", values_to = "value") 
  plot <- ggplot(df %>% filter(timestep==max(timestep)), aes(x = concept, y = value, fill = concept)) + theme_minimal() + 
    geom_col() + facet_wrap(facets=sweepingParams, labeller = label_both) + #facet_grid(h ~ lambda, labeller = label_both) 
    theme(axis.text.x=element_blank()) 
  
  gp <- ggplotly(plot) 
  # Move the axis labels further away from plot
  gp[['x']][['layout']][['annotations']][[1]][['y']] <- -0.1 # x axis label
  gp[['x']][['layout']][['annotations']][[2]][['x']] <- -0.1 # y axis label
  gp %>% layout(margin = list(l = 75, b = 75))
}

#--------#
# Plot scenario comparisons
#--------#

plot_comparison <- function(df, yVar = "value"){
  
  plot <- ggplot(df, aes_(x = ~timestep, y = as.name(yVar), colour = ~scenario_name)) + 
    geom_line(show.legend = TRUE, size=1.5) + facet_wrap(vars(concept)) + theme_minimal() + 
    theme(panel.spacing.y = unit(2, "lines")) + 
    labs(title = "Concepts under different scenarios over time", colour = "Scenario")
  
  return(plot)
  
  ## Commented out plotly lines 2021/03/17
  # gp <- ggplotly(plot) 
  # # Move the axis labels further away from plot
  # gp[['x']][['layout']][['annotations']][[1]][['y']] <- -0.1 # x axis label
  # gp[['x']][['layout']][['annotations']][[2]][['x']] <- -0.1 # y axis label
  # gp %>% layout(margin = list(l = 75, b= 100))
}

plot_comparison_bars <- function(df, yVar = "value"){
  plot <- ggplot(df %>% filter(timestep==max(timestep)), aes_(x = ~scenario_name, y = as.name(yVar), fill = ~scenario_name)) + 
    geom_col(show.legend = TRUE) + facet_wrap(vars(concept)) + theme_minimal() + 
    theme(panel.spacing.y = unit(2, "lines")) + 
    theme(axis.text.x=element_blank()) + 
    labs(title = "Concept 'equilibrium' states under different scenarios", 
         fill = "Scenario", x = "concepts at end of simulation (different scenarios)") +
    geom_hline(yintercept = 0, color = "black")
  
  return(plot)
  
  # gp <- ggplotly(plot) 
  # # Move the axis labels further away from plot
  # # gp[['x']][['layout']][['annotations']][[1]][['y']] <- -0.1 # x axis label
  # gp[['x']][['layout']][['annotations']][[2]][['x']] <- -0.1 # y axis label
  # gp %>% layout(margin = list(l = 75, b= 100))
}

# Later: fix colours? http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/

plot_comparison_slope <- function(df){
  plot <- ggplot(tidy_slope_graph(df) %>% filter(timestep==max(timestep)), 
                 aes(x = scenario_type, y = value, colour = scenario_name, group=scenario_name)) +
    geom_line(size = 2) + geom_point(size = 4) + facet_wrap(vars(concept)) + theme_minimal() +
    theme(panel.spacing.y = unit(2, "lines")) +
    labs(title = "Concept 'equilibrium' states under different scenarios",
         fill = "Scenario", x = "concepts at end of simulation (different scenarios)")

  return(plot)
}
