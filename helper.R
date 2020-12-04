#helper.R
#Author: Patricia Angkiriwang, University of British Columbia; based on code by Brian Gregor, Oregon Systems Analytics LLC
#Copyright: 2016, Oregon Department of Transportation 2016
#Copyright: 2019, Brian Gregor
#Modifications copyright: 2019, Patricia Angkiriwang
#License: Apache 2

# note: the templates folder in the models folder needs to exist


#------------------------------------------#
# INITIALIZING, LOADING, AND SAVING MODELS #
#------------------------------------------#
#--------------------------------#
# Initialize a New Model
#--------------------------------#
#' Initialize a new model.
#'
#' \code{initializeNewModel} initializes a new model by creating a directory
#' with the model name. Creates and saves a model status list.
#'
#' This function initializes a new model with the given model name. It does this
#' by creating a directory with the model name and a list to store the model
#' status. This list is saved in the model directory in JSON format and is
#' also returned by the function. The status list contains the name of the 
#' model, the parent (none), the date and time is was created, and the date
#' and time it was last edited (same as creation time).
#'
#' @param ModelName a string representation of the model name.
#' @return a list containing values for name, parent, created, and lastedit.
#' @export
initializeNewModel <- function(ModelName, Author) {
  #Create directory for model
  NewDir <- file.path("./models", ModelName)
  dir.create(NewDir)
  #Create and save a status list
  Attribution <- 
    paste0("Model: ", ModelName, "   Author: ", Author, "   Created: ", as.character(Sys.time()))
  status_ls <- list(name = ModelName,
                    parent = "none",
                    created = as.character(Sys.time()),
                    lastedit = as.character(Sys.time()),
                    attribution = Attribution,
                    notes = character(0))
  writeLines(toJSON(status_ls, auto_unbox=TRUE), file.path(NewDir, "status.json"))
  #Copy and save the concept and relations template files
  CopyDir <- "./models/templates"
  FilesToCopy_ <- file.path(
    CopyDir, c("concepts.json", "relations.json")
  )
  file.copy(FilesToCopy_, NewDir, recursive = TRUE)
  #Create scenarios directory if does not exist
  ScenarioPath <- file.path(NewDir, "scenarios")
  if (!dir.exists(ScenarioPath)) {
    dir.create(ScenarioPath)
  }
  #Create analysis directory if does not exist
  AnalysisPath <- file.path(NewDir, "analysis")
  if (!dir.exists(AnalysisPath)) {
    dir.create(AnalysisPath)
  }
  #Return the status list  
  status_ls
}

#--------------------------------#
# Initialize a New Model by Copying an Existing Model
#--------------------------------#
#' Initialize a new model by copying an existing model.
#'
#' \code{initializeCopyModel} initializes a new model by creating a directory
#' with the model name and copying the contents of an existing model into that
#' directory. Creates and saves a model status list.
#'
#' This function initializes a new model with the given model name from an
#' existing model. It does this by creating a directory with the model name and
#' copying the model files for an existing model into it. It creates a new
#' model status list which identifies the name of the new model and the name
#' of the parent model it is a copy of. The status list also identifies the 
#' date and time is was created. The function can be used to copy only the 
#' model or all the scenarios as well as the model.
#'
#' @param ModelName a string representation of the model name.
#' @param CopyModelName a string representation of the name of the model to copy.
#' @param CopyScenarios a logical to determine whether to copy the model 
#' scenarios.
#' @return a list containing values for name, parent, created, and lastedit.
#' @export
initializeCopyModel <- function(ModelName, CopyModelName, Author, CopyScenarios = FALSE) {
  NewDir <- file.path("./models", ModelName)
  dir.create(NewDir)
  CopyDir <- file.path("./models", CopyModelName)
  if(CopyScenarios) {
    FilesToCopy_ <- file.path(
      CopyDir, c("status.json", "concepts.json", "relations.json", "scenarios")
    )
    file.copy(FilesToCopy_, NewDir, recursive = TRUE)
    Sc <- dir(file.path(NewDir, "scenarios"))
    for (sc in Sc) {
      ScenDir <- file.path(NewDir, "scenarios", sc)
      status_ls <- fromJSON(paste0(ScenDir, "/status.json"))
      status_ls$model <- ModelName
      writeLines(toJSON(status_ls, auto_unbox=TRUE), file.path(ScenDir, "status.json"))
    }
  } else {
    FilesToCopy_ <- file.path(
      CopyDir, c("status.json", "concepts.json", "relations.json")
    )
    file.copy(FilesToCopy_, NewDir)
    #Create scenarios directory if does not exist
    ScenarioPath <- file.path(NewDir, "scenarios")
    if (!dir.exists(ScenarioPath)) {
      dir.create(ScenarioPath)
    }
  }
  #Create analysis directory if does not exist
  AnalysisPath <- file.path(NewDir, "analysis")
  if (!dir.exists(AnalysisPath)) {
    dir.create(AnalysisPath)
  }
  #Copy and edit the model status file
  status_ls <- as.list(fromJSON(file.path(NewDir, "status.json")))
  Attribution <- 
    paste0("Model: ", ModelName, "   Author: ", Author, "   Copy Created: ", as.character(Sys.time()))
  status_ls$name <- ModelName
  status_ls$parent <- CopyModelName
  status_ls$created <- as.character(Sys.time())
  status_ls$lastedit <- as.character(Sys.time())
  status_ls$attribution <- c(Attribution, status_ls$attribution)
  status_ls$notes <- status_ls$notes
  writeLines(toJSON(status_ls, auto_unbox=TRUE), file.path(NewDir, "status.json"))
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
#' @param ModelName a string representation of the model name.
#' @return a list containing values for name, parent, created, and lastedit.
#' @export
loadModelStatus <- function(ModelName, Author = NULL){
  ModelDir <-  file.path("./models", ModelName)
  status_ls <- as.list(fromJSON(file.path(ModelDir, "status.json")))
  if (!is.null(Author)) {
    Attribution <- 
      paste0("Model: ", ModelName, "   Author: ", Author, "   Edited: ", as.character(Sys.time()))
    status_ls$attribution <- c(Attribution, status_ls$attribution)
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
#' @param ModelName a string representation of the model name.
#' @return a data frame containing the model concept information.
#' @export
loadModelConcepts <- function(ModelName){
  ModelDir <-  file.path("./models", ModelName)
  fromJSON(file.path(ModelDir, "concepts.json"))
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
#' @param ModelName a string representation of the model name.
#' @return a data frame containing the model relations information.
#' @export
loadModelRelations <- function(ModelName){
  ModelDir <-  file.path("./models", ModelName)
  fromJSON(file.path(ModelDir, "relations.json"), simplifyDataFrame = FALSE)
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
#' @param ModelData a model.
#' @return no return value. Has side effect of saving the model status list,
#' model concepts data frame, and model relations data frame as JSON-formatted
#' files.
#' @export
saveModel <- function(ModelData) {
  ModelName <- ModelData$status$name
  ModelDir <- file.path("./models", ModelName)
  writeLines(prettify(toJSON(ModelData$status, auto_unbox=TRUE)), file.path(ModelDir, "status.json"))
  writeLines(prettify(toJSON(ModelData$concepts, auto_unbox=TRUE)), file.path(ModelDir, "concepts.json"))
  writeLines(prettify(toJSON(ModelData$relations, auto_unbox=TRUE)), file.path(ModelDir, "relations.json"))
}



#------------------------------------------#
#             EDITING MODELS               #
#------------------------------------------#

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
#' @param Concepts_df a data frame containing the concepts data.
#' @return a data frame containing the concepts data to be shown in a table.
#' @export
formatConceptTable <- function(Concepts_df,export=FALSE) {

  if (export==FALSE){
    df <- data.frame(Name = Concepts_df$name,
                     ID = Concepts_df$concept_id,
                     Minimum = Concepts_df$values[["min"]],
                     Maximum = Concepts_df$values[["max"]],
                     #Group = Concepts_df$group,
                     stringsAsFactors = FALSE)
  } else{ # untested; written 2019/05/20
    df <- data.frame(Name = Concepts_df$name,
                     ID = Concepts_df$concept_id,
                     #"Group or Type" = Concepts_df$group,
                     Description = Concepts_df$description,
                     Minimum = Concepts_df$values[["min"]],
                     Maximum = Concepts_df$values[["max"]],
                     "Values (Min/Max) Description" = Concepts_df$values[["description"]],
                     check.names = FALSE,
                     stringsAsFactors = FALSE)
  }
  return(df)
}


#-----------------------------------#
# Extract vector of variables from relations list
#-----------------------------------#
#' Extracts a vector of variables from the relations list
#' @param Relations_ls a list containing the relations data.
#' @param var a list containing the relations data.
#' @param level where in the nested list is this variable located? Is it a property of a causal variable, a group of causal vars, or an affected variable?
#' @return a vector containing the data of interest 
#' @export
extract_rel <- function(Relations_ls, var, level = "causal_link"){
  switch(level, 
         "causal_link" = unlist(lapply(Relations_ls, function(a){lapply(a$affected_by, function(x){sapply(x$links, "[[", var)})})),
         "causal_group" = unlist(lapply(Relations_ls, function(a){lapply(a$affected_by, function(x){rep(x[[var]], length(x$links))})})),
         "affected" = unlist(lapply(Relations_ls, function(a){rep(a[[var]], sum(sapply(a$affected_by, function(x){length(x$links)})))}))
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
#' @param Relations_ls a list containing the relations data.
#' @return a data frame containing the relations data to be shown in a table.or exported
#' @export
formatRelationTable <- function(Relations_ls,Concepts_df,export=FALSE,use.full.names=TRUE) {
  name_key <- Concepts_df$name
  names(name_key) <- Concepts_df$concept_id
  
  causal_vars <- extract_rel(Relations_ls, "concept_id")
  affects_vars <- extract_rel(Relations_ls, "concept_id", level="affected")
  rel_ks <- extract_rel(Relations_ls, "k", level="affected")
  rel_types <- extract_rel(Relations_ls, "type", level="causal_group")
  rel_group <- unlist(lapply(Relations_ls, function(a){mapply(function(x, y){rep(y, length(x$links))}, a$affected_by, seq_along(a$affected_by))}))
  if (use.full.names){
    df <- data.frame(From = name_key[causal_vars],
                     To = name_key[affects_vars],
                     k = rel_ks,
                     Grouping = rel_group,
                     Type = rel_types,
                     Direction = extract_rel(Relations_ls, "direction"),
                     Weight = extract_rel(Relations_ls, "weight"),
                     stringsAsFactors = FALSE, row.names = NULL)
  } else {
    df <- data.frame(From = causal_vars,
                     To = affects_vars,
                     k = rel_ks,
                     Grouping = rel_group,
                     Type = rel_types,
                     Direction = extract_rel(Relations_ls, "direction"),
                     Weight = extract_rel(Relations_ls, "weight"),
                     stringsAsFactors = FALSE, row.names = NULL)
  }
  if (export){ # untested; written 2019/05/21
    df$Description <- extract_rel(Relations_ls, "description", level="causal_group")
  }
  return(df)
}


#----------------------------------------------#
# Make an Adjacency Matrix from a Relations List
#----------------------------------------------#
#' Creates an adjacency matrix 
#' \code{makeAdjacencyMatrix} creates an adjacency matrix from a relations list
#' 
#' This function creates an adjacency matrix from a relations list. The
#' adjacency matrix is a square matrix with as many rows and columns as the
#' number of concepts in the model. The rows and columns are named with the
#' concept variable names. The rows represent the causal side of the
#' relationship and the columns represent the affected side. The values in the
#' matrix are logicals with TRUE meaning that a relationship exists and FALSE
#' meaning that it does not.
#' 
#' @param Relations_ls a list of relations
#' @param Var_ a vector of concepts in the system (usually model$concepts$ID)
#' @export
makeAdjacencyMatrix <- function(Relations_ls, Var_, Type = "Logical") {
  # 2019/05/23: changed so that variables (Var_) is an input variable rather than defined through the relations list
  # e.g. if there is a concept that doesn't affect anything, it would be included here
  # Var_ <- unlist(lapply(Relations_ls, function(x) x$name)) 
    # Extract the relations and concepts from the data
    rs <- Relations_ls
    c_ids = Var_
    
    # Get all the variables associated with the edges 
    # (names of columns in any links data frame except "concept_id")
    edge_vars <- names(rs[[1]][["affected_by"]][[1]]$links[[1]]) #edge_vars <- names(rs[1, "affected_by"][[1]]$links[[1]])
    edge_vars <- edge_vars[edge_vars != "concept_id"]
    
    # Make a list of empty matrices, one for each of the edge variables in the data, plus the type of relationship (saved in "type" for each set of links)
    mx <- array(NA, dim = c(length(c_ids), length(c_ids)), 
                dimnames = list(c_ids, c_ids))
    mx_list <- rep(list(mx), length(edge_vars) + 2)
    names(mx_list) <- c(edge_vars, "type", "rel_group")
    
    # For number of relations present, find what is affecting it and for each edge variable
    # enter its value in the corresponding adjacency matrices
    for (i in 1:length(rs)){#1:dim(rs)[1]){ 
      id <- rs[[i]]$concept_id #rs$concept_id[i]
      infl_list <- rs[[i]]$affected_by #rs[rs["concept_id"] == id,"affected_by"]
      # Loop over all sets of links (Note: tfn data considers only 1 set)
      for (j in 1:length(infl_list)){ 
        links <- infl_list[[j]]$links # list of links in that set of influences ##infl_list[[j]]$links[[1]] # a data frame
        infls <- sapply(links, function(x) x$concept_id) #sapply(links$concept_id, function(x) which(c_ids == x))
        for (e in edge_vars){
          mx_list[[e]][infls, id] <- sapply(links, '[[', e)
        }
        mx_list[["type"]][infls, id] <- infl_list[[j]]$type # Corresponding type for that set of links (and/or etc.)
        mx_list[["rel_group"]][infls, id] <- j
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
#' @param VarName a string representation of the concept variable name
#' @return a data frame which includes all the mandatory relations fields with
#' the name field populated with the concept variable name and all other fields
#' populated with empty fields
#' @export
initRelationsEntry <- function(VarName){
  Lst <- 
    list(name = VarName,
         affects = list("")
    )
  Lst$affects[[1]] <- data.frame(
    variable = "",
    direction = "",
    weight = "",
    description = ""
  )
  Lst
}

#---------------------------------------------------------------#
# Define function to extract weight and direction
#---------------------------------------------------------------#
# Create an adjacency matrix that uses both (sign from direction + magnitude from weight) 
# to get a single numerical value for each edge.
adjMatrixCalc <- 
  function(adj_mx_list){
  # Define what numbers the linguistic values (low/highs) are converted to
  # vals <- c(VL = 0.1, L = 0.25, ML = 0.375, M = 0.5, MH = 0.675, H = 0.75, VH = 0.95) 
  vals <- c(VL = 1, L = 1, ML = 1, M = 1, MH = 1, H = 1, VH = 1) # Use uniform weighting for now
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
#'
#' @param Relations_ls a list of model relations.
#' @param Concepts_df a data frame with model concepts.
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
  function(Relations_ls, Concepts_df, RowGroup = "All", ColGroup = "All",
           orientation = "Portrait", rankdir = "Top-to-Bottom", shape = "box",
           Show = "label")
  {
    # Name to variable key to use for labels
    name_key <- Concepts_df$concept
    names(name_key) <- Concepts_df$concept_id
    
    #Make matrices of relations and labels
    Cn <- Concepts_df$concept_id
    adj_mx_ls <- makeAdjacencyMatrix(Relations_ls, Cn)
    adj_mx_ls$weight_num <- adjMatrixCalc(adj_mx_ls)
    #Create row and column indices for selected row and column groups
    if (RowGroup == "All") {
      Cr <- Cn
    } else {
      Cr <- Cn[Concepts_df$group %in% RowGroup]
    }
    if (ColGroup == "All") {
      Cc <- Cn
    } else {
      Cc <- Cn[Concepts_df$group %in% ColGroup]
    }
    #Select relations and labels matrices for selected rows and columns
    Relates.CrCc <- adj_mx_ls$weight_num[Cr,Cc]
    Labels.CrCc <- adj_mx_ls$weight[Cr,Cc]
    Concepts. <- unique(Cr)
    #Remove rows and columns that are all NA values
    # AllNARows_ <- apply(Relates.CrCc, 1, function(x) all(is.na(x)))
    # AllNACols_ <- apply(Relates.CrCc, 2, function(x) all(is.na(x)))
    # Relates.CrCc <- Relates.CrCc[!AllNARows_, !AllNACols_]
    # Labels.CrCc <- Labels.CrCc[!AllNARows_, !AllNACols_]
    #Update Cr and Cc and identify unique concepts
    Cr <- rownames(Relates.CrCc)
    Cc <- colnames(Relates.CrCc)
    #Convert rankdir argument
    if (rankdir == "Top-to-Bottom") rankdir <- "TB"
    if (rankdir == "Left-to-Right") rankdir <- "LR"
    #Make DOT data
    Dot_ <-
      paste("digraph {\n orientation =", orientation, ";\n rankdir =", rankdir, ";\n")
    for (concept in Concepts.) {
      c_ <- gsub("\\-","\\_",concept) # replace hyphens with underscores for proper grViz syntax
      l_ <- gsub("([[:punct:]])", "\\\\\\1", name_key[concept]) # escape punctuation characters in node labels
      Dot_ <- paste(Dot_, c_, "[ shape =", shape, ", label =\"", l_,"\"];\n")
    }
    for (cr in Cr) {
      for (cc in Cc) {
        Value <- Relates.CrCc[cr,cc]
        if (!is.na(Value)) {
          if (Show == "label") {
            Label <- Labels.CrCc[cr,cc]
          } else {
            Label <- Value
          }
          if (Value != 0) {
            if (Value > 0) {
              Dot_ <- paste0(Dot_, cr, " -> ", cc, "[ label=", Label, " ];\n")
            } else {
              Dot_ <-
                paste0(
                  Dot_, cr, " -> ", cc, "[ color=red, fontcolor=red, style=dashed, label=", Label, " ];\n"
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


#---------------------------------------------#
#             RUNNING THE MODEL               #
#---------------------------------------------#

#---------------#
# Run simulation
#---------------#
run_model <- function(model, params){
  Relations_ls <- model$relations
  Cn <- model$concepts$concept_id
  adj_mx_ls <- makeAdjacencyMatrix(Relations_ls, Cn)
  adj_mx_ls$weight_num <- adjMatrixCalc(adj_mx_ls)
  run <- fcm.run(adj_mx_ls$weight_num, adj_mx_ls$type, adj_mx_ls$rel_group, 
                 cn = Cn, iter = params$iter, k = params$k, init = params$init, 
                 infer_type = params$infer_type, h = params$h, lambda = params$lambda)#,
                 # set.concepts = scen$var, set.values = scen$val) %>% 
    # mutate(scenario=scen$name, 
    #        infer_type=infer_type,
    #        h = h,
    #        lambda = lambda,
    #        model = m)
  return(run)
}


#--------------------------------------------------------#
# Rescaling a Value from an Input Range to an Output Range
#--------------------------------------------------------#
#' Rescale value
#'
#' \code{rescale} rescales a value from a specified input range to a specified
#' output range.
#'
#' This function rescales a value from a specified input range to a specified
#' output range. The default output range is 0 to 100 because that is the range
#' used to represent concepts in FSDM models.
#'
#' @param Value a numeric value to be rescaled from the input range to the
#' output range.
#' @param FromRange a numeric vector of length 2 in which the first value is
#' the minimum value in the range and the second value is the maximum value in
#' the range.
#' @param ToRange a numeric vector of length 2 in which the first value is the
#' minimum value in the range and the second value is the maximum value in the
#' range.
#' @return A numeric value in the output range.
#' @export
rescale <- function(Value, FromRange, ToRange) {
  ToRange[1] + diff(ToRange) * (Value - FromRange[1]) / diff(FromRange)
}
# Example
# rescale(1:10, c(0,10), c(0,100))
