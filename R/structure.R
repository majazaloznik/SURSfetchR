#' Get API response in json format and extract as text
#'
#' This function is a general funciton to return a simple json response in text
#' format. The defaults are set to return from the GetStructure method on the
#' SiStat API with the complete file (matrix) and category (field) structure
#' for the available .px files, but can be used for anything. Added the user agent
#' as well, currenlty this package repository.
#'
#' @param url in case you want to call some other API, but defaults to the GetStructure one
#' @param ua thought i'd be nice and identify myself in the call, the user agent
#' gets put in the call header in case it might be helpful.
#'
#' @return (large) character vector with json response from the API to be parsed and handled
#' downstream
#' @export
#'
get_API_response <- function(url = "https://pxweb.stat.si/SiStat/sl/Api/GetStructure",
                                   ua = httr::user_agent("https://github.com/majazaloznik/SURSfetchR")){
  res <- httr::GET(url = url,
                   response = "json",
                   ua)
  cont <- httr::content(res, as = "text")
}

#' Parse the SURS GetStructure API response into usable format
#'
#' Takes the text response from the Getstructure API that we get using \link[SURSfetchR]{get_API_response}.
#' Defaults to returning a tree structure from the package data.tree, but
#' you could also return a data frame or a list of lists.
#'
#' @param res API text response - output from the SURS GetStructure API.
#' @param output defaults to "tree", other possibilities are "lol" for a list of lists
#' and "df" for a df obvs.
#'
#' @return a data.tree tree object, a data frame or a list of lists
#' @export
#'
parse_structAPI_response <- function(res, output = "tree") {
  struc_list <- jsonlite::fromJSON(res,
                                   simplifyDataFrame = FALSE)
  tryCatch(
    {if(output == "tree") {
      out <- data.tree::as.Node(struc_list) } else {
        if(output == "lol") {
          out <- struc_list } else {
            if(output == "df") {
              out <- parsed_request <- jsonlite::fromJSON(res)}
          }
      }
      return(out)
    },
    error = function(error_message) {
      message("That's not a legal output type")
      return(NA)
    }
  )
}


#' Helper - get node names
#'
#' Getting data from the depths of the API response. Helper function used
#' to reshape and extract the data structure. Gets the name of the "podrocje"
#' or the px matrix code
#'
#' @param node tree node
#'
#' @return character vector of length 1
#' @keywords internal
node_name <- function(node) {
  result <- node$naslovSlo
  if(length(result) == 0) result <- node$pxMatrixName
  return(result)
}

#' Helper - get node ids
#'
#' Getting data from the depths of the API response. Helper function used
#' to reshape and extract the data structure. Gets the ID of the "podrocje"
#' or of the px matrix.
#'
#' @param node tree node
#'
#' @return character vector of length 1
#' @keywords internal
node_id <- function(node) {
  result <- node$idSistatPodrocje
  if(length(result) == 0) result <- node$idPxMatrix
  return(result)
}

#' Helper - get px file ids
#'
#' Getting data from the depths of the API response. Helper function used
#' to reshape and extract the data structure. Gets the ID of the matrix file
#' (which is different from the matrix ID, since it changes with every update).
#'
#' @param node tree node
#'
#' @return character vector of length 1
#' @keywords internal
node_file_id <- function(node) {
  result <- node$idPxFile
  return(result)
}

#' Helper - get matrix (table) name
#'
#' Getting data from the depths of the API response. Helper function used
#' to reshape and extract the data structure. Gets the long form name of the
#' matrix.
#'
#' @param node tree node
#'
#' @return character vector of length 1
#' @keywords internal
node_matrix_name <- function(node) {
  result <- node$description
  return(result)
}


#' Helper - get matrix (table) last update time
#'
#' Getting data from the depths of the API response. Helper function used
#' to reshape and extract the data structure. Gets the last updated time
#' for a matrix
#'
#' @param node tree node
#'
#' @return character vector of length 1
#' @keywords internal
node_updated <- function(node) {
  result <- node$lastUpdated
  return(result)
}

#' Helper - get matrix (table) parent
#'
#' Getting data from the depths of the API response. Helper function used
#' to reshape and extract the data structure. Gets the node's parent node name.
#' Of course if the parent name is (archive)matrixList, it get's the next parent.
#'
#' @param node tree node
#'
#' @return character vector of length 1
#' @keywords internal
node_parent <- function(node) {
  if( node$isRoot ) {
    result <- 0
  } else {
    if( node$parent$isRoot ) {
      result <- 0
    } else {
      result <- node$parent$idSistatPodrocje
      if(length(result) == 0) result <- node$parent$parent$idSistatPodrocje
    }
    return(result)
  }
}

#' Extract proper hierarcy from tree structure from thr GetStructure API
#'
#' The tree structure has superfluous levels that need to be collapsed, which this function
#' does, while pulling out the relevant labels and other fields we are interested in and
#' outputs a data frame.
#'
#' @param tree data.tree structure from \link[SURSfetchR]{parse_structAPI_response}
#'
#' @return a data frame with 6 columns and a row for each matrix and each field.
#' @export
#'
get_full_structure <- function(tree){
  file_id <- NULL
  id <- NULL
  matrix_name <- NULL
  my_name <- NULL
  my_name_parent <- NULL
  parent_id <- NULL
  parent_name <- NULL
  pathString <- NULL
  updated <- NULL
  tree$Do(function(node) node$name <- ifelse(grepl("[0-9]", node$name), node_name(node), node$name))

  wnames <- data.tree::ToDataFrameTree(tree,
                                       name = "name",
                                       my_name = node_name,
                                       id = node_id,
                                       file_id =node_file_id,
                                       matrix_name = node_matrix_name,
                                       updated = node_updated,
                                       "pathString",
                                       parent_id = node_parent)

  #remove supreflous levels and get parent ids
  wnames %>%
    dplyr::mutate(pathString = gsub("/childPodrocja", "", pathString),
                  pathString = gsub("/matrixList", "", pathString),
                  pathString = gsub("/archiveMatrixList$", "", pathString),
                  pathString = gsub("^Root/", "Si-Stat/", pathString)) %>%
    dplyr::select(id, my_name, parent_id,  pathString,  matrix_name, updated, file_id)  %>%
    dplyr::filter(!is.na(id)) %>%
    (function(df) dplyr::left_join(df, df %>% dplyr::select("my_name", "id"), by= c("parent_id" = "id"),
                                   suffix = c("", "_parent"))) %>%
    dplyr::rename(name = my_name, parent_name = my_name_parent) %>%
    dplyr::mutate(parent_name = ifelse(is.na(parent_name), "SiStat", parent_name)) %>%
    dplyr::mutate(arch = grepl("archiveMatrixList", pathString)) %>%
    dplyr::relocate(parent_name, .after = parent_id) %>%
    dplyr::distinct()-> full_hierarchy
}



#' Extract only the matrix hierarchy from the full one
#'
#' Extracts only rows describing matrices from the full hierarchy.
#'
#' @param full_df data frame output from  \link[SURSfetchR]{get_full_structure}
#'
#' @return a data frame with 7 columns and a row for each matrix
#'
#' @export
get_matrix_hierarchy <- function(full_df) {
  full_df %>%
    dplyr::filter(grepl("[0-9]{3}S$", pathString)) -> matrix_hierarchy
  matrix_hierarchy
}


#' Extract only the field hierarchy from the full one
#'
#' Extracts only rows describing fields from the full hierarchy.
#'
#' @param full_df data frame output from  \link[SURSfetchR]{get_full_structure}
#'
#' @return a data frame with 4 columns and a row for each field.
#'
#' @export
get_field_hierarchy <- function(full_df) {
  full_df %>%
    dplyr::mutate(pathString = gsub("/H[0-9]{3}S$", "", pathString),
           pathString = gsub("/[A-Z0-9]{,7}S$", "", pathString),
           pathString = gsub("/archiveMatrixList$", "", pathString),
           pathString = gsub("/$", "", pathString)) %>%
    dplyr::distinct(pathString, .keep_all= TRUE) %>%
    dplyr::select(-file_id,  -matrix_name, -updated) -> field_hierarchy
  field_hierarchy
}



#' Get tree out of hierarchical df
#'
#' Takes the df output from \link[SURSfetchR]{get_field_hierarchy} or
#' \link[SURSfetchR]{get_matrix_hierarchy} and creates a data.tree
#'
#' @param df data frame output from \link[SURSfetchR]{get_field_hierarchy} or
#' \link[SURSfetchR]{get_matrix_hierarchy}
#'
#' @return  a data.tree tree object
#' @export
get_tree <- function(df) {
  tree <- data.tree::as.Node(df)
  tree
}
