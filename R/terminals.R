#' R6 Class Representing a Terminal Strip
#'
#' @description
#' The class terminals defines various fields and methods that allows:
#' - Create a terminal strip: given a type of terminal and quantity create and numerate the strips.
#' - Assign the terminal strip to a device (instrument or box).
#' - Connect a cable conductor to terminals in the strip.
#'
#' @details
#' The object store information about terminals to be used in an automation project
#' @export
terminals <- R6::R6Class("terminals",
                     public = list(
                       #' @field tag Terminal strip name: character.
                       tag = NULL,
                       #' @field terminal_type Terminals type in the strip,
                       #' "end" for terminals at the end of a path and "pass"
                       #' for terminals that connects two connectors: character.
                       terminal_type = NULL,
                       #' @field terminals_qty quantity of terminals in strip:
                       #' numeric, int.
                       terminals_qty = NULL,
                       #' @field struc Data structure for connections.
                       struct = vector(mode = "list", length = 0),

                       #' @description
                       #' Create a new terminals object.
                       #' @param tag terminal strip tag.
                       #' @return A new `terminals` object.
                       initialize = function(tag){
                         if(nchar(tag)==0){
                           stop("Tag for cable should have at least one character")
                         }
                         self$tag <- as.character(tag)
                       }




                     )
)
