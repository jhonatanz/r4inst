#' R6 Class Representing a Terminal Strip
#'
#' @description
#' The class 'TS' defines various fields and methods for terminal strips that
#' allows:
#'
#' - Create a terminal strip: given a name, a device and type of terminal.
#' - Create a structure of terminal strip given a quantity of terminals.
#' - Add terminals to connect a cable.
#' - Connect a cable conductor to terminals in the strip.
#'
#' @details
#' The TS objects can be of two types: "end" and "pass".
#'
#' The "end" terminal strip type is used for devices at the end of a path, i.e
#' instruments, control panels, final elements, control systems.
#'
#' The "pass" terminal strip type is used for Junction Boxes or marshalling
#' panels in which the terminals will have two sides: "input" and "output", and
#' terminal will connect two ends of cables.
#'
#' @export
TS <- R6::R6Class("TS",
                  public = list(
                    #' @field tag Terminal strip name: character.
                    tag = NULL,
                    #' @field device Device to which terminal strip belongs:
                    #' character.
                    device = NULL,
                    #' @field term_type Terminals type in the strip,
                    #' "end" for terminals at the end of a path and "pass"
                    #' for terminals that connects two connectors: character.
                    term_type = NULL,
                    #' @field term_qty quantity of terminals in strip:
                    #' numeric, int.
                    term_qty = NULL,
                    #' @field struct Data structure for connections.
                    struct = vector(mode = "list", length = 0),

                    #' @description
                    #' Create a new TS object.
                    #' @param tag terminal strip tag.
                    #' @param device device to which the terminal strip
                    #' belongs
                    #' @param term_type erminal type to be used: "end"
                    #' or "pass.
                    #' @return A new `TS` object.
                    initialize = function(tag, device, term_type){
                      if(nchar(tag)==0){
                        stop("Tag for terminal strip should have at least one
                             character")
                      }
                      if(term_type != "end" & term_type != "pass"){
                        stop("Terminal type should be either 'end' or 'pass'")
                      }
                      self$tag <- as.character(tag)
                      self$device <- device
                      self$term_type <- term_type
                      invisible(self)
                    },

                    #' @description
                    #' Creates the structure of terminals object. Modifies
                    #' the struct field of the object.
                    #' @param term_qty Quantity of terminals required.
                    gen_struct = function(term_qty){
                      self$term_qty <- term_qty
                      struct <- private$create_struct(from = 1,
                                                      qty = term_qty)
                      private$append_struct(struct)
                      invisible(self)
                    },

                    #' @description
                    #' Add the specified quantity of terminals at the end of the
                    #' current TS object.
                    #' @param qty Quantity of terminals to be added.
                    add_terminal = function(qty){
                      ts_df <- self$df_struct()
                      from <- nrow(ts_df)+1
                      struct <- private$create_struct(from = from,
                                                      qty = qty)
                      private$append_struct(struct)
                      self$term_qty <- nrow(self$df_struct())
                      invisible(self)
                    },

                    #' @description
                    #' Add terminals to the terminal strip at the end of it and
                    #' connect a cable to those.
                    #' @param cable a cable object to be added
                    #' @param side input or output side, for pass type
                    #' terminal strips
                    #' @examples
                    #' # Addition of a new cable to an existing terminal strip
                    #' New_TS <- TS$new("TS-001", "JB-001", "pass")
                    #' New_TS$gen_struct(3)
                    #' (New_TS_df <- New_TS$df_struct())
                    #'
                    #' New_cable <- CB$new("CB-001")
                    #' New_cable$gen_struct("pair", 1, TRUE, FALSE)
                    #'
                    #' New_TS$add_cable(New_cable, side = "input")
                    #' (New_TS_df_updated <- New_TS$df_struct())
                    add_cable = function(cable, side = NULL){
                      cable_df <- cable$df_struct()
                      ts_df <- self$df_struct()
                      qty <- nrow(cable_df)
                      from <- nrow(ts_df)+1
                      struct <- private$create_struct(from = from, qty = qty)
                      if(self$term_type == "pass"){
                        if(side == "input"){
                          struct$term_conn_I <- paste(cable_df$tag, cable_df$group, cable_df$cond, sep = "/")
                        }
                        if(side == "output"){
                          struct$term_conn_O <- paste(cable_df$tag, cable_df$group, cable_df$cond, sep = "/")
                        }
                      }
                      if(self$term_type == "end"){
                        struct$term_conn <- paste(cable_df$tag, cable_df$group, cable_df$cond, sep = "/")
                      }
                      private$append_struct(struct)
                      self$term_qty <- nrow(self$df_struct())
                      invisible(self)
                    },

                    #' @description
                    #' Connect cable to existing terminals. If not enough
                    #' terminals available for connect the cable, raise an
                    #' error. Will overwrite existing connections.
                    #' @param cable a cable object to be added.
                    #' @param side input or output side, for "pass" type
                    #' terminal strips, if "end" terminal type, this parameter
                    #' will be ignored.
                    #' @param init initial terminal to use to connect the cable,
                    #' this and the subsequent terminals will be used until the
                    #' whole cable is connected.
                    #' @examples
                    #' # Connection of a cable to an existing terminal strip
                    #' New_TS <- TS$new("TS-001", "JB-001", "end")
                    #' New_TS$gen_struct(3)
                    #' (New_TS_df <- New_TS$df_struct())
                    #'
                    #' New_cable <- CB$new("CB-001")
                    #' New_cable$gen_struct("pair", 1, TRUE, FALSE)
                    #'
                    #' New_TS$con_cable(New_cable, init = 1)
                    #'
                    #' (New_TS_df_upt <- New_TS$df_struct())
                    con_cable = function(cable, side = NULL, init){
                      cable_df <- cable$df_struct()
                      ts_df <- self$df_struct()
                      from <- init
                      to <- nrow(cable_df)+init-1
                      if(nrow(ts_df) < to){
                        stop("not enough terminals to add the cable, please add
                             more terminals")
                      }
                      if(self$term_type == "pass"){
                        if(side == "input"){
                          term_conn_I <- paste(cable_df$tag, cable_df$group,
                                               cable_df$cond, sep = "/")
                          for(i in from:to){
                            self$struct$term_conn_I[[i]] <- term_conn_I[i-init+1]
                          }
                        }
                        if(side == "output"){
                          term_conn_O <- paste(cable_df$tag, cable_df$group,
                                               cable_df$cond, sep = "/")
                          for(i in seq(from = from, to = to)){
                            self$struct$term_conn_O[[i]] <- term_conn_O[i-init+1]
                          }
                        }
                      }
                      if(self$term_type == "end"){
                        term_conn <- paste(cable_df$tag, cable_df$group,
                                             cable_df$cond, sep = "/")
                        for(i in from:to){
                          self$struct$term_conn[[i]] <- term_conn[i-init+1]
                        }
                      }
                    invisible(self)
                    },

                    #' @description
                    #' Converts the 'struct' field to a data frame.
                    #' @returns a data frame with the structure of the
                    #' terminal strip.
                    #' @examples
                    #' # Conversion of terminal strip structure to a data frame
                    #' New_TS<- TS$new("TS-001", "JB-001", "end")
                    #' New_TS$gen_struct(5)
                    #'
                    #' (df_struct <- New_TS$df_struct())
                    df_struct = function(){
                      df <- tibble::tibble(device = self$device,
                                           tag = self$tag,
                                           dplyr::bind_rows(self$struct))
                      return(df)
                    },

                    #' @description
                    #' print the information for terminal strip objects.
                    print = function(){
                      cat("TERMINAL STRIP: \n")
                      cat("  Tag: ", self$tag, "\n", sep = "")
                      cat("  Device: ", self$device, "\n", sep = "")
                      cat("  Terminal type: ", self$term_type, "\n", sep = "")
                      cat("  Terminals qty: ", self$term_qty, "\n", sep = "")
                      cat("  structure generated: ", length(self$struct) > 0,
                          "\n", sep ="")
                    }
                  ),
                  private = list(
                    append_struct = function(struct){
                      if(self$term_type == "end"){
                        self$struct$nums <- append(self$struct$nums,
                                                   struct$nums)
                        self$struct$term_tags <- append(self$struct$term_tags,
                                                   struct$term_tags)
                        self$struct$term_conn <- append(self$struct$term_conn,
                                                   struct$term_conn)
                      }
                      if(self$term_type == "pass"){
                        self$struct$nums_inp <- append(self$struct$nums_inp,
                                                   struct$nums_inp)
                        self$struct$term_tags_I <- append(self$struct$term_tags_I,
                                                        struct$term_tags_I)
                        self$struct$term_conn_I <- append(self$struct$term_conn_I,
                                                        struct$term_conn_I)
                        self$struct$nums_out <- append(self$struct$nums_out,
                                                       struct$nums_out)
                        self$struct$term_tags_O <- append(self$struct$term_tags_O,
                                                          struct$term_tags_O)
                        self$struct$term_conn_O <- append(self$struct$term_conn_O,
                                                          struct$term_conn_O)
                      }
                    },

                    create_struct = function(from, qty){
                      if(self$term_type == "end"){
                        nums <- paste0("T", seq(from = from,
                                                to = from + qty - 1))
                        term_tags <- rep("NA", times = qty)
                        term_conn  <- rep("NA", times = qty)
                        struct <- list(nums = nums,
                                       term_tags = term_tags,
                                       term_conn = term_conn)
                        return(struct)
                      }
                      if(self$term_type == "pass"){
                        nums_inp <- paste0("TI", seq(from = from,
                                                     to = from + qty - 1))
                        nums_out <- paste0("TO", seq(from = from,
                                                     to = from + qty - 1))
                        term_tags_I <- rep("NA", times = qty)
                        term_tags_O <- rep("NA", times = qty)
                        term_conn_I <- rep("NA", times = qty)
                        term_conn_O <- rep("NA", times = qty)
                        struct <- list(nums_inp = nums_inp, nums_out = nums_out,
                                       term_tags_I = term_tags_I,
                                       term_tags_O = term_tags_O,
                                       term_conn_I = term_conn_I,
                                       term_conn_O = term_conn_O)
                        return(struct)
                      }
                    }
                  )
)

