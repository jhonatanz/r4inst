#' R6 Class Representing a Cable
#'
#' @description
#' The class cable defines various fields and methods that allows three basic operations (methods):
#' - Create a cable structure: given a group type and a group quantity (see the definitions of the fields) create and numerate the conductors of the cable.
#' - provide a data frame with the structure of the cable to be used on other process.
#' - define other attributes.
#'
#' @details
#' The object store information about cables to be used in an automation project
#' @export
cable <- R6::R6Class("cable",
                     public = list(
                       #' @field tag Cable name: character.
                       tag = NULL,
                       #' @field cable_spec Cable Specification document code: character.
                       cable_spec = NULL,
                       #' @field group Cable agroupation: "cond" per conductor, "pair" per pairs, "triad" per triads, "quad" per quartet: character.
                       group = NULL,
                       #' @field groups_qty Groups quantity: numeric, int.
                       groups_qty = NULL,
                       #' @field ovr_shield  If cable have overall shield or not: logic.
                       ovr_shield = NULL,
                       #' @field ind_shields If each group have shield or not: logic.
                       ind_shields = NULL,
                       #' @field ovr_diam The overall diameter of the cable: numeric, float.
                       ovr_diam = NULL,
                       #' @field ovr_diam_UOM The overall diameter units of measurement.
                       ovr_diam_UOM = NULL,
                       #' @field struct Data structure for conductors and groups.
                       struct = vector(mode = "list", length = 0),

                       #' @description
                       #' Create a new cable object.
                       #' @param tag cable tag.
                       #' @return A new `cable` object.
                       initialize = function(tag){
                         if(nchar(tag)==0){
                           stop("Tag for cable should have at least one character")
                         }
                         self$tag <- as.character(tag)
                       },

                       #' @description
                       #' Generates the data structure for conductors and groups, modifies the object updating the 'struct' field.
                       #' @param group Cable agroupation: "cond" per conductor, "pair" per pairs, "triad" per triads, "quad" per quartet: character.
                       #' @param groups_qty Groups quantity: numeric, int.
                       #' @param ind_shields If each group have shield or not: logic.
                       #' @param ovr_shield If cable have overall shield or not: logic.
                       #' @examples
                                              #' # example code
                                              #'
                                              #' New_Cable <- cable$new("cable-001")
                                              #'
                                              #' # Structure definition of a cable grouped by pairs, with 3 pairs,
                                              #' # individual and overall shield.
                                              #'
                                              #' New_Cable$generate_structure("pair", 3, TRUE, TRUE)
                                              #' New_Cable$df_structure()
                       generate_structure = function(group, groups_qty, ind_shields,
                                                     ovr_shield){
                         self$group <- group
                         self$groups_qty <- groups_qty
                         self$ind_shields <- ind_shields
                         self$ovr_shield <- ovr_shield
                         seq_grp <- seq(from = 1, to = groups_qty, by = 1)
                         len <- ifelse(ovr_shield, groups_qty+1, groups_qty)
                         struct <- vector(mode = "list", length = len)

                         std <- dplyr::case_when(
                           group == "cond" ~ list(abr = "CO",
                                                  cond = c("C1"),
                                                  color = c("BL")),
                           group == "pair" ~ list(abr = "PR",
                                                  cond = c("C1", "C2"),
                                                  color = c("BL", "WH")),
                           group == "triad" ~ list(abr = "TR",
                                                   cond = c("C1", "C2", "C3"),
                                                   color = c("BL", "WH", "RD")),
                           group == "quad" ~ list(abr = "TR",
                                                   cond = c("C1", "C2", "C3", "C4"),
                                                   color = c("BL", "WH", "RD", "GR")),
                           .default = NULL
                         )
                         grps <- paste0(std$abr, seq_grp)

                         for(i in seq_along(struct)){
                           struct[[i]] <- list(group = grps[i], cond = std$cond,
                                              color = std$color)
                           if(ind_shields){
                             struct[[i]]$cond <- c(struct[[i]]$cond, "SH")
                             struct[[i]]$color <- c(struct[[i]]$color, "NA")
                           }

                           if(i>groups_qty){
                             struct[[i]] <- list(group = "OV_SHD", cond = "SH",
                                                color = "NA")
                             grps <- c(grps, "OV_SHD")
                           }
                         }

                         names(struct) <- grps
                         self$struct <- struct
                         invisible(self)
                       },

                       #' @description
                       #' Converts the 'struct' field to a data frame.
                       #' @returns a data frame with the structure of the cable.
                       #' @examples
                                              #' # Conversion of cable structure to a data frame
                                              #' New_Cable<- cable$new("cable-001")
                                              #'
                                              #' df_struct <- New_Cable$df_structure()
                                              #' str(df_struct)
                       df_structure = function(){
                         df <- tibble::tibble(tag = self$tag, dplyr::bind_rows(self$struct))
                         return(df)
                       },

                       #' @description
                       #' Updates attributes for cable, specifically cable specification, overall diameter, overall diameter UOM, this data is encapsulated on a list.
                       #' @param attrib list with the following named fields: cable_spec, ovr_diam, ovr_diam_UOM.
                       #' @examples
                                              #' # Update of atributtes of a cable
                                              #'
                                              #' New_Cable <- cable$new("cable-001")
                                              #'
                                              #' at <- list(
                                              #'   cable_spec = "cab-spec-001",
                                              #'   ovr_diam = 0.58,
                                              #'   ovr_diam_UOM = "in")
                                              #'
                                              #' New_Cable$update_attr(at)
                                              #' New_Cable
                       update_attr = function(attrib){
                         self$cable_spec <- attrib$cable_spec
                         self$ovr_diam <- attrib$ovr_diam
                         self$ovr_diam_UOM <- attrib$ovr_diam_UOM
                       },

                       #' @description
                       #' print the information for cable objects.
                       print = function(){
                         cat("CABLE: \n")
                         cat("  Tag: ", self$tag, "\n", sep = "")
                         cat("  Cable Spec: ", self$cable_spec, "\n", sep = "")
                         cat("  grouped by: ", self$group, "\n", sep = "")
                         cat("  groups qty: ", self$groups_qty, "\n", sep = "")
                         cat("  overall shield: ", self$ovr_shield, "\n", sep = "")
                         cat("  individual shields: ", self$ind_shields, "\n", sep = "")
                         cat("  Overall diameter: ", self$ovr_diam, "\n", sep = "")
                         cat("  Overall diameter UOM: ", self$ovr_diam_UOM, "\n", sep = "")
                         cat("  structure generated: ", length(self$struct) > 0, "\n", sep ="")

                       }
                     )
)
