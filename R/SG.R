#' R6 Class Representing a Signal
#'
#' @description
#' The class 'SG' defines various fields and methods for signals that allows:
#'
#' - Create a signal: given a name, a device (origin), a control system
#' (destiny) and a signal type.
#' - Assign the signal to a cable.
#' - Check that the signal have a complete path from origin to destiny.
#'
#' @details
#' The SG objects are defined to follow the signal that will be generated on
#' field devices or control systems.
#'
#' Each signal should be associated with an origin and a destiny. Additionally,
#' the signal would be associated to one or various cables that will be conduct
#' the signal from the origin to the destiny.
#'
#' As convention, the origin is the device or control system that generates the
#' signal, the destiny is the device or control system that receives the signal.
#'
#' @export
SG <- R6::R6Class("SG",
                  public = list(
                    #' @field tag The name of the signal, character
                    tag = NULL,
                    #' @field origin Origin (device or CS that generates the
                    #' signal), character
                    origin = NULL,
                    #' @field destiny Destiny (device or CS that receive the
                    #' signal), character
                    destiny = NULL,
                    #' @field type Type of the signal (Analog or digital),
                    #' character
                    type = NULL,
                    #' @field path Path that follows the signal from origin to
                    #' destiny, list
                    path = NULL
                  ))
