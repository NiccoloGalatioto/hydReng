# Methods for class CSarbitrary
#------------------------------------------------------------------------------

# Accessors
#----------
setMethod("x", "CSarbitrary", function(object) object@x)
setMethod("z", "CSarbitrary", function(object) object@z)
setMethod("xb_l", "CSarbitrary", function(object) object@xb_l)
setMethod("xb_r", "CSarbitrary", function(object) object@xb_r)
setMethod("kSt_B", "CSarbitrary", function(object) object@kSt_B)
setMethod("kSt_l", "CSarbitrary", function(object) object@kSt_l)
setMethod("kSt_r", "CSarbitrary", function(object) object@kSt_r)

# Validity check
#---------
setValidity("CSarbitrary", function(object) {
  msg <- NULL
  valid <- TRUE

  # Check if lengths of x and z match
  if (length(x(object)) != length(z(object))) {
    valid <- FALSE
    msg <- c(msg, "x and z have not the same length")
  }

  # Check xb_l if it's not NULL and if it matches any x
  if (!is.null(xb_l(object))) {
    if (!(xb_l(object) %in% x(object))) {
      valid <- FALSE
      msg <- c(msg, "xb_l is not equal to any x")
    }
  }

  # Check xb_r if it's not NULL and if it matches any x
  if (!is.null(xb_r(object))) {
    if (!(xb_r(object) %in% x(object))) {
      valid <- FALSE
      msg <- c(msg, "xb_r is not equal to any x")
    }
  }

  # Check if x is unique
  if (any(duplicated(x(object)))) {
    valid <- FALSE
    msg <- c(msg, "x values must be unique")
  }

  # Return validation result
  if (valid) TRUE else msg
})

# Replacement methods
#------------
setMethod("return_valid_object", "CSarbitrary", function(object) {
  if (validObject(object)) return(object)
})

setMethod("x<-", "CSarbitrary", function(object, value) {
  object@x <- value
  return_valid_object(object)
})

setMethod("z<-", "CSarbitrary", function(object, value) {
  object@z <- value
  return_valid_object(object)
})

setMethod("xb_r<-", "CSarbitrary", function(object, value) {
  object@xb_r <- value
  return_valid_object(object)
})

setMethod("xb_l<-", "CSarbitrary", function(object, value) {
  object@xb_l <- value
  return_valid_object(object)
})

setMethod("kSt_B<-", "CSarbitrary", function(object, value) {
  object@kSt_B <- value
  return_valid_object(object)
})

setMethod("kSt_l<-", "CSarbitrary", function(object, value) {
  object@kSt_l <- value
  return_valid_object(object)
})

setMethod("kSt_r<-", "CSarbitrary", function(object, value) {
  object@kSt_r <- value
  return_valid_object(object)
})

# Wetted Area
#---------
#' @title Calculates Wetted Area
#' @name wetted_area
#' @aliases wetted_area,CSarbitrary-method
#' @description Calculates the wetted area of a CSarbitrary or CScircle
#'     object for given water levels.
#' @usage wetted_area(object, h, ret = "A")
#' @param object An object of class CSarbitrary or CScircle.
#' @param h A numeric vector of water levels [m].
#' @param ret A character string; if `A`, returns total wetted area. If `Aii`,
#'     returns wetted area by segment.
#' @return A numeric vector or matrix of wetted areas based on the `ret`
#'     argument.
#' @importFrom methods new validObject
#' @importFrom stats approx
#' @examples
#' # Define sample cross-section data
#' x <- c(0, 4, 9, 13)
#' z <- c(2, 0, 0, 2)
#' cs <- CSarbitrary(x = x, z = z, xb_l = 4, xb_r = 9, kSt_B = 35,
#'                   kSt_l = 45, kSt_r = 45)
#'
#' # Calculate total wetted area at water levels 1 m and 2 m
#' h <- c(1, 2)
#' wetted_area(cs, h, ret = "A")
#'
#' # Calculate wetted area for each segment at the same water levels
#' wetted_area(cs, h, ret = "Aii")
#' @export
setMethod("wetted_area", "CSarbitrary", function(object, h, ret = "A") {
  n_segments <- length(x(object)) - 1  # Number of segments

  # Initialize A_h depending on return type
  A_h <- if (ret == "A") rep(NA, length(h)) else matrix(nrow = length(h),
                                                        ncol = n_segments)

  for (jj in seq_along(h)) {
    zh <- min(z(object)) + h[jj]  # Level of h [m a.s.l.]
    Aii <- numeric(n_segments)    # Vector for areas

    for (ii in seq_len(n_segments)) {
      if (z(object)[ii] < zh & z(object)[ii + 1] < zh) {
        # Both points below water level
        Aii[ii] <- 0.5 * ((zh - z(object)[ii]) + (zh - z(object)[ii + 1])) *
          (x(object)[ii + 1] - x(object)[ii])
      } else if (z(object)[ii] >= zh & z(object)[ii + 1] < zh) {
        # One point below water level (riverbank)
        x_inters <- approx(x = z(object)[ii:(ii + 1)], y = x(object)[ii:(ii + 1)],
                           xout = zh)$y
        Aii[ii] <- 0.5 * (zh - z(object)[ii + 1]) * (x(object)[ii + 1] - x_inters)
      } else if (z(object)[ii] < zh & z(object)[ii + 1] >= zh) {
        # One point below water level (riverbank)
        x_inters <- approx(x = z(object)[ii:(ii + 1)], y = x(object)[ii:(ii + 1)],
                           xout = zh)$y
        Aii[ii] <- 0.5 * (zh - z(object)[ii]) * (x_inters - x(object)[ii])
      } else {
        # Both points above water level (dry)
        Aii[ii] <- 0
      }
    }

    if (ret == "A") {
      A_h[jj] <- sum(Aii)
    } else {
      A_h[jj, ] <- Aii
    }
  }

  return(A_h)
})


# Wetted Perimeter
#---------
#' @title Calculates Wetted Perimeter
#' @name wetted_perimeter
#' @aliases wetted_perimeter,CSarbitrary-method
#' @description Calculates the wetted perimeter of a CSarbitrary or CScircle
#'     object for given water levels.
#' @usage wetted_perimeter(object, h, ret = "P")
#' @param object An object of class CSarbitrary or CScircle.
#' @param h A numeric vector of water levels [m].
#' @param ret A character string; if `P`, returns total wetted perimeter. If `Pii`,
#'     returns wetted perimeter by segment.
#' @return A numeric vector or matrix of wetted perimeter based on the `ret`
#'     argument.
#' @importFrom methods new validObject
#' @importFrom stats approx
#' @examples
#' # Define sample cross-section data
#' x <- c(0, 4, 9, 13)
#' z <- c(2, 0, 0, 2)
#' cs <- CSarbitrary(x = x, z = z, xb_l = 4, xb_r = 9, kSt_B = 35,
#'                   kSt_l = 45, kSt_r = 45)
#'
#' # Calculate total wetted perimeter at water levels 1 m and 2 m
#' h <- c(1, 2)
#' wetted_perimeter(cs, h, ret = "P")
#'
#' # Calculate wetted perimeter for each segment at the same water levels
#' wetted_perimeter(cs, h, ret = "Pii")
#' @export



# Wetted Perimeter
#---------
setMethod("wetted_perimeter", "CSarbitrary", function(object, h, ret = "P") {
  n_segments <- length(x(object)) - 1  # Number of segments

  # Initialize P_h depending on return type
  P_h <- if (ret == "P") rep(NA, length(h)) else matrix(nrow = length(h),
                                                        ncol = n_segments)

  for (jj in seq_along(h)) {
    zh <- min(z(object)) + h[jj]  # Level of h [m a.s.l.]
    Pii <- numeric(n_segments)    # Vector for hydraulic radius

    for (ii in seq_len(n_segments)) {
      if (z(object)[ii] < zh & z(object)[ii + 1] < zh) {
        # Both points below water level
        Pii[ii] <- sqrt((x(object)[ii + 1] - x(object)[ii])^2 +
                          (z(object)[ii + 1] - z(object)[ii])^2)
      } else if (z(object)[ii] >= zh & z(object)[ii + 1] < zh) {
        # One point below water level (riverbank)
        x_inters <- approx(x = z(object)[ii:(ii + 1)], y = x(object)[ii:(ii + 1)],
                           xout = zh)$y
        Pii[ii] <- sqrt((x(object)[ii + 1] - x_inters)^2 +
                          (z(object)[ii + 1] - zh)^2)
      } else if (z(object)[ii] < zh & z(object)[ii + 1] >= zh) {
        # One point below water level (riverbank)
        x_inters <- approx(x = z(object)[ii:(ii + 1)], y = x(object)[ii:(ii + 1)],
                           xout = zh)$y
        Pii[ii] <- sqrt((x_inters - x(object)[ii])^2 +
                          (zh - z(object)[ii])^2)
      } else {
        # Both points above water level (dry)
        Pii[ii] <- 0
      }
    }

    if (ret == "P") {
      P_h[jj] <- sum(Pii)
    } else {
      P_h[jj, ] <- Pii
    }
  }

  return(P_h)
})
