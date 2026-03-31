# RAgree functions as package missing

#' Coefficient of unalikeability
#' 
#' Function to calculate the unalikeability coefficient to quantify the amount 
#' of variability in categorical data.
#' 
#' @param x a vector of categorical data
#' 
#' alternatively, an \code{n x r} matrix of \code{n} subjects by \code{r}
#' raters or an \code{n x m} data frame of \code{n} subjects; when \code{x}
#' is a data  frame, the user must specify which columns correspond to
#' \code{id}, \code{rater}, and \code{score}
#' @param ... additional arguments passed to or from other methods
#' @param method the method for calculating the unalikeability coefficient;
#' see details
#' @param id,rater,score column names corresponding to IDs, raters, and scores
#' @param summary logical; if \code{TRUE}, prints summary statistics for the
#' unalikeability coefficients
#' @param plot logical; if \code{TRUE}, prints a heat map of unalikeability
#' coefficients
#' 
#' @return A list containing the following:
#' 
#' \tabular{rlll}{
#' \tab \code{$method} \tab agreement method \cr
#' \tab \code{$ragree.name} \tab method type \cr
#' \tab \code{$subjects} \tab number of subjects \cr
#' \tab \code{$raters} \tab number of raters \cr
#' \tab \code{$categories} \tab number of categories \cr
#' \tab \code{$value} \tab median of all unalikeability coefficients \cr
#' \tab \code{$summary} \tab a data frame with the summary information
#' printed when \code{summary = TRUE} \cr
#' \tab \code{$data} \tab a long data frame with all coefficients \cr
#' }
#' 
#' @details
#' The coefficient of unalikeability describes a concept of variability for 
#' categorical variables and provides a quantitative method for its 
#' measurement. A smaller coefficient is better corresponding to less variation
#' in the scores.
#' 
#' For the case of a finite number of observations (\code{n}), a finite number 
#' of categories (\code{m}) and a finite number of objects, \eqn{k_i}, within
#' category \code{i}, will allow expression of the coefficient of unalikeablity
#' as:
#' 
#' \deqn{u = 1 - \sum p_i ^ 2} where \eqn{p_i = k_i / n}.
#' 
#' The interpretation of \eqn{u} is that it represents the proportion of 
#' possible comparisons (pairings) which are unalike. Note that \eqn{u}
#' includes comparisons of each response with itself.
#' 
#' Currently, two methods for calculating the coefficient are implemented. If
#' \code{method = 1}, then the formula described above is used. If
#' \code{method = 2}, then the formula described in Perry (2005).
#' 
#' @seealso
#' \code{RcmdrPlugin.ISCSS::unalike}
#' 
#' @author
#' Robert Redd \email{rredd@@jimmy.harvard.edu}
#' 
#' @references
#' Kader, GD. Variability for Categorical Variables. \emph{Journal of
#' Statistics Education}, Vol. 15, No. 2 (2007).
#' 
#' Perry, M. and Kader, G. Variation as Unalikeability. \emph{Teaching
#' Statistics}, Vol. 27, No. 2 (2005), pp. 58-60.
#' 
#' @examples
#' unalike(1, 2)
#' unalike(rep(1, 10))
#' 
#' 
#' ## examples in Kader (2007):
#' l <- list(
#'   group1 = rep(c('A', 'B'), c(7, 3)),
#'   group2 = rep(c('A', 'B'), c(5, 5)),
#'   group3 = rep(c('A', 'B'), c(1, 9)),
#'   group4 = rep(c('A', 'B', 'C'), c(2, 3, 5))
#' )
#' 
#' sapply(l, unalike)
#' 
#' 
#' ## matrix/data frames are assumed to be subjects x raters
#' mat <- do.call('cbind', l[1:3])
#' unalike(mat) ## see Kader
#' 
#' 
#' library('irr')
#' data(diagnoses)
#' 
#' kappam.fleiss(diagnoses)
#' unalike(as.matrix(diagnoses))
#' 
#' library('ggplot2')
#' unalike(as.matrix(diagnoses), plot = TRUE)
#' 
#' 
#' dat <- data.frame(
#'   id    = rep(seq.int(nrow(diagnoses)), ncol(diagnoses)),
#'   rater = rep(names(diagnoses), each = nrow(diagnoses)),
#'   score = unlist(diagnoses)
#' )
#' unalike(dat)
#' 
#' @export

unalike <- function(x, ...) {
  UseMethod('unalike')
}

unalike1 <- function(x) {
  ## helper function to calculate the unalikeability coefficient
  x <- if (inherits(x, 'table'))
    x else table(x)
  1 - sum(prop.table(x) ^ 2)
}

unalike2 <- function(x) {
  x <- if (inherits(x, 'table'))
    untable(x) else x
  n <- length(x)
  o <- outer(x, x, `!=`)
  # mean(o[lower.tri(o)])
  sum(o[row(o) != col(o)]) / (n ^ 2 - n)
}

untable <- function(x) {
  stopifnot(
    inherits(x, 'table'),
    length(dim(x)) == 1L
  )
  rep(seq_along(x), x)
}

#' @rdname unalike
#' @export
unalike.default <- function(x, ..., method = 1L) {
  x <- if (inherits(x, 'table'))
    x else c(x, ...)
  
  if (method == 1L)
    unalike1(x)
  else if (method == 2L)
    unalike2(x)
  else stop('Invalid method - should be 1 or 2', call. = FALSE)
}

#' @rdname unalike
#' @export
unalike.matrix <- function(x, ...) {
  x <- as.data.frame(cbind(`_id_` = seq.int(nrow(x)), x))
  x <- reshape(x, idvar = '_id_', varying = list(2:ncol(x)),
               direction = 'long')
  names(x) <- c('id', 'rater', 'score')
  
  unalike(x, id = 'id', rater = 'rater', score = 'score', ...)
}

#' @rdname unalike
#' @export
unalike.data.frame <- function(x, id = 'id', rater = 'rater', score = 'score',
                               summary = TRUE, plot = FALSE, ...) {
  if (any(idx <- !(c(id, rater, score) %in% names(x))))
    stop(
      sprintf('%s not found in data', toString(c(id, rater, score)[idx]))
    )
  
  ## calculate the unalikeability coefficient for each id
  x$unalike <- ave(as.numeric(as.factor(x[[score]])), list(x[[id]]),
                   FUN = function(ii) unalike(ii, ...))
  ua <- x[!duplicated(x[[id]]), ]$unalike
  
  res <- data.frame(
    LCI    = quantile(ua, probs = 0.025),
    Min    = min(ua),
    Median = median(ua),
    Mean   = mean(ua),
    Max    = max(ua),
    UCI    = quantile(ua, probs = 0.975)
  )
  
  if (summary) {
    cat('\nSummary of unalikeability coefficients:\n\n')
    print(res, row.names = FALSE, digits = 3L)
    cat('\n\n')
  }
  
  if (plot) {
    x$id <- as.factor(x$id)
    
    p <- ggplot(x, aes(x = factor(rater), y = rev(id))) + 
      geom_tile(aes(fill = unalike), colour = 'white') + 
      scale_fill_gradient(name = 'Un-alikeability\ncoefficient',
                          limits = c(0, 1), 
                          low = 'white', 
                          high = 'steelblue') +
      scale_x_discrete(expand = c(0, 0)) +
      scale_y_discrete(labels = rev(unique(x$id)), expand = c(0, 0)) +
      theme_bw() + 
      theme(legend.position = 'right',
            axis.text.x = element_text(size = 15, colour = 'grey50'),
            axis.title.x = element_blank(),
            axis.title.y = element_blank())
    
    print(p)
  }
  
  res <- list(
    method      = sprintf('Unalikeability coefficient for %s raters',
                          nrow(x) / length(unique(x[[id]]))),
    ragree.name = 'Unalike',
    subjects    = length(unique(x[[id]])),
    raters      = nrow(x) / length(unique(x[[id]])),
    categories  = length(unique(x[[score]])),
    value       = median(x[['unalike']]),
    summary     = res,
    data        = x
  )
  
  structure(res, class = 'ragree')
}

vname <- function(name) paste(name, group, sep = "_")
vsname <- function(name) paste(name, stratum, group, sep = "_")

# This first function to estimate the Mode is created to deal with matrices were the columns are the maturity stage, and the content of the cell the number of readers that decided each maturity stage. This function Mode_I goes with the function cv_I
Mode_I <- function(x) {
  names(sort(x, decreasing = TRUE)[1])
}

# This second function to estimate the Mode is created to deal with matrices were the columns are the readers, and the content of the cell the maturity stage decided by each reader. This function Mode_II goes with the function cv_II
Mode_II <- function(x) {
  names(sort(table(x), decreasing = TRUE)[1])
}

# ape <- function(x) {
#   if (length(x) == 0) {
#     return(numeric(0))
#   }
#   if (Mode(x) == 0) {
#     NA
#   } else {
#     100 * mean(abs((x - mean(x, na.rm = TRUE)) / mean(x, na.rm = TRUE)), na.rm = TRUE)
#   }
# }

# Estimate of variance in categorical variables: Coefficient of unalikeability (see paper of Kader and Perry in https://www.tandfonline.com/doi/full/10.1080/10691898.2007.11889465?scroll=top&needAccess=true. It is estimated with the function unalike, of package Ragree.

# This first function to estimate the coefficient of unalikeability is created to deal with matrices were the columns are the maturity stage, and the content of the cell the number of readers that decided each maturity stage. This function cu_I goes with the function Mode_I, that estimates the mode for matrices equally designed (i.e. columns are the maturity stage, and the content of the cell the number of readers that decided each maturity stage, while rows are the fishID)
cu_I <- function (x) {
  if (length(x) == 0) {
    return(numeric(0))
  }
  if (Mode_I(x) == 0) {
    NA
  } else {
    dat=rep.int(names(x[which(x>0)]),times=x[which(x>0)])
    unalike(dat, na.rm = TRUE)
  }
}


# This second function to estimate the coefficient of unalikeability is created to deal with matrices were the columns are the readers, and the content of the cell the maturity stage decided by each reader. This function cu_II goes with the function Mode_II, that estimates the mode for matrices equally designed (i.e. columns are readers and content is the maturity stage decided by each reader, while rows are the fishID)
cu_II <- function (x) {
  if (length(x) == 0) {
    return(numeric(0))
  }
  if (Mode_II(x) == 0) {
    NA
  } else {
    unalike(x)
  }
}


capFirst <- function(s) {
    paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
}

