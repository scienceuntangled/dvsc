#' Preprocess a DataVolley object before creating a Sportscode file
#'
#' @param x datavolley: a datvolley object as returned by [read_dv]
#' @param cover logical: convert the `skill` label of any dig that follows a block to "Cover"
#'
#' @return A modified version of `x`
#'
#' @seealso \code{\link{dv2sc}}
#'
#' @examples
#'
#' x <- read_dv(dv_example_file())
#' x <- sc_preprocess(x)
#'
#' @export
sc_preprocess <- function(x, cover = TRUE) {
    if (inherits(x, "datavolley") || (is.list(x) && "plays" %in% names(x))) {
        xp <- datavolley::plays(x)
    } else {
        stop("x should be a datavolley object or equivalent")
    }
    assert_that(is.flag(cover), !is.na(cover))
    if (cover) {
        ## digs
        idx <- which(xp$skill %eq% "Dig")
        idx <- idx[idx > 1]
        ## that were preceded by a block in the same rally
        idx <- idx[xp$skill[idx-1] %eq% "Block" & xp$point_id[idx] %eq% xp$point_id[idx-1]]
        xp$skill[idx] <- "Cover"
    }
    x$plays <- xp
    x
}

