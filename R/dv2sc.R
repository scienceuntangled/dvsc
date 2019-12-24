#' Create sc file from dvw file
#'
#' @param x datavolley or data.frame: a datavolley object as returned by [datavolley::read_dv()], or the plays component of that object
#' @param destfile string: destination file name. If `NULL`, the function will return the raw XML rather than the filename
#' @param home_team_short string: the short name to use for the home team. If missing or `NULL`, the `home_team_id` from `x` will be used
#' @param visiting_team_short string: the short name to use for the visiting team. If missing or `NULL`, the `visiting_team_id` from `x` will be used
#' @param rotation_style string: "international" or "USA". Using the USA style, rotation 1 is when the setter is in position 1 and rotation 2 is when they are in position 6. Using the international style, rotation 1 is when the setter is in position 1 and rotation 2 is when they are in position 2.
#' @return The path to the created XML file, or if `destfile` was `NULL`, the XML itself as a character vector.
#' @seealso [datavolley::read_dv()]
#'
#' @examples
#' x <- read_dv(dv_example_file())
#' sx <- dv2sc(x)
#'
#' @export
dv2sc <- function(x, destfile = tempfile(fileext = ".xml"), home_team_short, visiting_team_short, rotation_style = "international") {
    if (!is.null(destfile)) {
        assert_that(is.string(destfile))
    }
    if (inherits(x, "datavolley") || (is.list(x) && "plays" %in% names(x))) {
        x <- datavolley::plays(x)
    }
    if (missing(home_team_short) || is.null(home_team_short)) {
        home_team_short <- first_nonNA(x$home_team_id)
    } else {
        ## change all team_id and home_team_id values to match
        assert_that(is.string(home_team_short))
        x$team_id[x$team_id %eq% x$home_team_id] <- home_team_short
        x$home_team_id <- home_team_short
    }
    if (missing(visiting_team_short) || is.null(visiting_team_short)) {
        visiting_team_short <- first_nonNA(x$visiting_team_id)
    } else {
        ## change all team_id and visiting_team_id values to match
        assert_that(is.string(visiting_team_short))
        x$team_id[x$team_id %eq% x$visiting_team_id] <- visiting_team_short
        x$visiting_team_id <- visiting_team_short
    }
    ## add some columns to x
    ## if we don't have player roles, need to infer setter player ID from rotation
    setter_player_id <- rep(NA_character_, nrow(x))
    setterpos <- x$home_setter_position
    idx <- x$team %eq% x$home_team & !is.na(setterpos)
    for (pos in 1:6) {
        pidx <- idx & setterpos %eq% pos
        setter_player_id[pidx] <- x[[paste0("home_player_id", pos)]][pidx]
    }
    setterpos <- x$visiting_setter_position
    idx <- x$team %eq% x$visiting_team & !is.na(setterpos)
    for (pos in 1:6) {
        pidx <- idx & setterpos %eq% pos
        setter_player_id[pidx] <- x[[paste0("visiting_player_id", pos)]][pidx]
    }
    x$setter_player_id <- setter_player_id
    ## just in case set codes aren't given descriptions
    idx <- !is.na(x$set_code) & is.na(x$set_description)
    if (any(idx)) x$set_description[idx] <- x$set_code[idx]

    ## rotation
    assert_that(is.string(rotation_style))
    rotation_style <- match.arg(tolower(rotation_style), c("international", "usa"))
    if (rotation_style %eq% "international") {
        x$home_rotation <- x$home_setter_position
        x$visiting_rotation <- x$visiting_setter_position
    } else {
        x$home_rotation <- ((1-x$home_setter_position) %% 6) + 1
        x$visiting_rotation <- ((1-x$visiting_setter_position) %% 6) + 1
    }

    ## create instances for each rally
    play_point_ids <- sort(unique(x$point_id[x$skill %eq% "Serve"]))
    px <- x[x$point_id %in% play_point_ids, ]
    iid <- 1
    alli <- c()
    for (ppid in play_point_ids) {
        temp <- make_rally_instances_txt(px[px$point_id %eq% ppid & !is.na(px$skill), ], starting_instance_id = iid)
        alli <- c(alli, temp$instances)
        ## next ID to use
        if (!is.null(temp$instance_ids)) {
            iid <- max(temp$instance_ids)+1
        }
    }
    ixml <- c("<ALL_INSTANCES>", alli, "</ALL_INSTANCES>")

    ## build rows xml
    cx <- row_colours()
    cx$team[cx$team %eq% "home"] <- home_team_short
    cx$team[cx$team %eq% "visiting"] <- visiting_team_short
    idx <- !is.na(cx$team)
    cx$code[idx] <- paste(cx$team[idx], cx$code[idx], sep = " ")
    rxml <- c("<ROWS>", vapply(seq_len(nrow(cx)), function(i) make_row_txt(cx[i, ]), FUN.VALUE = ""), "</ROWS>")

    ## put the whole thing together
    sc <- c("<file><script/>", tag("SORT_INFO", tag("sort_type", "sort order")), ixml, rxml, "</file>")
    if (!is.null(destfile)) {
        ##cat(as.character(sc), file = destfile)
        cat(c(sc, "\n"), file = destfile, sep = "")
        destfile
    } else {
        sc
    }
}

##<file><script/>
##  <SORT_INFO><sort_type>sort order</sort_type></SORT_INFO>
##  <ALL_INSTANCES>
##    <instance> ... </instance>
##    <instance> ... </instance>
##    ...
##    <instance> ... </instance>
##  </ALL_INSTANCES>
##  <ROWS>
##    <row>
##      <sort_order>1</sort_order>
##      <code>Rally</code>
##      <R>0</R>
##      <G>24310</G>
##      <B>50358</B>
##    </row>
##    <row>
##      ...
##    </row>
##    ...
##    <row>
##      ...
##    </row>
##  </ROWS>
##</file>
