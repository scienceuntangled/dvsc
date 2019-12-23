tag <- function(name, ..., sep = "") {
    n <- list(...)
    if (length(n) == 1 && is.null(n[[1]])) glue("<{name} />") else paste0(glue("<{name}>"), paste(..., sep = sep, collapse = sep), glue("</{name}>"))
}

simpletags <- function(...) {lx <- list(...); vapply(seq_along(lx), function(z) paste0("<", names(lx)[z], ">", lx[[z]], "</", names(lx)[z], ">"), FUN.VALUE = "") }

make_label_txt <- function(group, text) {
    glue("<label><group>{group}</group><text>{text}</text></label>")
}

make_row_txt <- function(x) {
    tag("row", simpletags(sort_order = x$sort_order, code = x$code, R = x$R, G = x$G, B = x$B))
}

first_nonNA <- function(z) na.omit(z)[1]

game_score_text <- function(rx) {
    game_score <- max(rx$home_team_score, rx$visiting_team_score, na.rm = TRUE)
    if (game_score <= 10) "0-10" else if (game_score <= 19) "11-19" else "20+"
}

start_end_times <- function(rx) if (!all(is.na(rx$video_time))) range(rx$video_time, na.rm = TRUE) else c(NA, NA)

`%eq%` <- function(x,y) { x==y & !is.na(x) & !is.na(y) }
