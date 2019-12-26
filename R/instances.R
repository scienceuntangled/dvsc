## construct all instances pertaining to a rally (point_id)
make_rally_instances_txt <- function(rx, starting_instance_id) {
    start_end <- start_end_times(rx)
    if (any(is.na(start_end))) return(list(instances = NULL, instance_ids = NULL))
    instance_id <- starting_instance_id
    iids <- instance_id
    out <- make_instance_txt(rx, instance_id = instance_id, type = "rally_summary", start_end = start_end)
    ## details for overall rally
    inst0 <- make_instance_txt(rx, type = "rally", start_end = start_end)
    ttids <- unique(na.omit(rx$team_touch_id))
    for (ttid in ttids) {
        rxtt <- rx[rx$team_touch_id %eq% ttid, ]
        ttinst <- make_instance_txt(rxtt, inst = inst0, type = "team_touch", start_end = start_end)
        for (i in seq_len(nrow(rxtt))) {
            instance_id <- instance_id + 1
            this <- make_instance_txt(rxtt[i, ], inst = ttinst, instance_id = instance_id, type = "action")
            if (is.null(this)) {
                instance_id <- instance_id - 1
            } else {
                out <- c(out, this)
                iids <- c(iids, instance_id)
            }
        }
    }
    list(instances = out, instance_ids = iids)
}

## build an instance, starting either from nothing or from an existing skeleton to which we add stuff
make_instance_txt <- function(type, rx, inst = NULL, instance_id, start_end) {
    if (is.null(inst)) {
        ## not provided, generate new one
        ## elements that are common to all instances including "Rally" instances
        inst <- simpletags(start = start_end[1], end = start_end[2])
        serve_type <- rx$skill_type[rx$skill %eq% "Serve"]
        ## TODO serve type, receive grade only on serve/reception phase rows
        if (length(serve_type) == 1) inst <- c(inst, make_label_txt(group = "Serve Type", text = serve_type))
        rcv_grade <- rx$evaluation[rx$skill %eq% "Reception"]
        if (length(rcv_grade) == 1) inst <- c(inst, make_label_txt(group = "Receive Grade", text = rcv_grade))
        inst <- c(inst, make_label_txt(group = "Game Score", text = game_score_text(rx)))
        ## todo: rotation should optionally follow US system, not international
        inst <- c(inst, make_label_txt(group = paste0(rx$home_team_id[1], " Rotation"), text = paste0("Rotation ", first_nonNA(rx$home_rotation))))
        inst <- c(inst, make_label_txt(group = paste0(rx$visiting_team_id[1], " Rotation"), text = paste0("Rotation ", first_nonNA(rx$visiting_rotation))))
    }
    if (missing(type) || is.null(type)) {
        ## do nothing
    } else if (tolower(type) %eq% "rally_summary") {
        inst <- instance_add_rally_summary_txt(inst, rx)
        inst <- tag("instance", c(inst, tag("id", instance_id)))
    } else if (tolower(type) %eq% "rally") {
        inst <- instance_add_rally_txt(inst, rx)
    } else if (tolower(type) %eq% "team_touch") {
        ## rx here is team touch rows
        inst <- instance_add_team_touch_txt(inst, rx)
    } else if (tolower(type) %eq% "action") {
        ## instance is for a specific action within team touch
        ## rx should be single row now
        this_inst <- instance_add_common_txt(inst = inst, rxi = rx)
        ## then specific to action
        if (rx$skill %in% c("Serve", "Reception", "Attack", "Block", "Set", "Dig", "Cover")) {
            inst <- instance_add_action_txt(this_inst, rxi = rx, rx$skill)
        } else {
            ## not handled (yet)
            inst <- NULL
        }

## "Ball Over" "Cover" "Save" "Substitution"
## "Defense First Ball" "Defense Transition" "Downball"
## "Freeball" "General Error" "Offense First Ball" "Offense Transition"

        if (!is.null(inst)) inst <- tag("instance", c(inst, tag("id", instance_id)))
    } else {
        stop("unexpected instance type: ", type)
    }
    inst
}

## add rally details to an instance
instance_add_rally_summary_txt <- function(inst, rx) {
    c(inst, tag("code", "Rally"),
      make_label_txt(group = "Phase", text = "All"),
      make_label_txt(group = "Team", text = "All"),
      make_label_txt(group = "Team Name", text = "All"))
}

## add details about the overall rally to an instance
instance_add_rally_txt <- function(inst, rx) {
    rcvx <- rx[rx$skill %eq% "Reception", ]
    if (nrow(rcvx) == 1) {
        inst <- c(inst, make_label_txt(group = "Receiver", text = rcvx$player_name))
        ## is this for all touches in the rally, or only reception-phase?
    }
    inst
}

## add details about the team touch to an instance
instance_add_team_touch_txt <- function(inst, rxtt) {
    atx <- rxtt[rxtt$skill %eq% "Attack", ]
    if (nrow(atx) == 1) {
        if (!is.na(atx$skill_subtype)) inst <- c(inst, make_label_txt(group = "Attack Style", text = atx$skill_subtype))
        if (!is.na(atx$player_name)) inst <- c(inst, make_label_txt(group = "Attacker", text = atx$player_name))
        if (atx$start_zone %eq% 4) {
            inst <- c(inst, make_label_txt(group = "Attack Location", text = "Left Side"))
        } else if (atx$start_zone %eq% 2) {
            inst <- c(inst, make_label_txt(group = "Attack Location", text = "Right Side"))
        } else if (atx$start_zone %eq% 3) {
            ## these should be by tempo, not zone?
            inst <- c(inst, make_label_txt(group = "Attack Location", text = "Middle"))
        } else if (atx$start_zone %eq% 8) {
            inst <- c(inst, make_label_txt(group = "Attack Location", text = "Pipe"))
        } else if (atx$start_zone %eq% 9) {
            inst <- c(inst, make_label_txt(group = "Attack Location", text = "Right Side Back"))
        }
    }
    setx <- rxtt[rxtt$skill %eq% "Set", ]
    if (nrow(setx) == 1) {
        inst <- c(inst, make_label_txt(group = "Assist", text = setx$player_name))
        ## Attack Type (In System, Out of System)
        attack_type <- if (setx$player_id %eq% setx$setter_player_id) "In System" else "Out of System"
        inst <- c(inst, make_label_txt(group = "Attack Type", text = attack_type))
        ## TODO: NA as "No Middle"?
        if (!is.na(setx$set_description)) inst <- c(inst, make_label_txt(group = "Middle Route", text = setx$set_description))
    }
    inst
}

## add groups that are common for single-action instances, but which
##  don't have the same text values across different actions within a set of team touches
instance_add_common_txt <- function(inst, rxi) {
    inst <- c(inst, make_label_txt(group = "Rally Won", text = if (rxi$point_won_by %eq% rxi$team) "Won" else "Lost"))
    if (rxi$team %eq% rxi$home_team) {
        inst <- c(inst, make_label_txt(group = "Team", text = "Home"))
    } else if (rxi$team %eq% rxi$visiting_team) {
        inst <- c(inst, make_label_txt(group = "Team", text = "Away"))
    }
    inst <- c(inst, make_label_txt(group = "Team Name", text = rxi$team))
    inst <- c(inst, make_label_txt(group = "Phase", text = rxi$phase))
    ## player of this action
    inst <- c(inst, make_label_txt(group = paste0(rxi$team_id, " Player Name"), text = rxi$player_name))
    inst <- c(inst, make_label_txt(group = paste0(rxi$team_id, " Player Jersey"), text = rxi$player_number))
    ## action grade
    inst <- c(inst, make_label_txt(group = "Grade", text = rxi$evaluation))
    inst
}

## add details specific to the action
instance_add_action_txt <- function(inst, rxi, type) {
    ## rxi is a single row
    inst <- instance_add_common_txt(inst = inst, rxi = rxi)
    if (tolower(type) %in% c("serve", "set", "attack", "block", "dig", "cover")) {
        inst <- c(inst, tag("code", paste0(rxi$team_id, " ", type)))
    } else if (tolower(type) == "reception") {
        inst <- c(inst, tag("code", paste0(rxi$team_id, " Receive")))
    } else {
        warning("unhandled action type: ", type)
    }
    inst
}
