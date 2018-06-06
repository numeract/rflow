# constants / globals for all functions


# force dir delete?
DIR_DELETE_FORCE = TRUE
# how long to wait (in seconds) after deleting a dir and recreating it
DIR_DELETE_WAIT = 0.1


# create a separate binding environment to keep eddies
.EDDY_ENV <- new.env(parent = emptyenv())
# Each eddy_env has a string variable .CURRENT_NAME that stores the name
# of the current eddy. This to avoid duplicate binding of the eddy_end
# to help with gc()
.EDDY_ENV[[".CURRENT_NAME"]] <- NA_character_


# flow
.STATE_KEY = "_state"


# flow_dfr
.ROW_CACHE = "_row_cache"
ROW_HASH <- "..row_hash.."


# flow_dfg
ROW_ID <- "..row_id.."
GROUP_HASH <- "..group_hash.."
