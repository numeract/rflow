# functions to indicate data sources for rflow (monitor for changes, etc.)


# !diagnostics suppress=.


file_source <- function(file_path,
                        eddy = get_default_eddy()
) {
    stopifnot(rlang::is_scalar_character(file_path))
    stopifnot(inherits(eddy, "R6Eddy"))
    
    # we must provide a function to init R6Flow, use the simplest fn possible
    # fn_name is the name of the file to watch and determines fn_key
    fn_key <- eddy$digest(file_path)
    
    if (eddy$find_rflow(fn_key) == "memory") {
        rflow <- eddy$get_rflow(fn_key)
    } else {
        rflow <- R6Flow$new(
            fn = base::identity,
            fn_key = fn_key,
            fn_name = paste0("source:", file_path),
            hash_input_fn = NULL,
            split_output_fn = NULL,
            eddy = eddy
        )
    }
    
    # run a simplified version of `rf_fn`
    if (!file.exists(file_path)) {
        in_hash <- eddy$digest(object = NULL)
    } else {
        in_hash <- eddy$digest(object = file_path, file = TRUE)
    }
    
    found_state_idx <- rflow$find_state_index(in_hash)
    if (found_state_idx > 0L) {
        rflow$state_index <- found_state_idx
        rflow$save()
    } else {
        # the output is the same for all in_hash: the file_path
        out_data <- list(value = file_path, visible = TRUE)
        out_hash <- eddy$digest(out_data)
        rflow$add_state(in_hash, out_hash)
        eddy$add_data(out_hash, out_data, fn_key)
        rflow$save()
    }
    
    # similar to `rf_fn`, return the R6flow object
    rflow
}
