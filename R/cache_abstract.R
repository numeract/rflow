# cache engine (abstract methods - eddy calls only these)


# !diagnostics suppress=


R6Cache <- R6::R6Class(
    classname = "R6Cache",
    public = list(
        initialize = function() {stop("abstract method")},
        
        list_groups = function() {stop("abstract method")},
        has_group = function(group) {stop("abstract method")},
        # no get_group()
        add_group = function(group) {stop("abstract method")},
        delete_group = function(group) {stop("abstract method")},
        
        list_keys = function(group) {stop("abstract method")},
        has_key = function(group, key) {stop("abstract method")},
        
        get_data = function(group, key) {stop("abstract method")},
        add_data = function(group, key, value) {stop("abstract method")},
        delete_data = function(group, key) {stop("abstract method")},
        
        reset = function() {stop("abstract method")},
        sync = function() {stop("abstract method")}
    )
)
