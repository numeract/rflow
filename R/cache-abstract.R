# cache engine (abstract methods - eddy calls only these)


# !diagnostics suppress=., self, private

# force dir delete?
DIR_DELETE_FORCE = TRUE
# how long to wait (in seconds) after deleting a dir and recreating it
DIR_DELETE_WAIT = 0.1


# nocov start
R6Cache <- R6::R6Class(
    classname = "R6Cache",
    public = list(
        initialize = function() {stop("abstract method")},
        
        list_groups = function() {stop("abstract method")},
        has_group = function(group) {stop("abstract method")},
        # no get_group() function
        add_group = function(group) {stop("abstract method")},
        forget_group = function(group) {stop("abstract method")},
        delete_group = function(group) {stop("abstract method")},
        
        list_keys = function(group) {stop("abstract method")},
        has_key = function(group, key) {stop("abstract method")},
        
        get_data = function(group, key) {stop("abstract method")},
        add_data = function(group, key, value) {stop("abstract method")},
        delete_data = function(group, key) {stop("abstract method")},
        
        summary = function() {stop("abstract method")},
        print = function() {},
        reset = function() {stop("abstract method")},
        terminate = function() {stop("abstract method")}
    )
)


# print ----
R6Cache$set("public", "print", function() {
    
    df <- self$summary()
    
    emph_obj <- paste0("<", crayon::italic(class(self)[[1L]]), ">")
    cat(emph_obj, "with", crayon::bold(nrow(df)), "fn_keys\n")
    print(df)
    
    invisible(self)
}, overwrite = TRUE)
# nocov end
