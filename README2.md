
<!-- README.md is generated from README.Rmd. Please edit that file -->

``` r
sample_code <- 
'library(tidyverse)
iris %>%
  group_by(Species) %>%
  summarize(mean(Sepal.Length))'
```

``` r
library(tidyverse)
```

    #> ── Attaching core tidyverse packages ─────────────────── tidyverse 2.0.0.9000 ──
    #> ✔ dplyr     1.1.0     ✔ readr     2.1.4
    #> ✔ forcats   1.0.0     ✔ stringr   1.5.0
    #> ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
    #> ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    #> ✔ purrr     1.0.1     
    #> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    #> ✖ dplyr::filter() masks stats::filter()
    #> ✖ dplyr::lag()    masks stats::lag()
    #> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
iris %>%
  group_by(Species) %>%
  summarize(mean(Sepal.Length))
```

    #> # A tibble: 3 × 2
    #>   Species    `mean(Sepal.Length)`
    #>   <fct>                     <dbl>
    #> 1 setosa                     5.01
    #> 2 versicolor                 5.94
    #> 3 virginica                  6.59

```` r
return_chunk_code <- function(chunk_name,
                           eval = TRUE, echo = TRUE, include = TRUE, ...) {

  my_code <- NULL

  is_live <- FALSE

  # Check to see if we're in editor context
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {

    is_live <- tryCatch({
      rstudioapi::getSourceEditorContext()
      TRUE
    }, error = function(e) FALSE)

  }

  try_chunk <- purrr::safely(knitr::knit_code$get)(chunk_name)


  # If that worked, yay we're good
  if (is.null(try_chunk$error) && !is.null(try_chunk$result)) {

    my_code <- str_c(try_chunk$result, collapse = "\n")

    my_code <- stringr::str_trim(my_code)

    my_opts <- as.list(attributes(try_chunk$result)$chunk_opts)

    # Avoid knitr's duplicate chunk label error by appending "-flaired" to the
    # chunk label before rendering with knit_child
    my_label <- paste0(my_opts[["label"]], "-flaired")
    # If the same chunk is decorated twice, or if the user by chance has labeled
    # a chunk of the same name plus "-flaired", add a random string to ensure
    # uniqueness
    if (my_label %in% knitr::all_labels()) {
      random <- sample(c(0:9, letters), 7, replace = TRUE)
      random <- paste(random, collapse = "")
      my_label <- paste0(my_label, "-", random)
    }
    # Remove the label from the chunk options. Required for properly forming
    # my_code below.
    my_opts <- within(my_opts, rm("label"))


  } else if (is_live) {  # If that failed, try the editor pull

    ed <- rstudioapi::getSourceEditorContext()
    sources <- ed$contents

    my_code <- code_from_editor(sources, chunk_name)

  }

  # If neither of those worked, error
  if (is.null(my_code)) {

    stop(paste0("Error: No chunk found with name '", chunk_name, "'"))

  }

  # In editor, don't evaluate, but return source code for preview no matter what
  # In knitting, knit it as the options suggest.
  if (is_live) {

    # Don't bother evaluating if in editor

    my_code <- stringr::str_replace(my_code, fixed("}"),
                                    ", eval = FALSE, echo = TRUE, include = TRUE}")

    # knitted <- knitr::knit(text = my_code,
    #                        quiet = TRUE)

  } 
  

    # # Check for flair = FALSE option... for now, this will just exclude flair chunks
    # if (!is.null(my_opts$flair) && !my_opts$flair) {
    # 
    #   placeholder <- new_decorated(NULL)
    # 
    #   return(placeholder)
    # 
    # } else {
    # 
    #   # If engine isn't overwritten, it's R
    #   if (is.null(my_opts[["engine"]])) {
    # 
    #     my_engine = "r"
    # 
    #   } else {
    # 
    #     my_engine <- my_opts[["engine"]]
    #     my_opts <- within(my_opts, rm("engine"))
    # 
    #   }

      # If there are special options, write them into the chunk.

      # if (length(my_opts) > 1) {
      # 
      #   my_code <- paste0("```{", my_engine, " ", my_label, ", ",
      #                     toString(list_to_strings(my_opts)),
      #                     "}\n", my_code, "\n```")
      # } else {
      # 
      #   my_code <- paste0("```{", my_engine,  " ", my_label, "}\n", my_code, "\n```")
      # 
      # }

      # knitted <- knitr::knit_child(text = my_code,
                                 # quiet = TRUE)

    # } # flair = FALSE or not

  # } # live in editor or not

  # new_decorated(knitted, my_code, chunk_name)
  
  if(is_live){
     return(my_code)
  }
  
  
  if(!is_live){
    return(knitr::knit_code$get(name = chunk_name))
  }
  

}


#' Takes plain text of knitted code and converts it to a list, in which code
#' sources have the class \code{source}.
#'
#' @param knitted Text of knitted code
#'
#' @return A list with code sources and knitted output.
#'
#' @export
src_to_list <- function(knitted) {

  knitted <- knitted %>%
    split_sandwiches("```[A-z]*") %>%
    as.list()

  before_code <- which(stringr::str_detect(knitted, "```[A-z]+"))

  knitted[before_code + 1] <- stringr::str_trim(knitted[before_code + 1])

  knitted[before_code + 1] <- purrr::map(knitted[before_code + 1],
                                         as_decorated_source)

  knitted <- knitted[-c(before_code, before_code + 2)]

  return(knitted)

}


code_from_editor <- function(.contents, chunk_name) {


  # Find the start of the desired chunk
  chunk_regex <- paste0('\\`\\`\\`\\{[A-z]+ ', chunk_name, '(\\}|(,.*\\}))$')

  start_chunk <- .contents %>%
    str_which(chunk_regex)

  if (length(start_chunk) == 0) {

    stop(paste0("Error: No chunk found with name '", chunk_name, "'"))

  } else if (length(start_chunk) > 1) {

    stop(paste0("Error: Duplicate chunk name '", chunk_name, "'"))

  }

  end_chunk <- .contents[-c(1:start_chunk)] %>%
    str_which(fixed("```")) %>%
    min() + start_chunk

  chunk_text <- .contents[(start_chunk):(end_chunk)] %>%
    str_c(collapse = "\n")

  attributes(chunk_text) <- NULL

  return(chunk_text)

}

#   
# 
# #' Converts list to vector of strings
# #'
# #' Helper for decorate_chunk
# #'
# #' @param opts_list A list, presumably of chunk options
# #'
# #' @return A character vector
# list_to_strings <- function(opts_list) {
# 
#   paste(names(opts_list), opts_list, sep = " = ")
# 
# }
# 
# #' Compares current global chunk options to defaults;
# #' finds options that differ from default.
# #'
# #' Helper for decorate_chunk
# #'
# #' @return A character vector
# get_new_globals <- function() {
# 
#   default_opts <- knitr::opts_chunk$get(default = TRUE)
#   current_opts <- knitr::opts_chunk$get()
# 
#   names_match <- names(current_opts) %in% names(default_opts)
#   keep_1 <- current_opts[!names_match]
# 
#   ol_names <- intersect(names(current_opts), names(default_opts))
#   current_opts <- current_opts[ol_names]
#   default_opts <- default_opts[ol_names]
# 
#   eh <- list_to_strings(current_opts) != list_to_strings(default_opts)
# 
#   keep_2 <- current_opts[eh]
# 
#   c(keep_1, keep_2)
# 
# }
#   
# 
# new_decorated <- function(knitted, code = NULL, label = NULL) {
#   # convert knitted string to a list with sources separate from output
#   knitted <- knitted %>% src_to_list()
# 
#   structure(
#     knitted,
#     class = "decorated",
#     orig_code_text = code,
#     chunk_name = label
#   )
# }
# # library(flair)
# 
# split_sandwiches <- function(.string, start_rx, end_rx = NULL) {
# 
# 
#   top_buns <- str_extract_all(.string, start_rx) %>% unlist()
# 
#   if (is.null(end_rx)) {
# 
#     meat <- .string %>%
#       str_split(start_rx) %>% unlist()
# 
#     bottom_buns = NULL
# 
#   } else {
# 
#     meat <- .string %>%
#       str_split(start_rx) %>% unlist() %>%
#       str_split(end_rx) %>% unlist()
# 
#     bottom_buns <- str_extract_all(.string, end_rx) %>% unlist()
# 
#   }
# 
# 
#   # Check that buns are matched, or if no buns just return the string
#   if (!is.null(end_rx) && length(top_buns) != length(bottom_buns)) {
# 
#     stop("Error: Each top bread must have a matching bottom bread.")
# 
#   } else if (length(top_buns) == 0) {
# 
#     return(.string)
# 
#   }
# 
#   sammie <- make_sandwiches(meat, top_buns, bottom_buns)
#   sammie <- sammie[str_length(sammie) != 0]
# 
#   return(sammie)
# 
#   }
# 
# 
# make_sandwiches <- function(meat, top_buns, bottom_buns = NULL) {
# 
#     n_buns <- length(top_buns) + length(bottom_buns)
#     n_meat <- length(meat)
# 
#     if (n_meat != n_buns + 1) {
# 
#       stop("Error: Something weird happened...")
# 
#     }
# 
#     if (is.null(bottom_buns)) {
# 
#       bread <- top_buns
# 
#     } else {
# 
#       bread <- rep("", n_buns)
#       bread[((1:(n_buns/2))*2 - 1)] = top_buns
#       bread[(1:(n_buns/2))*2] = bottom_buns
# 
#     }
# 
#     sammie <- rep("", n_meat + n_buns)
#     where_meat <- (1:n_meat)*2 - 1
#     sammie[where_meat] = meat
#     sammie[-where_meat] = bread
# 
#     return(sammie)
# 
# }
# 
# as_decorated_source <- function(x, ...) {
#   UseMethod("as_decorated_source", x)
# }
# 
# #' @export
# as_decorated_source.default <- function(x, ...) {
#   stop("`as_decorated_source()` requires a character vector")
# }
# 
# #' @export
# as_decorated_source.character <- function(x, ...) {
#   structure(x, class = "decorated_source")
# }
# 
# is_decorated_source <- function(x) {
#   if (length(x) > 1) {
#     return(map_lgl(as.list(x), is_decorated_source))
#   }
#   inherits(x, "decorated_source")
# }
# 
# modify_sources <- function(x, .f, ...) {
#   # `.f()` is the function we'll apply to any `decorated_source` items in `x`.
#   # `as_mapper()` lets us use the same syntax as `purrr::map()`, i.e. function
#   # names, anonymous functions, `~ .x` style, etc. The `purrr::partial()` call
#   # fills in the arguments of the function `.f` with any arguments in the `...`
#   # -- it's like we've updated the default values of the arguments of `.f`.
#   .f <- purrr::partial(purrr::as_mapper(.f), ...)
# 
#   # Ensure the `decorated_source` items retain their class after we apply `.f()`
#   .f_decorated <- function(item) {
#     modified <- .f(item)
#     as_decorated_source(modified)
#   }
# 
#   purrr::modify_if(x, is_decorated_source, .f_decorated)
# }
````

``` r
library(tidyverse)
return_chunk_code(chunk_name = "sample_chunk")
```

    #> [1] "library(tidyverse)"              "iris %>%"                       
    #> [3] "  group_by(Species) %>%"         "  summarize(mean(Sepal.Length))"
    #> attr(,"chunk_opts")
    #> attr(,"chunk_opts")$label
    #> [1] "sample_chunk"

``` r
# code_from_editor(chunk_name = "sample_chunk")
```
