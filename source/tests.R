library(testthat)

Rscript <- "HW07_TidyPart03.R"
answers <- readRDS("answers.RDS")


# robust label normalization: make dashes uniform, collapse spaces
`%||%` <- function(a, b) if (!is.null(a)) a else b

normalize_label <- function(x) {
  x <- enc2utf8(x %||% "")
  # unify various dashes/hyphens to ASCII '-'
  x <- gsub("[\u2010\u2011\u2012\u2013\u2014\u2015\u2212]", "-", x, perl = TRUE)
  # collapse multiple spaces and NBSP to single space
  x <- gsub("[\u00A0\\s]+", " ", x, perl = TRUE)
  trimws(x)
}


expect_string_is_in_Rscript <- function(string) {
  test_that(paste(string, "is used (visible)"), {
    # Read the entire file into one string
    file_content <- Rscript |>
      readLines(warn = FALSE) |>
      paste(collapse = "\n")
    
    # Expect the string to be present
    expect_true(grepl(string, file_content, fixed=TRUE))
  }) 
}

# Collect all variables used by each aesthetic across the whole plot
# (global mapping + every layer), without overwriting earlier layers.
# Returns a named list: aes_name -> unique character vector of variable names.
.all_aes_vars <- function(p) {
  amap <- list()
  
  # Initialize with global mappings (store each mapping as a list element)
  if (!is.null(p$mapping) && length(p$mapping)) {
    for (nm in names(p$mapping)) {
      # store the unevaluated expression object
      amap[[nm]] <- list(p$mapping[[nm]])
    }
  }
  
  # Append layer-specific mappings (do NOT overwrite earlier ones)
  for (ly in p$layers) {
    if (!is.null(ly$mapping) && length(ly$mapping)) {
      for (nm in names(ly$mapping)) {
        amap[[nm]] <- c(amap[[nm]], list(ly$mapping[[nm]]))
      }
    }
  }
  
  # For each aesthetic, extract variable names from all stored expressions,
  # flatten, and de-duplicate.
  lapply(amap, function(expr_list) {
    unique(unlist(lapply(expr_list, function(e) {
      if (is.null(e)) character(0) else all.vars(rlang::get_expr(e))
    })))
  })
}


# Assert that the plot uses a given variable `var` for aesthetic `aes_name`
# in ANY mapping position (global or any layer).
# This avoids false negatives when later layers override earlier ones.
expect_has_aes_var <- function(p, aes_name, var, msg = NULL) {
  vars_used <- .all_aes_vars(p)
  used <- vars_used[[aes_name]]
  if (is.null(used)) used <- character(0)
  
  testthat::expect_true(
    var %in% used,
    msg %||% sprintf(
      "aes '%s' does not use variable '%s' (got: %s)",
      aes_name, var, if (length(used)) paste(used, collapse = ", ") else ""
    )
  )
}



# ------------------------------------------------------------------------------
# Tests
# ------------------------------------------------------------------------------



test_that("script starts with metadata (visible)", {
  first_character <- substr(readLines(Rscript, n = 1), 1, 1)
  expect_equal(first_character, '#')
})



# ------------------------------------------------------------------------------
# Shuttle Tests
# ------------------------------------------------------------------------------

test_that("shuttle has correct dimensions (visible)", {
  expect_equal(dim(shuttle), dim(answers$shuttle))
})

test_that("shuttle has correct column names (visible)", {
  expect_setequal(names(shuttle), names(answers$shuttle))
})

test_that("shuttle has TempC calculated correctly (visible)", {
  expect_equal(shuttle$TempC, 5/9 * (shuttle$Temp - 32) )
})

test_that("shuttle_plot is a scatterplot (visible)", {
  geoms <- vapply(shuttle_plot$layers, function(ly) class(ly$geom)[1], character(1))
  expect_true("GeomPoint"  %in% geoms)
})

test_that("shuttle_plot has a smoother (visible)", {
  geoms <- vapply(shuttle_plot$layers, function(ly) class(ly$geom)[1], character(1))
  expect_true("GeomSmooth" %in% geoms)
})


test_that("shuttle_plot has TempC on x-axis (visible)", {
  expect_has_aes_var(shuttle_plot, "x", "TempC")
})

test_that("shuttle_plot has Incidents on y-axis (visible)", {
  expect_has_aes_var(shuttle_plot, "y", "Incidents")
})

test_that("shuttle_plot x-axis label is 'Temperature (C)' (visible)", {
  expect_equal(shuttle_plot$labels$x, "Temperature (C)")
})

test_that("shuttle_plot y-axis label OK (visible)", {
  expect_equal(
    normalize_label(shuttle_plot$labels$y),
    "Number of O-ring Incidents"
  )
})

test_that("shuttle_plot title OK (visible)", {
  expect_equal(
    normalize_label(shuttle_plot$labels$title),
    "Shuttle O-ring Incidents by Temperature"
  )
})

test_that("shuttle_plot subtitle is 'Prior to Challenger Disaster' (visible)", {
  expect_equal(shuttle_plot$labels$subtitle, "Prior to Challenger Disaster")
})

test_that("shuttle_plot uses bw theme (visible)", {
  panel_bg <- shuttle_plot$theme$panel.background
  expect_s3_class(panel_bg, "element_rect")   # ggplot theme elements are "element_rect"
  expect_equal(panel_bg$fill, "white")
  
  
  panel_grid_major <- shuttle_plot$theme$panel.grid.major
  expect_true(is.null(panel_grid_major) || inherits(panel_grid_major, "element_blank"))
})





# ------------------------------------------------------------------------------
# T rex Tests
# ------------------------------------------------------------------------------

test_that("rex has correct dimensions (visible)", {
  expect_equal(dim(rex), dim(answers$rex))
})

test_that("rex has correct column names (visible)", {
  expect_setequal(names(rex), names(answers$rex))
})

test_that("rex$Bone is a factor (visible)", {
  expect_true(is.factor(rex$Bone))
})

test_that("rex$Bone is in the proper order (visible)", {
  expect_equivalent(levels(rex$Bone), paste0("Bone", 1:12))
})


test_that("rex_summary has correct dimensions (visible)", {
  expect_equal(dim(rex_summary), dim(answers$rex_summary))
})

test_that("rex_summary has correct column names (visible)", {
  expect_setequal(names(rex_summary), names(answers$rex_summary))
})

test_that("rex_plot is a jitterplot (visible)", {
 geoms <- vapply(rex_plot$layers, function(ly) class(ly$geom)[1], character(1))
  expect_true("GeomPoint"  %in% geoms)
})



test_that("rex_plot uses geom_pointrange (visible)", {
 geoms <- vapply(rex_plot$layers, function(ly) class(ly$geom)[1], character(1))
  expect_true("GeomPointrange"  %in% geoms)
})





test_that("rex_plot has Bone on x-axis (visible)", {
  expect_has_aes_var(rex_plot, "x", "Bone")
})

test_that("rex_plot has Oxygen on y-axis (visible)", {
  expect_has_aes_var(rex_plot, "y", "Oxygen")
})

test_that("rex_plot x-axis label is 'Bone ID' (visible)", {
  expect_equal(rex_plot$labels$x, "Bone ID")
})

test_that("rex_plot y-axis label is 'Oxygen Isotopic Composition' (visible)", {
  expect_equal(rex_plot$labels$y, "Oxygen Isotopic Composition")
})

test_that("rex_plot title is 'Oxygen Isotopic Composition of Vertebrate Bone Phosphate' (visible)", {
  expect_equal(rex_plot$labels$title, "Oxygen Isotopic Composition of Vertebrate Bone Phosphate")
})

test_that("rex_plot subtitle is 'in 12 bones of a single T. rex specimen' (visible)", {
  expect_equal(rex_plot$labels$subtitle, "in 12 bones of a single T. rex specimen")
})

test_that("rex_plot uses bw theme (visible)", {
  # Use $ instead of @, and check element classes correctly
  panel_bg <- rex_plot$theme$panel.background
  expect_s3_class(panel_bg, "element_rect")   # ggplot theme elements are "element_rect"
  expect_equal(panel_bg$fill, "white")
  
  # panel.grid.major is often element_blank(), not NULL
  panel_grid_major <- rex_plot$theme$panel.grid.major
  expect_true(is.null(panel_grid_major) || inherits(panel_grid_major, "element_blank"))
})




# ------------------------------------------------------------------------------
# grazer Tests
# ------------------------------------------------------------------------------

test_that("grazer has correct dimensions (visible)", {
  expect_equal(dim(grazer), dim(answers$grazer))
})

test_that("grazer has correct column names (visible)", {
  expect_setequal(names(grazer), names(answers$grazer))
})

test_that("grazer$Block is a character (visible)", {
  expect_true(is.character(grazer$Block))
})

test_that("grazer$Treat is a factor (visible)", {
  expect_true(is.factor(grazer$Treat))
})

test_that("grazer$Treat is in the proper order (visible)", {
  expect_equivalent(levels(grazer$Treat), c("C","f","fF","L","Lf","LfF"))
})

test_that("grazer_plot is a scatterplot (visible)", {
  expect_s3_class(grazer_plot$layers[[1]]$geom, "GeomPoint")
})

test_that("grazer_plot uses facet_wrap (visible)", {
  expect_s3_class(grazer_plot$facet, "FacetWrap")
})

test_that("grazer_plot has Block in the facet (visible)", {
  expect_equal(as.character(grazer_plot$facet$params$facets), 
               "~Block")
})


test_that("grazer_plot has Treat on x-axis (visible)", {
  expect_has_aes_var(grazer_plot, "x", "Treat")
})

test_that("grazer_plot has Cover on y-axis (visible)", {
  expect_has_aes_var(grazer_plot, "y", "Cover")
})

test_that("grazer_plot has Treat as color (visible)", {
  expect_has_aes_var(grazer_plot, "colour", "Treat")
})

test_that("grazer_plot has Treat as shape (visible)", {
  expect_has_aes_var(grazer_plot, "shape", "Treat")
})


test_that("grazer_plot uses bw theme (visible)", {
  # Use $ instead of @, and check element classes correctly
  panel_bg <- grazer_plot$theme$panel.background
  expect_s3_class(panel_bg, "element_rect")   # ggplot theme elements are "element_rect"
  expect_equal(panel_bg$fill, "white")
  
  # panel.grid.major is often element_blank(), not NULL
  panel_grid_major <- grazer_plot$theme$panel.grid.major
  expect_true(is.null(panel_grid_major) || inherits(panel_grid_major, "element_blank"))
})


test_that("grazer_summary has correct dimensions (visible)", {
  expect_equal(dim(grazer_summary), dim(answers$grazer_summary))
})

test_that("grazer_summary has correct column names (visible)", {
  expect_setequal(names(grazer_summary), names(answers$grazer_summary))
})

test_that("grazer_summary$Block is a character (visible)", {
  expect_true(is.character(grazer_summary$Block))
})

test_that("grazer_summary$Treat is a factor (visible)", {
  expect_true(is.factor(grazer_summary$Treat))
})

test_that("grazer_summary$Treat factor levels are in the proper order (visible)", {
  expect_equivalent(levels(grazer_summary$Treat), c("C","f","fF","L","Lf","LfF"))
})

test_that("grazer$mean is in the proper order (visible)", {
  expect_equivalent(grazer_summary$mean, answers$grazer_summary$mean)
})




test_that("grazer_means has correct dimensions (visible)", {
  expect_equal(dim(grazer_means), dim(answers$grazer_means))
})

test_that("grazer_means has correct column names (visible)", {
  expect_setequal(names(grazer_means), names(answers$grazer_means))
})

test_that("grazer_means$Block is a character (visible)", {
  expect_true(is.character(grazer_means$Block))
})


test_that("grazer_means columns are in the proper order (visible)", {
  expect_equivalent(colnames(        grazer_means), 
                    colnames(answers$grazer_means))
})



test_that("grazer_summary_plot has points (visible)", {
  geoms <- vapply(grazer_summary_plot$layers, function(ly) class(ly$geom)[1], character(1))
  expect_true("GeomPoint"  %in% geoms)
})

test_that("grazer_summary_plot has a line (visible)", {
  geoms <- vapply(grazer_summary_plot$layers, function(ly) class(ly$geom)[1], character(1))
  expect_true("GeomLine"  %in% geoms)
})

test_that("grazer_summary_plot has Block on x-axis (visible)", {
  expect_has_aes_var(grazer_summary_plot, "x", "Block")
})

test_that("grazer_summary_plot has mean on y-axis (visible)", {
  expect_has_aes_var(grazer_summary_plot, "y", "mean")
})

test_that("grazer_summary_plot has Treat as color (visible)", {
  expect_has_aes_var(grazer_summary_plot, "colour", "Treat")
})

test_that("grazer_summary_plot has Treat as shape (visible)", {
  expect_has_aes_var(grazer_summary_plot, "shape", "Treat")
})

test_that("grazer_summary_plot y-axis label is 'Mean Cover' (visible)", {
  expect_equal(grazer_summary_plot$labels$y, "Mean Cover")
})


test_that("grazer_summary_plot uses bw theme (visible)", {
  # Use $ instead of @, and check element classes correctly
  panel_bg <- grazer_summary_plot$theme$panel.background
  expect_s3_class(panel_bg, "element_rect")   # ggplot theme elements are "element_rect"
  expect_equal(panel_bg$fill, "white")
  
  # panel.grid.major is often element_blank(), not NULL
  panel_grid_major <- grazer_summary_plot$theme$panel.grid.major
  expect_true(is.null(panel_grid_major) || inherits(panel_grid_major, "element_blank"))
})




# ------------------------------------------------------------------------------
# WorldPhones Tests
# ------------------------------------------------------------------------------

test_that("WorldPhones_df has correct dimensions (visible)", {
  expect_equal(dim(WorldPhones_df), dim(answers$WorldPhones_df))
})

test_that("WorldPhones_df has correct column names (visible)", {
  expect_setequal(names(WorldPhones_df), names(answers$WorldPhones_df))
})

test_that("WorldPhones_df$Year is numeric (visible)", {
  expect_true(is.numeric(WorldPhones_df$Year))
})


test_that("WorldPhones_transformed has correct dimensions (visible)", {
  expect_equal(dim(WorldPhones_transformed), dim(answers$WorldPhones_transformed))
})

test_that("WorldPhones_transformed has correct column names (visible)", {
  expect_setequal(names(        WorldPhones_transformed), 
                  names(answers$WorldPhones_transformed))
})


test_that("WorldPhones_plot is a line plot (visible)", {
  geoms <- vapply(WorldPhones_plot$layers, function(ly) class(ly$geom)[1], character(1))
  expect_true("GeomLine"  %in% geoms)
 
})

test_that("WorldPhones_plot has logarithmic y-axis (visible)", {
  expect_equal(WorldPhones_plot$scales$get_scales("y")$trans$name, 
               "log-10")
})


test_that("WorldPhones_plot has Year on x-axis (visible)", {
  expect_has_aes_var(WorldPhones_plot, "x", "Year")
})

test_that("WorldPhones_plot has Count on y-axis (visible)", {
  expect_has_aes_var(WorldPhones_plot, "y", "Count")
})

test_that("WorldPhones_plot has Region as color (visible)", {
  expect_has_aes_var(WorldPhones_plot, "colour", "Region")
})

test_that("WorldPhones_plot has Region as linetype (visible)", {
  expect_has_aes_var(WorldPhones_plot, "linetype", "Region")
})


test_that("WorldPhones_plot uses bw theme (visible)", {
  # Use $ instead of @, and check element classes correctly
  panel_bg <- WorldPhones_plot$theme$panel.background
  expect_s3_class(panel_bg, "element_rect")   # ggplot theme elements are "element_rect"
  expect_equal(panel_bg$fill, "white")
  
  # panel.grid.major is often element_blank(), not NULL
  panel_grid_major <- WorldPhones_plot$theme$panel.grid.major
  expect_true(is.null(panel_grid_major) || inherits(panel_grid_major, "element_blank"))
})

