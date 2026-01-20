# Package internal functions

# Internal data are created in data-raw/internal.R


# TODO: Change to driven from database. AMLR study beach start dates, etc.
.amlr.beaches <- c(
  "Chungungo", "Cachorros", "Maderas", "Copi", "Hue",
  "Copihue", "Modulo",  "Daniel", "Marko",
  "Modulo North", "Modulo South"
)


#-------------------------------------------------------------------------------
#
.mod_check <- function(...) {
  for (i in list(...)) {
    stopifnot(is.reactive(i))
  }
}


#-------------------------------------------------------------------------------
### Summary function used by census tabs
# TODO: merge with vcs_summ_func in phocid census tab?
.vcs_summ_func <- function(y, ..., season.df, beach.chr = FALSE) {
  df.out <-  y %>%
    group_by(...) %>%
    summarise(across(where(is.numeric), ~if_else(all(is.na(.x)), NA_integer_,
                                                 sum(.x, na.rm = TRUE))),
              Beaches = paste(sort(unique(Beach)), collapse = ", "),
              .groups = "drop") %>%
    arrange_season(season.df, species)

  if (!beach.chr) select(df.out, -Beaches) else df.out
}


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
### Returns the radioButton widget for selecting the timing summary level
### choices argument is the choices that should be included
.summaryTimingUI <- function(
    ns,
    choices = .summary.timing.choices,
    selected = "fs_single"
) {
  choices.args <- match.arg(choices, several.ok = TRUE)

  if (!all(choices.args %in% .summary.timing.choices.list))
    stop("Need to update internal function - please contact the database manager")

  # Need to do the in to keep as named list
  choices.list <- .summary.timing.choices.list[
    .summary.timing.choices.list %in% choices.args
  ]
  # choices.list <- intersect(.summary.timing.choices.list, choices.args)

  if (!(selected %in% choices.list))
    stop("selected must be one of the choices")

  radioButtons(ns("summary_timing"), label = tags$h5("Summary timing"),
               choices = choices.list, selected = selected)
}


#-------------------------------------------------------------------------------
### Returns the radioButton widget for selecting the location summary level
.summaryLocationUI <- function(
    ns,
    choices = c("by_capewide", "by_amlr", "by_beach"),
    choices.selected = "by_capewide",
    group.option = TRUE)
{
  choices.args <- match.arg(choices, several.ok = TRUE)

  choices.list.all <- list(
    "Cape-wide" = "by_capewide",
    "AMLR study beaches" = "by_amlr",
    "By beach" = "by_beach"
  )

  if (!all(choices.args %in% choices.list.all))
    stop("Need to update internal function - please contact the database manager")

  choices.list <- choices.list.all[choices.list.all %in% choices.args]

  if (!(choices.selected %in% choices.list))
    stop("choices.selected must be one of the choices")

  rb.out <- radioButtons(ns("summary_location"), label = tags$h5("Location"),
                         choices = choices.list, selected = choices.selected)

  if (group.option) {
    list(
      rb.out,
      conditionalPanel(
        condition = "input.summary_location == 'by_beach'", ns = ns,
        checkboxInput(ns("location_aggregate"), "Group beaches", value = TRUE)
      )
    )
  } else {
    rb.out
  }
}


#-------------------------------------------------------------------------------
### Returns the radioButton widget for selecting the sp/age/sex summary level
.summarySpAgeSexUI <- function(
    ns,
    choices = c("by_sp", "by_sp_age_sex"),
    choices.selected = "by_sp")
{
  choices.args <- match.arg(choices, several.ok = TRUE)

  choices.list.all <- list(
    "By species" = "by_sp",
    "By species, and sex + age class" = "by_sp_age_sex"
  )

  if (!all(choices.args %in% choices.list.all))
    stop("Need to update internal function - please contact the database manager")

  choices.list <- choices.list.all[choices.list.all %in% choices.args]

  if (!(choices.selected %in% choices.list))
    stop("choices.selected must be one of the choices")

  radioButtons(ns("summary_sas"), label = tags$h5("Species/age class/sex"),
               choices = choices.list, selected = choices.selected)
}

#-------------------------------------------------------------------------------
### Returns a list of all the species/color pairs present in a table
.colorsPresent <- function(table) {
  colors.all <- tamatoamlr::pinniped.sp.colors
  color.values <- colors.all[names(colors.all) %in% table$species]
  return(color.values)
}


#-------------------------------------------------------------------------------
### Generic validate message used in else blocks in Tamatoa
.validate_else <- function(widget.name) {
  validate(
    glue("Invalid input${widget.name} value - ",
         "please contact the database manager")
  )
}
