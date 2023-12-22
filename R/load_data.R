#' Title
#'
#' @return
#' @export
load_remote_codebooks <- function() {
  
} # End function load_remote_codebooks
################################################################################

#' Load all sheets from combined codebooks for sub-cohort
#'
#' @return A dataframe containing all the variables. A dataframe.
#' @export
load_local_codebooks <- function() {
  path_codebooks <- "docs/HELIX_codebooks_subcohort_childhood_17jul2022_PUBLIC_v2.xlsx"
  sheet_names <- c(
    "socioeconomic_social capital", "socioeconomic_social_capital",
    "tobacco",
    "clinical", "neuro", "respiratory", "spirometry",
    "OTHERS"
  )
  params <- yaml::read_yaml("docs/params.yaml")
  
  # Loop over the sheets and append
  df <- lapply(sheet_names, function(x) {
    readxl::read_xlsx(
      path = path_codebooks,
      sheet = x,
      skip = 2
    ) |>
      tibble::as_tibble() |>
      tidylog::rename(
        variable = var,
        type = typ
      ) |>
      tidylog::select(
        -dplyr::any_of(c(
          "harm_table",
          "code", "label", "units"
        ))
      ) |>
      tidylog::mutate(
        type = tolower(type),
        sheet_name = x
      )
  }) |> # End loop over sheets
    dplyr::bind_rows() |>
    tidylog::relocate(sheet_name) |>
    tidylog::distinct(variable, .keep_all = TRUE) |>
    tidylog::drop_na(variable) |>
    tidylog::mutate(
      dplyr::across(
        dplyr::everything(),
        \(x) stringr::str_trim(
          x, side = "both"
        )
      )
    ) |>
    tidylog::filter(
      variable %in% unique(
        c(params$covariates, params$clinical)
      )
    )
  
  # Write data request to file
  write.table(
    x = df,
    file = "docs/data_request.csv",
    quote = FALSE,
    sep = "|",
    row.names = FALSE
  )
  
  return(df)
} # End function load_local_codebooks
################################################################################
