purrr::walk(list.files("../../R", full.names = TRUE), source)

read_output <- \(name) readr::read_csv(file.path("../../tabs", name), show_col_types = FALSE)
