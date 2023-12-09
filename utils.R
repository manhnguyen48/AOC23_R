library(httr2)
library(stringr)

#Function to set up folder and get input for a given day
set_up_day <- function(day) {
  #create folder
  main_dir <- getwd()
  day_folder <-
    c(paste0("day_", stringr::str_pad(day, 2, "left", "0")))
  task_folder <- c("00_input", "01_script")
  directories <-
    vapply(task_folder,
           \(x) paste0(day_folder, "/", x),
           character(1),
           USE.NAMES = FALSE)
  invisible(lapply(c(day_folder,directories), \(x) {
    if (!file.exists(x)) {
      dir.create(file.path(main_dir, x))
    }
  }))
  message("Folder structure has been created")
  # #Get the input file
  url <- paste0("https://adventofcode.com/2023/day/", day, "/input")
  session_cookie <- Sys.getenv("SESSION")
  file_content <- httr2::request(url) |>
    httr2::req_headers(Cookie = paste0("session=", session_cookie)) |>
    httr2::req_cache(path = tempdir()) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    strsplit("\\n")
  readr::write_lines(file_content[[1]],
                     file.path(main_dir,
                               paste0(day_folder, "/", "00_input/input.txt")))
}

