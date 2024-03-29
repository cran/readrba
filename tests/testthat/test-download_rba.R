library(readxl)

test_that("download_rba() downloads file(s)", {
  skip_if_offline()
  skip_on_cran()

  # Single file

  urls <- get_rba_urls("d1")
  filename1 <- download_rba(urls)

  expect_is(read_excel_noguess(filename1), "tbl_df")

  # Multiple files

  filenames <- download_rba(get_rba_urls(c("d1", "d2")))

  files <- purrr::map(filenames, read_excel_noguess)

  purrr::map(files, ~ expect_is(.x, "tbl_df"))
})
