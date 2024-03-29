
test_that("check_if_rba_ts() returns expected output", {
  skip_on_cran()
  skip_if_offline()

  temp_dir <- tempdir()
  on.exit(unlink(temp_dir))

  urls <- get_rba_urls(c("a1", "a5"))
  files <- download_rba(urls, path = temp_dir)
  dfs <- purrr::map(files, read_excel_noguess)

  expect_true(check_if_rba_ts(dfs[[1]]))
  expect_false(check_if_rba_ts(dfs[[2]]))
})
