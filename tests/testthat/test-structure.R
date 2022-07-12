test_that("check API response is appropriate and the parsing works", {
  rsp <- get_structAPI_response()
  expect_true(is.character(rsp))
  expect_gte(nchar(rsp), 2000000)
  out <- parse_structAPI_response(rsp)
  expect_true(is.environment(out))
  out <- parse_structAPI_response(rsp, output = "lol")
  expect_true(is.list(out))
  out <- parse_structAPI_response(rsp, output = "df")
  expect_true(is.data.frame(out))
  out <- parse_structAPI_response(rsp, output = "foobar")
  expect_true(is.na(out))
})


