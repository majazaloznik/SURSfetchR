rsp <- get_API_response()
out <- parse_structAPI_response(rsp)
test_that("check API response is appropriate and the parsing works", {
  expect_true(is.character(rsp))
  expect_true(is.environment(out))
  expect_gte(nchar(rsp), 2000000)
  out <- parse_structAPI_response(rsp, output = "lol")
  expect_true(is.list(out))
  out <- parse_structAPI_response(rsp, output = "df")
  expect_true(is.data.frame(out))
  out <- parse_structAPI_response(rsp, output = "foobar")
  expect_true(is.na(out))
})

test_that("node helper functions", {
  expect_equal(node_name(out$`1`), "Socialna zaščita")
  expect_equal(node_id(out$`1`), 45)
  expect_true(node_file_id(out$`1`$childPodrocja$`1`$matrixList$`1`) >= 753518)
  expect_true(grepl("letno",node_matrix_name(out$`1`$childPodrocja$`1`$matrixList$`1`)))
  expect_true(grepl("^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}$",
                    node_updated(out$`1`$childPodrocja$`1`$matrixList$`1`)))
  expect_equal(node_parent(out$`1`$childPodrocja), 45)
})


test_that("Structure is extracted properly", {
  full <- get_full_structure(out)
  expect_equal(ncol(full), 9)
  expect_gt(nrow(full), 6000)
  expect_true(all(sapply(full, \(x) !all(is.na(x)))))
  expect_true(inherits(full$published, "POSIXct"))
  mat_h <- get_matrix_hierarchy(full)
  expect_true(nrow(mat_h) < nrow(full))
  field_h <- get_field_hierarchy(full)
  expect_true(nrow(field_h) < nrow(full))
  expect_true(nrow(field_h) + nrow(mat_h) == nrow(full))
  tree <- get_tree(mat_h)
  expect_true(inherits(tree, "Node"))
})

