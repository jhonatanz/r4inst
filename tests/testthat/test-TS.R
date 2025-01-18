test_that("TS initialization works", {
  ts <- TS$new("TS-001", "JB-001", "end")
  expect_equal(ts$tag, "TS-001")
  expect_equal(ts$device, "JB-001")
  expect_equal(ts$term_type, "end")
})

test_that("TS initialization fails with invalid input", {
  expect_error(TS$new("", "JB-001", "end"))
  expect_error(TS$new("TS-001", "JB-001", "invalid"))
})

test_that("gen_struct creates the correct structure", {
  ts <- TS$new("TS-001", "JB-001", "end")
  ts$gen_struct(3)
  expect_equal(ts$term_qty, 3)
  expect_equal(length(ts$struct$nums), 3)
})

test_that("add_terminal adds terminals correctly", {
  ts <- TS$new("TS-001", "JB-001", "end")
  ts$gen_struct(3)
  ts$add_terminal(2)
  expect_equal(ts$term_qty, 5)
  expect_equal(length(ts$struct$nums), 5)
})

test_that("add_cable adds a cable correctly", {
  ts <- TS$new("TS-001", "JB-001", "pass")
  ts$gen_struct(3)
  cable <- CB$new("CB-001")
  cable$gen_struct("pair", 1, F, F)
  ts$add_cable(cable, side = "input", cable_side = "dest")
  expect_equal(ts$term_qty, 5)
  expect_equal(ts$struct$term_conn_I[4], "CB-001/PR1/C1")
  expect_equal(ts$struct$term_conn_I[5], "CB-001/PR1/C2")
  ts1 <- TS$new("TS-002", "FIT-001", "end")
  ts1$gen_struct(3)
  ts1$add_cable(cable, cable_side = "origin")
  expect_equal(ts1$term_qty, 5)
  expect_equal(ts1$struct$term_conn[4], "CB-001/PR1/C1")
  expect_equal(ts1$struct$term_conn[5], "CB-001/PR1/C2")
})

test_that("con_cable connects a cable correctly", {
  ts <- TS$new("TS-001", "FIT-001", "end")
  ts$gen_struct(3)
  cable <- CB$new("CB-001")
  cable$gen_struct("pair", 1, F, F)
  ts$con_cable(cable, init = 1, cable_side = "origin")
  expect_equal(ts$struct$term_conn[1], "CB-001/PR1/C1")
  expect_equal(ts$struct$term_conn[2], "CB-001/PR1/C2")
  ts1 <- TS$new("TS-002", "JB-001", "pass")
  ts1$gen_struct(3)
  ts1$con_cable(cable, init = 1, side = "output", cable_side = "dest")
  expect_equal(ts1$struct$term_conn_O[1], "CB-001/PR1/C1")
  expect_equal(ts1$struct$term_conn_O[2], "CB-001/PR1/C2")
})

test_that("con_cable fails with insufficient terminals", {
  ts <- TS$new("TS-001", "JB-001", "end")
  ts$gen_struct(1)
  cable <- CB$new("CB-001")
  cable$gen_struct("pair", 1, F, F)
  expect_error(ts$con_cable(cable, init = 1, cable_side = "origin"))
})

test_that("df_struct returns the correct data frame", {
  ts <- TS$new("TS-001", "JB-001", "end")
  ts$gen_struct(3)
  df <- ts$df_struct()
  expect_equal(nrow(df), 3)
  expect_equal(df$device[1], "JB-001")
})
