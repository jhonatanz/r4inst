test_that("cable initialization works", {
  # Test that initialization with a valid tag works
  cable_obj <- CB$new("cable-001")
  expect_equal(cable_obj$tag, "cable-001")

  # Test that initialization with an empty tag throws an error
  expect_error(CB$new(""), "Tag for cable should have at least one character")
})

test_that("generate_structure works", {
  cable_obj <- CB$new("cable-001")

  # Test a cable with pairs, individual shields, and overall shield
  cable_obj$gen_struct("pair", 3, TRUE, TRUE)
  expect_equal(cable_obj$group, "pair")
  expect_equal(cable_obj$groups_qty, 3)
  expect_true(cable_obj$ind_shields)
  expect_true(cable_obj$ovr_shield)
  expect_equal(length(cable_obj$struct), 4) # 3 pairs + 1 overall shield
  expect_equal(names(cable_obj$struct), c("PR1", "PR2", "PR3", "OV_SHD"))

  # Test a cable with conductors, no individual shields, and no overall shield
  cable_obj$gen_struct("cond", 5, FALSE, FALSE)
  expect_equal(cable_obj$group, "cond")
  expect_equal(cable_obj$groups_qty, 5)
  expect_false(cable_obj$ind_shields)
  expect_false(cable_obj$ovr_shield)
  expect_equal(length(cable_obj$struct), 5)
  expect_equal(names(cable_obj$struct), c("CO1", "CO2", "CO3", "CO4", "CO5"))

})

test_that("df_structure works", {
  cable_obj <- CB$new("cable-001")
  cable_obj$gen_struct("pair", 3, TRUE, TRUE)
  df <- cable_obj$df_struct()

  # Test the structure of the data frame
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 10)  # 3 pairs * (2 conductors + 1 ind shield) + 1 overall shield
  expect_equal(ncol(df), 4) # tag, group, cond, and color
  expect_equal(names(df), c("tag", "group", "cond", "color"))

})

test_that("update_attr works", {
  cable_obj <- CB$new("cable-001")
  attrib <- list(
    cable_spec = "cab-spec-001",
    ovr_diam = 0.58,
    ovr_diam_UOM = "in"
  )
  cable_obj$update_attr(attrib)

  # Test that attributes are updated correctly
  expect_equal(cable_obj$cable_spec, "cab-spec-001")
  expect_equal(cable_obj$ovr_diam, 0.58)
  expect_equal(cable_obj$ovr_diam_UOM, "in")
})

test_that("print method doesn't throw errors", {
  cable_obj <- CB$new("cable-001")
  expect_error(print(cable_obj), NA) # Expect no error
})
