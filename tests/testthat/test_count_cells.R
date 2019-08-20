context("count_cells")
library(seurathelpeR)

test_that("count_cells is tibble", {
  count_cells_1grp_out <- count_cells(pbmc_small, "RNA_snn_res.0.8")
  expect_is(count_cells_1grp_out, c("tbl", "tbl_df", "data.frame"))

  count_cells_2grp_out <- count_cells(pbmc_small, "groups", "RNA_snn_res.0.8")
  expect_is(count_cells_2grp_out, c("tbl", "tbl_df", "data.frame"))
})

test_that("count_cells perc column add up to 100 for each group", {
  expect_equal(sum(count_cells(pbmc_small, "RNA_snn_res.0.8")$perc), 100)

  # with sub-groups
  count_cells_w_subgrps <- count_cells(pbmc_small, "groups", "RNA_snn_res.0.8")
  expect_equal(do.call(sum, lapply(
    split(count_cells_w_subgrps, count_cells_w_subgrps[, 1]),
    function(x) {
      sum(x$perc)
    }
  )),
  length(unique(count_cells_w_subgrps[[1]])) * 100)
})
