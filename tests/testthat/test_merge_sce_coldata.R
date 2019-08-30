context("merge_sce_coldata")
library(seurathelpeR)

pbmc_small_sce <- Seurat::as.SingleCellExperiment(pbmc_small)
pbmc_small_sce2 <- pbmc_small_sce

# remove some cells from pbmc_small_sce
orig_sce_rm_cells <- c(78:80)
pbmc_small_sce <- pbmc_small_sce[, -orig_sce_rm_cells]

# remove different cells from pbmc_small_sce2
cmp_sce_rm_cells <- c(7, 77)
pbmc_small_sce2 <- pbmc_small_sce2[, -cmp_sce_rm_cells]

# add a fake ColData column to pbmc_small_sce2
pbmc_small_sce2$fake_col <- sample(x=7, size=ncol(pbmc_small_sce2), replace=TRUE)

test_merge_sce <- merge_sce_coldata(pbmc_small_sce, pbmc_small_sce2)

test_that("merge_sce_coldata is NA for cmp_SCE columns for cells missing in cmp_SCE", {
  # check that the cells missing from the cmp_SCE has NA for colData
  expect_true(
    all(is.na(SummarizedExperiment::colData(test_merge_sce)[cmp_sce_rm_cells, c(-1:-ncol(SummarizedExperiment::colData(pbmc_small_sce)))]))
  )
})

test_that("merge_sce_coldata correctly matches cells from the two SCE objects", {
  # get cell names from the merged SCE object
  cell_names <- colnames(test_merge_sce)

  # check the cmp_SCE data
  expect_true(
    all((lapply(cell_names, function(x) {
      all(unlist(SummarizedExperiment::colData(pbmc_small_sce2)[x, ]) ==  unlist(SummarizedExperiment::colData(test_merge_sce)[x, c(-1:-ncol(SummarizedExperiment::colData(pbmc_small_sce)))]))
    }) %>% unlist())[-cmp_sce_rm_cells])
  )

  # check the orig_SCE data
  expect_true(
    all(lapply(cell_names, function(x) {
      all(unlist(SummarizedExperiment::colData(pbmc_small_sce)[x, ]) ==  unlist(SummarizedExperiment::colData(test_merge_sce)[x, c(1:ncol(SummarizedExperiment::colData(pbmc_small_sce)))]))
    }) %>% unlist())
  )

})
