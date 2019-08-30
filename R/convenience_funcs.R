#' Count cells in Seurat object.
#'
#' \code{count_cells} returns a tibble with counts.
#'
#' @param seurat_object Seurat object.
#' @param group_by_var A character value for grouping the cells. Must correspond
#'   to a column in Seurat@@meta.data.
#' @param subgroup_var Optional parameter. A character value for sub-groups
#'   within each group. Must correspond to a column in Seurat@@meta.data.
#' @return Tibble with cell counts and percentages. If subgroup_var is provided,
#'   percentages are calculated from the sub-groups making up groups; otherwise,
#'   percentages are calculated from the total cell count across the groups.
#'
#' @examples
#' # load example dataset from Seurat
#' data("pbmc_small", package="Seurat")
#'
#' # Count group-sub-group combinations and calculate % of sub-groups making up groups.
#' count_cells(pbmc_small, "groups", "RNA_snn_res.0.8")
#'
#' # Count groups and calculate % of the total that each group consists of.
#' count_cells(pbmc_small, "groups")
#'
#' \dontrun{
#' count_cells(pbmc_small)
#' }
#' @export
count_cells <- function(seurat_object, group_by_var, subgroup_var){
  seurat_object_df_perc <- 0 # placeholder

  if (missing(subgroup_var)) {
    seurat_object_df <- seurat_object@meta.data[, c(group_by_var), drop = FALSE]

    seurat_object_df_perc <- seurat_object_df %>%
      dplyr::group_by(!!as.symbol(group_by_var)) %>%
      dplyr::count() %>%
      dplyr::ungroup() %>%
      dplyr::mutate(perc = (.data$n / sum(.data$n) ) * 100)
  } else {
    seurat_object_df <- seurat_object@meta.data[, c(group_by_var, subgroup_var)]

    seurat_object_df_perc <- seurat_object_df %>%
      dplyr::group_by(!!as.symbol(group_by_var), !!as.symbol(subgroup_var)) %>%
      dplyr::count() %>%
      dplyr::group_by(!!as.symbol(group_by_var)) %>%
      dplyr::mutate(perc = (.data$n / sum(.data$n) ) * 100) %>%
      dplyr::ungroup()
  }

  return(seurat_object_df_perc)
}

#' Add a title to a ggplot object using cowplot.
#'
#' \code{add_title_ggplot} adds a title to a ggplot object using cowplot.
#'
#' This is useful when the ggplot object aleady has a title (e.g. gene ID) or if
#' it is a faceted plot (e.g. UMAP split by 'orig.ident').
#'
#' @param ggplot_obj A ggplot object.
#' @param plot_title A character value for the plot title.
#'
#' @examples
#' # load example dataset from Seurat
#' data("pbmc_small", package="Seurat")
#'
#' add_title_ggplot(Seurat::DimPlot(pbmc_small, group.by = "RNA_snn_res.0.8", split.by = "groups"),
#' paste0("n=",(length(Seurat::Idents(pbmc_small)))))
#' @export
add_title_ggplot <- function(ggplot_obj, plot_title){
  featplot_title <- cowplot::ggdraw() +
    cowplot::draw_label(plot_title, fontface = "bold", size = 20)
  out_plot <- cowplot::plot_grid(featplot_title, ggplot_obj, ncol = 1,
                                 rel_heights = c(0.1, 1))

  return(out_plot)
}

#' Function for making summary tables based on cellular data fetched using
#' Seurat::FetchData.
#'
#' \code{table_summary_seurat} returns a list of tibbles with summary metrics of
#' the metrics fetched from a Seurat object using Seurat::FetchData.
#'
#' @param fetch_dat_out A data frame with cells as rows and cellular data as
#'   columns (e.g. output from Seurat::FetchData).
#' @param re_group A character value for the regex to group by.
#'
#' @return A list of named tibbles, one for each metric.
#'
#' @examples
#' # load example dataset from Seurat
#' data("pbmc_small", package="Seurat")
#'
#' table_summary_seurat(Seurat::FetchData(pbmc_small, vars = c("nFeature_RNA", "nCount_RNA")),
#' "^\\S+(?=_[ACGT]+$)")
#' @export
table_summary_seurat <- function(fetch_dat_out, re_group){
  seurat_metrics <- colnames(fetch_dat_out)
  out_dfs <- list()

  for (seurat_met in seurat_metrics) {
    out_df <- fetch_dat_out %>%
      tibble::rownames_to_column(var = "cell") %>%
      dplyr::mutate(lib = stringr::str_extract(.data$cell, re_group)) %>%
      dplyr::group_by(.data$lib) %>%
      dplyr::summarise(perc25 = stats::quantile(!!as.name(seurat_met), 0.25),
                       median = stats::median(!!as.name(seurat_met)),
                       perc75 = stats::quantile(!!as.name(seurat_met), 0.75),
                       perc90 = stats::quantile(!!as.name(seurat_met), 0.90),
                       perc95 = stats::quantile(!!as.name(seurat_met), 0.95))
    out_dfs[[seurat_met]] <- out_df
  }

  return(out_dfs)
}

#' Function for merging the ColData of two SCE objects, matching by cell names.
#'
#' \code{merge_sce_coldata} returns the original SCE object with additional
#' ColData merged from another SCE object with overlapping cells. If there are
#' columns with the same name, the columns from the cmp_SCE will be suffixed
#' with "_cmp".
#'
#' This function is useful for example when analyzing the same dataset with
#' Seurat and the DropletUtils/scater/scran pipeline and then needing to
#' visualize column data from the two analyses on the same embedding. It seems
#' easier to convert Seurat to SCE than from SCE to Seurat.
#'
#' @param orig_sce SCE object that is to be retained.
#' @param cmp_sce SCE object with overlapping cells (cells must have the same
#'   names) and additional ColData to incorporate.
#'
#' @return The original SCE object with additional ColData from the second SCE
#'   object.
#'
#' @examples
#' # load example dataset from Seurat
#' data("pbmc_small", package="Seurat")
#'
#' pbmc_small_sce <- Seurat::as.SingleCellExperiment(pbmc_small)
#' pbmc_small_sce2 <- pbmc_small_sce
#'
#' # remove some cells from pbmc_small_sce
#' pbmc_small_sce <- pbmc_small_sce[, c(-78:-80)]
#'
#' # remove different cells from pbmc_small_sce2
#' pbmc_small_sce2 <- pbmc_small_sce2[, c(-7,-77)]
#'
#' # add a fake ColData column to pbmc_small_sce2
#' pbmc_small_sce2$fake_col <- sample(x=7, size=ncol(pbmc_small_sce2), replace=TRUE)
#'
#' merge_sce_coldata(pbmc_small_sce, pbmc_small_sce2)
#'
#' @export
merge_sce_coldata <- function(orig_sce, cmp_sce){
  colnames(SummarizedExperiment::colData(cmp_sce)) <- ifelse(colnames(SummarizedExperiment::colData(cmp_sce)) %in% colnames(SummarizedExperiment::colData(orig_sce)),
                                       paste0(colnames(SummarizedExperiment::colData(cmp_sce)), "_cmp"), colnames(SummarizedExperiment::colData(cmp_sce)))
  SummarizedExperiment::colData(orig_sce) <- cbind(SummarizedExperiment::colData(orig_sce),
                             SummarizedExperiment::colData(cmp_sce)[match(rownames(SummarizedExperiment::colData(orig_sce)),
                                                    rownames(SummarizedExperiment::colData(cmp_sce))), ])

  return(orig_sce)
}


