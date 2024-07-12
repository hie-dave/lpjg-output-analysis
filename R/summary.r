
make_summary_line <- function(benchmark, col_names) {
    table_lines <- as.list(rep("-", length(col_names)))
    names(table_lines) <- col_names

    for (dataset in benchmark@datasets) {
        if (table_lines$Dataset == "-") {
            table_lines$Dataset <- dataset@source@name
        } else {
            name <- paste0(table_lines$Dataset, ", ", dataset@source@name)
            table_lines$Dataset <- name
        }
    }
    table_lines$Quantity <- benchmark@description
    table_lines$Unit <- benchmark@agg.unit
    return(table_lines)
}
