#' A server module that contains the server logic to the readQFeatures box module
#'
#' @param id module id
#' @param input_table a reactiveVal containing the input table
#' @param sample_table a reactiveVal containing the sample table
#'
#' @return A QFeatures object
#' @rdname INTERNAL_box_readqfeatures_server
#' @keywords internal
#'
#' @importFrom data.table fread
#' @importFrom shiny is.reactive reactive moduleServer observe eventReactive updateSelectInput removeModal downloadHandler
#' @importFrom DT renderDataTable datatable
#' @importFrom QFeatures readQFeatures
#' @importFrom QFeatures zeroIsNA
#' @importFrom QFeatures nNA

#' @importFrom methods as
#' @import SingleCellExperiment
#' @import SummarizedExperiment
#' @import MultiAssayExperiment
#'
box_readqfeatures_server <- function(id, input_table, sample_table) {
    stopifnot(is.reactive(input_table))
    stopifnot(is.reactive(sample_table))

    moduleServer(id, function(input, output, session) {
        observeEvent(input$convert, {
            loading(paste("Be aware that this operation",
                "can be quite time consuming for large data sets",
                sep = " "
            ))
            run_col <- input$run_col
            coldata <- sample_table()
            if (run_col == "(none)") run_col <- NULL
            if (!is.data.frame(coldata)) coldata <- NULL
            global_rv$qfeatures <- error_handler(
                readQFeatures,
                component_name = "QFeatures converting (readQFeatures)",
                assayData = input_table(),
                colData = coldata,
                quantCols = input$quant_cols,
                runCol = run_col,
                removeEmptyCols = input$removeEmptyCols,
                verbose = FALSE
            )
            if (input$zero_as_NA && length(global_rv$qfeatures) > 0) {
                global_rv$qfeatures <- error_handler(
                    QFeatures::zeroIsNA,
                    component_name = "QFeatures converting (zero_as_NA)",
                    object = global_rv$qfeatures,
                    i = seq_along(global_rv$qfeatures)
                )
            }
            if (input$singlecell) {
                el <- ExperimentList(lapply(
                    experiments(global_rv$qfeatures),
                    as, "SingleCellExperiment"
                ))
                experiments(global_rv$qfeatures) <- el
            }
            # The following code is a workaround
            # to fix keep track of the steps in the QFeatures object
            # The idea is that each page will be assigned a number,
            # and will use the assays that have the number - 1 in the name.
            # And then add assays with the number of the page in the QFeatures
            for (i in seq_along(global_rv$qfeatures)) {
                names(global_rv$qfeatures)[i] <- paste0(
                    names(global_rv$qfeatures)[i],
                    "_(QFeaturesGUI#0)_initial_import"
                )
            }
            removeModal()
        })

        observe({
            input$reload_button
            updateSelectInput(session,
                "run_col",
                choices = c("(none)", colnames(input_table()))
            )
            updateSelectInput(session,
                "quant_cols",
                choices = colnames(input_table())
            )
        })

        qfeatures_df <- reactive({
            error_handler(
                qfeatures_to_df,
                component_name = "qfeatures_to_df",
                page_assays_subset(global_rv$qfeatures, 0)
            )
        })

        output$qfeatures_dt <- DT::renderDataTable({
            DT::datatable(qfeatures_df(),
                extensions = "FixedColumns",
                selection = "single",
                options = list(
                    searching = FALSE,
                    scrollX = TRUE,
                    fixedColumns = TRUE,
                    pageLength = 5,
                    lengthMenu = c(5, 10, 15)
                )
            )
        })

        output$assay_table <- DT::renderDataTable({
            if (!is.null(input$qfeatures_dt_rows_selected)) {
                row <- input$qfeatures_dt_rows_selected
                DT::datatable(
                    data.frame(assay(global_rv$qfeatures[[row]])),
                    extensions = "FixedColumns",
                    options = list(
                        searching = FALSE,
                        scrollX = TRUE,
                        fixedColumns = TRUE,
                        pageLength = 5,
                        lengthMenu = c(5, 10, 15, 20)
                    )
                )
            }
        })
    })
}
