#' Server logic for the 'viz_box' module
#' @param id The module id
#' @param assays_to_process The assays to process (reactive)
#' @return The server logic for the 'viz_box' module
#'
#' @rdname INTERNAL_server_module_viz_box
#' @keywords internal
#'
#' @importFrom shiny moduleServer observe req reactive updateSelectizeInput updateSelectInput
#' @importFrom SummarizedExperiment assay colData
#' @importFrom plotly plot_ly renderPlotly add_trace layout
#'

server_module_viz_box <- function(id, assays_to_process) {
    moduleServer(id, function(input, output, session) {
        unique_features <- reactive({
            req(assays_to_process())
            req(input$feature_type_column)
            if (!length(assays_to_process())) return(NULL)
            features_list <- lapply(seq_along(assays_to_process()), function(i) {
                rowData(assays_to_process()[[i]])[, input$feature_type_column]
            })
            features_vector <- unlist(features_list)
            unique(features_vector)
        })

        observe({
            req(unique_features())
            updateSelectizeInput(session,
                "feature",
                choices = unique_features(),
                server = TRUE
            )
        })

        observe({
            req(assays_to_process())
            updateSelectInput(session,
                "sample_type_column",
                choices = annotation_cols(assays_to_process(), "colData")
            )
        })

        observe({
            req(assays_to_process())
            updateSelectInput(session,
                "feature_type_column",
                choices = annotation_cols(assays_to_process(), "rowData")
            )
        })

        feature_summary_df <- reactive({
            req(assays_to_process())
            req(input$feature)
            req(input$sample_type_column)
            req(input$feature_type_column)

            summarize_assays_to_df(
            qfeatures = assays_to_process(),
            sample_column = input$sample_type_column,
            feature_column = input$feature_type_column
            )
        })

        output$plot <- renderPlotly({ # Maybe a problem since a lot of line are NAs
            req(feature_summary_df())
            unique_feature_boxplot(feature_summary_df(), input$feature)
        })
    })
}
