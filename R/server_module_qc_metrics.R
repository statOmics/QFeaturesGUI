#' Server module for qc metrics box
#'
#' @param id module id
#' @param assays_to_process a reactiveVal that contains the different assays that will be used in the module
#' @return The different assays that will be processed in the page
#' @rdname INTERNAL_server_module_qc_metrics
#' @keywords internal
#'
#' @importFrom shiny moduleServer updateSelectInput observeEvent eventReactive is.reactive
#' @importFrom MultiAssayExperiment getWithColData
#'
server_module_qc_metrics <- function(id, assays_to_process#, type
                                     ) {
    stopifnot(is.reactive(assays_to_process))
    moduleServer(id, function(input, output, session) {
        assays_choices_vector <- reactive({
            original_names <- names(assays_to_process())
            modified_names <- remove_QFeaturesGUI(original_names)
            choices_vector <- setNames(original_names, modified_names)
            return(choices_vector)
        })
        observe({
            req(assays_choices_vector)
            updateSelectInput(session,
                "selected_assay",
                choices = names(assays_choices_vector())
            )
        })
        single_assay <- reactive({
            req(input$selected_assay)
            req(assays_to_process())
            # Warning appears here
            # Warning message: 'experiments' dropped; see 'drops()'
            # see with Chris
            getWithColData(
                assays_to_process(),
                assays_choices_vector()[input$selected_assay]
            )
        })

        server_module_pca_box(
            id = "features",
            single_assay = single_assay,
            method = input$selected_method,
            transpose = FALSE
        )
        server_module_pca_box(
            id = "samples",
            single_assay = single_assay,
            method = input$selected_method,
            transpose = FALSE
        )

        server_module_viz_box("viz_box", assays_to_process)
    })
}

#' PCA Box server module
#'
#' @param id module id
#' @param single_assay a reactiveVal that contains the selected assay
#' @param method a character string specifying the PCA method to use
#' valid values are c("nipals")
#' @param transpose a boolean that specifies if the PCA should be transposed
#' TRUE for samples and FALSE for features
#' @return A shiny module server function that contains the PCA logic
#' @rdname INTERNAL_server_module_pca_box
#' @keywords internal
#'
#' @importFrom shiny moduleServer observe req reactive updateSelectInput
#' @importFrom plotly plot_ly renderPlotly layout %>%
#' @importFrom SummarizedExperiment colData rowData
#' @importFrom pcaMethods pca scores
#' @importFrom methods is
#'
server_module_pca_box <- function(id, single_assay, method, transpose) {
   moduleServer(id, function(input, output, session) {
       annotation_names <- reactive({
           req(single_assay())
           if (id == "features") {
               colnames(rowData(single_assay()))
           } else {
               colnames(colData(single_assay()))
           }
       })

        observe({
            req(single_assay())
            req(annotation_names())
            stopifnot(is(single_assay(), "SummarizedExperiment"))
            updateSelectInput(session,
                "pca_color",
                choices = c("(none)", annotation_names())
            )
        })

        color_data <- reactive({
            req(single_assay())
            req(input$pca_color)
            if (id == "features") {
                df <- rowData(single_assay())
            } else {
                df <- colData(single_assay())
            }
            if (input$pca_color == "(none)")
                return(data.frame(row.names = rownames(df)))
            df <- df[, input$pca_color, drop = FALSE]
            if (is.character(df[, 1])) {
                df[, 1] <- ifelse(nchar(df[, 1]) > input$color_width,
                    paste0(substr(df[, 1], 1, input$color_width), "..."), df[, 1]
                )
            }
            if (all(is.na(df))) {
                df[, 1] <- "NA"
            }
            colnames(df) <- input$pca_color

            return(df)
        })


        pca_result <- reactive({
            req(single_assay())
            pcaMethods_wrapper(
                single_assay(),
                method = method,
                transpose = transpose,
                scale = input$scale,
                center = input$center
            )
        })

        dataframe <- reactive({
            req(pca_result())
            req(color_data())
            if (id == "features") {
                eigenvectors <- pcaMethods::scores
            } else {
                eigenvectors <- pcaMethods::loadings
            }
            as.data.frame(merge(
                data.frame(eigenvectors(pca_result())),
                color_data(),
                by = "row.names"
            ))
        })

        output$pca <- renderPlotly({
            req(dataframe())
            req(pca_result())
            # TODO: Add a table with the selected points.
            error_handler(
                pca_plotly,
                component_name = "PCA quality control plot",
                df = dataframe(),
                pca_result = pca_result(),
                color_name = input$pca_color,
                show_legend = input$show_legend
            )
        })
    })
}
