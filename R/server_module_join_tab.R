#' Server for the assay joining tab
#'
#' @param id module id
#' @return The server logic for the assay joining tab
#' @rdname INTERNAL_server_module_aggregation_tab
#' @keywords internal
#'
#' @importFrom shiny moduleServer updateSelectInput observeEvent eventReactive is.reactive
#' @importFrom MultiAssayExperiment getWithColData
#'
server_module_join_tab <- function(id, step_number) {
    moduleServer(id, function(input, output, session) {
        assays_to_process <- eventReactive(input$reload, {
            error_handler(
                page_assays_subset,
                component_name = "Page assays subset",
                qfeatures = global_rv$qfeatures,
                pattern = paste0("_(QFeaturesGUI#", step_number - 1, ")")
            )
        })

        observeEvent(assays_to_process(), {
            output$rownames <- renderUI({
                tags$div(
                    tags$h4("Assays to join"),
                    tags$ul(
                        style = "width: 100%; height: 100%; overflow: auto",
                        lapply(seq_along(assays_to_process()), function(i) {
                            tags$li(
                                style = "font-size: 15px;",
                                class = "list-element",
                                tags$span(
                                    style = "font-size: 15px",
                                    names(assays_to_process())[i]
                                ),
                                tags$br(),
                                tags$span(
                                    style = "margin-left: 20px; font-size: 13px;",
                                    paste(
                                        "Rownames: ",
                                        paste(
                                            head(rownames(assays_to_process())[[i]], 10),
                                            collapse = ", "
                                        ),
                                        "[...]"
                                    )
                                )
                            )
                        }),
                        class = "list-group"
                    )
                )
            })
        })

        processed_assays <- reactive({
            req(assays_to_process())
            error_handler(
                join_qfeatures,
                component_name = "aggregation",
                qfeatures = assays_to_process()
            )
        })

        observeEvent(input$export, {
            req(processed_assays())
            loading(paste("Be aware that this operation",
                          "can be quite time consuming for large data sets",
                          sep = " "
            ))
            error_handler(
                add_joined_assay_to_global_rv,
                component_name = "Add assays to global_rv",
                processed_qfeatures = processed_assays(),
                step_number = step_number,
                type = "join"
            )
            removeModal()
        })
    })
}
