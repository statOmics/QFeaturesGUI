#' @title Server logic for the samples filtering tab
#' @param id The module id
#' @param step_number The step number
#'
#' @return The processed assays
#' @rdname INTERNAL_server_module_samples_filtering_tab
#' @keywords internal
#'
#' @importFrom shiny moduleServer eventReactive observeEvent renderUI reactiveValues observe reactiveValuesToList NS reactive
#' @importFrom QFeatures filterFeatures
#' @importFrom htmltools tags
server_module_samples_filtering_tab <- function(id, step_number) {
    moduleServer(id, function(input, output, session) {
        assays_to_process <- eventReactive(input$reload, {
            error_handler(
                page_assays_subset,
                component_name = "Page assays subset",
                qfeatures = global_rv$qfeatures,
                step_number = step_number - 1
            )
        })
        server_module_qc_metrics("psm_pre", assays_to_process)

        n_boxes <- reactiveVal(0)

        filtering_conditions <- reactiveValues()
        boxes_states <- reactiveValues()
        observeEvent(input$add_box, {
            n_boxes(n_boxes() + 1)
        })

        observeEvent(input$remove_box, {
            if (n_boxes() > 0) {
                n_boxes(n_boxes() - 1)
            }
        })

        observeEvent(n_boxes(), {
            output$filtering_boxes <- renderUI({
                if (n_boxes() > 0) {
                    lapply(seq_len(n_boxes()), function(i) {
                        # Call the server part of the filtering box module and store the output
                        interface_module_filtering_box(
                            NS(id, paste0("filtering_", i)),
                            box_title = paste0("Filtering Box #", i)
                        )
                    })
                }
            })
            if (n_boxes() > 0) {
                lapply(seq_len(n_boxes()), function(i) {
                    res <- server_module_filtering_box(
                        paste0("filtering_", i),
                        assays_to_process,
                        "samples",
                        boxes_states[[paste0("box_", i)]]
                    )

                    observe({
                        filtering_conditions[[paste0("condition_", i)]] <- res$condition()
                        boxes_states[[paste0("box_", i)]] <- list(
                            annotation_selection = res$annotation_selection(),
                            filter_operator = res$filter_operator(),
                            filter_value = res$filter_value()
                        )
                    })
                })
            }

            output$boxes_summary <- renderUI({
                tags$div(
                    tags$p(tags$b(paste0("Number of boxes: ", n_boxes()))),
                    tags$ul(
                        lapply(seq_len(n_boxes()), function(i) {
                            tags$li(
                                style = "font-size: 20px;",
                                class = "list-element",
                                tags$span(
                                    style = "font-size: 15px",
                                    paste0("Filtering Box #", i, ":")
                                ),
                                tags$br(),
                                tags$span(
                                    style = "margin-left: 20px; font-size: 13px;",
                                    paste0(
                                        "Annotation Used: ",
                                        boxes_states[[paste0("box_", i)]]$annotation_selection
                                    )
                                )
                            )
                        }),
                        class = "list-group"
                    )
                )
            })

            output$filtering_summary <- renderUI({
                if (n_boxes() > 0) {
                    tags$ul(
                        lapply(seq_len(n_boxes()), function(i) {
                            tags$li(
                                class = "list-element",
                                style = "font-size: 20px;",
                                tags$span(
                                    style = "font-size: 14px;",
                                    paste0("Filtering Condition #", i, ":"),
                                    tags$br(),
                                    tags$span(
                                        style = "margin-left: 20px;",
                                        tags$b(
                                            filtering_conditions[[paste0("condition_", i)]]
                                        )
                                    )
                                )
                            )
                        }),
                        class = "list-group"
                    )
                }
            })
        })
        filtering_conditions_list <- reactive({
            reactiveValuesToList(filtering_conditions)
        })
        entire_condition <- reactive({
            res <- lapply(seq_len(n_boxes()), function(index) {
                paste0("qfeatures$", filtering_conditions_list()[[index]])
            })
            res <- unlist(res)
            if (length(filtering_conditions_list()) > 0) {
                return(paste(res, collapse = " & "))
            } else {
                return(NULL)
            }
        })

        processed_assays <- eventReactive(
            c(input$apply_filters, assays_to_process()),
            {
                if (length(filtering_conditions_list()) > 0) {
                    error_handler(sample_filtering,
                        component_name = "Sample filtering",
                        qfeatures = assays_to_process(),
                        conditions = entire_condition()
                    )
                } else {
                    return(assays_to_process())
                }
            }
        )
        server_module_qc_metrics("psm_filtered", processed_assays)

        observeEvent(input$export, {
            req(processed_assays())
            loading(paste("Be aware that this operation",
                "can be quite time consuming for large data sets",
                sep = " "
            ))
            error_handler(
                add_assays_to_global_rv,
                component_name = "Add assays to global_rv",
                processed_qfeatures = processed_assays(),
                step_number = step_number,
                type = "samples_filtering"
            )
            removeModal()
        })
    })
}


#' @title sample filtering
#'
#' @param qfeatures A qfeatures object
#' @param conditions A string with the conditions to filter the samples (chr)
#'
#' @return A qfeatures object with the filtered samples
#' @rdname INTERNAL_sample_filtering
#' @keywords internal
#'
sample_filtering <- function(qfeatures, conditions) {
    qfeatures[, eval(parse(text = conditions)), ]
}
