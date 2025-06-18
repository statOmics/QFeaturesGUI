##' @importFrom QFeatures nNA
##' @importFrom shiny bindEvent updateCheckboxInput
server_module_create_annotation_tab <- function(id, step_number) {
    moduleServer(id, function(input, output, session) {
        ## Get the assays to process when the button to load data from
        ## the previous step is clicked
        assays_to_process <- eventReactive(input$reload, {
            error_handler(page_assays_subset,
                          component_name = "Page assays subset",
                          qfeatures = global_rv$qfeatures,
                          step_number = step_number - 1
            )
        })

        ## Setup available rowdata variable names for providing
        ## feature_id and protein_id, and vars_to_combine
        rowdata_names <- reactive({
            req(assays_to_process())
            annotation_cols(assays_to_process(), "rowData")
        })
        observe({
            updateSelectInput(
                inputId = "feature_id",
                choices = as.character(rowdata_names())
            )
        })
        observe({
            updateSelectInput(
                inputId = "protein_id",
                choices = as.character(rowdata_names())
            )
        })
        observe({
            updateSelectInput(
                inputId = "vars_to_combine",
                choices = as.character(rowdata_names())
            )
        })

        ## The annotation happens here
        processed_assays <- reactive({
            loading("Annotating data...")
            out <- error_handler(
                annotate_qfeatures,
                qfeatures = assays_to_process(),
                component_name = "annotation",
                input = input
            )
            removeModal()
            out
        }) |>
            bindEvent(input$annotate)

        ## Prompt that some annotations are useless when there is only
        ## one run
        observeEvent(input$inconsistent, {
            req(assays_to_process())
            if (length(assays_to_process()) <= 1) {
                prompt_useless_annotation("inconsistent protein inference")
                updateCheckboxInput(inputId = "inconsistent", value = FALSE)
            }
        })
        observeEvent(input$one_run_wonders, {
            req(assays_to_process())
            if (length(assays_to_process()) <= 1) {
                prompt_useless_annotation("one run wonders")
                updateCheckboxInput(inputId = "one_run_wonders", value = FALSE)
            }
        })

        ## Generate a preview of the rowdata
        observeEvent(processed_assays(), {
            output$rowdata_preview <- DT::renderDataTable({
                DT::datatable(
                    data.frame(rowData(processed_assays())[[1]]),
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
        })

        ## When the export button is clicked, save the annotated
        ## QFeatures object in the global reactive variable
        observeEvent(input$export, {
            req(processed_assays())
            loading(paste(
                "Be aware that this operation",
                "can be quite time consuming for large data sets",
                sep = " "
            ))
            error_handler(
                add_assays_to_global_rv,
                component_name = "Add assays to global_rv",
                processed_qfeatures = processed_assays(),
                step_number = step_number,
                type = "annotation"
            )
            removeModal()
        })
    })
}
