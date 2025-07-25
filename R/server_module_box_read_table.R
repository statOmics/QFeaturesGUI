#' A server module that contains the server logic to the read_table box module
#'
#' @param id module id
#' @param given_table a dataframe that contains the table given by the user
#'
#' @return the table reactiveVal that contains a dataframe
#' @rdname INTERNAL_box_read_table_server
#' @keywords internal
#'
#' @importFrom shiny moduleServer observe observeEvent reactiveVal req
#' @importFrom utils data read.table
box_read_table_server <- function(id, given_table = NULL) {
    moduleServer(id, function(input, output, session) {
        table <- reactiveVal()
        observe(
            if (!is.null(given_table)) {
                table(given_table)
            }
        )

        observeEvent(
            input$import_button,
            {
                req(input$file)
                loading("Be aware that this operation can be quite time consuming for large data sets")
                new_table <- error_handler(
                    fread,
                    component_name = paste0("read.table ", id),
                    input$file$datapath,
                    sep = input$sep,
                    dec = input$dec,
                    #skip = input$skip,
                    stringsAsFactors = input$stringsAsFactors,
                    #comment.char = input$comment_char,
                    #header = TRUE,
                    #row.names = 1
                )
                if (exists("new_table"))
                  for (i in which(
                    sapply(new_table,class) == "integer64")
                    ) new_table[[i]] <- as.numeric(new_table[[i]])
                removeModal()
                table(new_table)
            }
        )


        output$dt_table <- DT::renderDataTable({
            req(table())
            DT::datatable(table(),
                extensions = "FixedColumns",
                options = list(
                    searching = FALSE,
                    scrollX = TRUE,
                    fixedColumns = TRUE,
                    pageLength = 5,
                    lengthMenu = c(5, 10, 15)
                )
            )
        })

        table
    })
}
