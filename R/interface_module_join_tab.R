#' Assay joining tab (section) ui builder
#'
#' @return A shiny tagList object that contains the join tab UI components
#' @rdname INTERNAL_interface_module_join_tab
#' @keywords internal
#'
#' @importFrom shiny fluidRow NS actionButton icon uiOutput
#' @importFrom shinydashboardPlus box
#' @importFrom htmltools tagList h2
#' @importFrom shinyBS bsTooltip
#'
interface_module_join_tab <- function(id) {
    tagList(
        actionButton(
            NS(id, "reload"),
            "Load assays from previous step",
            icon("hand-pointer", class = "fa-solid"),
            width = "100%",
            class = "load-button"
        ),
        shinyBS::bsTooltip(
            id = NS(id, "reload"),
            title = paste("Load the assays from the previous step.",
                          "Click on this button the first time you visit this page",
                          "or if you updated the assays from the previous steps.",
                          sep = " "
            ),
            trigger = "hover"
        ),
        box(
            title = "Join assays",
            status = "primary",
            width = 12,
            solidHeader = TRUE,
            collapsible = FALSE,
            p(paste0(
                "You don't need to specify parameters when ",
                "joining assays. Assays from the previous ",
                "step will be automatically joined based on ",
                "the rownames."
            )),
            uiOutput(NS(id, "rownames"))
        ),
        actionButton(
            NS(id, "export"),
            "Save the processed assays",
            icon("hand-pointer", class = "fa-solid"),
            width = "100%",
            class = "load-button"
        ),
        shinyBS::bsTooltip(
            id = NS(id, "export"),
            title = paste("Write the processed assays to the QFeatures object.",
                          "This is needed to proceed to the next steps.",
                          sep = " "
            ),
            trigger = "hover",
            placement = "top"
        )
    )
}
