interface_module_create_annotation_tab <- function(id) {
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
        fluidRow(
            box(
                title = "Annotation Configuration",
                status = "primary",
                width = 4,
                solidHeader = TRUE,
                collapsible = TRUE,
                style = "min-height: 200px;",
                selectInput(
                    inputId = NS(id, "feature_id"),
                    label = "Feature ID",
                    choices = NULL
                ),
                selectInput(
                    inputId = NS(id, "protein_id"),
                    label = "Protein ID",
                    choices = NULL
                )
            ),
            box(
                title = "Annotation checklist",
                status = "primary",
                width = 8,
                solidHeader = TRUE,
                collapsible = FALSE,
                style = "min-height: 200px;",
                ## Check box for combining annotations
                checkboxInput(
                    inputId = NS(id, "combine_annot"),
                    label = "Combine multiple annotations"
                ),
                p("Check this box to create an new feature annotation ",
                  "that is the combination of other feature ",
                  "annotations. This is particularly useful for ",
                  "creating a unique feature identifier.",
                ),
                selectInput(
                    inputId = NS(id, "vars_to_combine"),
                    label = NULL,
                    choices = NULL,
                    multiple = TRUE,
                    width = "100%"
                ),
                p(
                    "The annotation is stored in the column name below:"
                ),
                textInput(
                    inputId = NS(id, "combine_varnam"),
                    label = NULL,
                    value = "featureID",
                    width = "50%"
                ),
                ## Check box for inconsistent inference
                checkboxInput(
                    inputId = NS(id, "inconsistent"),
                    label = "Annotate inconsistent protein inference"
                ),
                p(paste0(
                    "Check this box to annotate features that ",
                    "inconsistently map to different protein IDs.",
                    "The annotation is stored in the column name below:"
                )),
                textInput(
                    inputId = NS(id, "inconsistent_varnam"),
                    label = NULL,
                    value = "isInconsistentProteinInference",
                    width = "50%"
                ),
                ## Check box for one-hit wonders
                checkboxInput(
                    inputId = NS(id, "one_hit_wonders"),
                    label = "Annotate one-hit wonders"
                ),
                p(paste0(
                    "Check this box to annotate one-hit wonders ",
                    "which are proteins for which the identification ",
                    "relies on a single feature. The annotation is ",
                    "stored in the column name below:"
                )),
                textInput(
                    inputId = NS(id, "one_hit_wonders_varnam"),
                    label = NULL,
                    value = "isOneHitWonder",
                    width = "50%"
                ),
                ## Check box for one-run wonders
                checkboxInput(
                    inputId = NS(id, "one_run_wonders"),
                    label = "Annotate one-run wonders"
                ),
                p(paste0(
                    "Check this box to annotate one-run wonders ",
                    "which are proteins (defined by \"Protein ID\") ",
                    "that are detected only in a single run ",
                    "The annotation is stored in the column name below:"
                )),
                textInput(
                    inputId = NS(id, "one_run_wonders_varnam"),
                    label = NULL,
                    value = "isOneRunWonder",
                    width = "50%"
                ),
                ## Check box for duplicated feature IDs
                checkboxInput(
                    inputId = NS(id, "duplicated"),
                    label = "Annotate duplicated feature IDs"
                ),
                p(paste0(
                    "Check this box to annotate features that have ",
                    "duplicated identifiers within a run.",
                    "The annotation is stored in the column name below:"
                )),
                textInput(
                    inputId = NS(id, "duplicated_varnam"),
                    label = NULL,
                    value = "isDuplicated",
                    width = "50%"
                ),
                p(paste0(
                    "Duplicated features are also automatically ",
                    "ranked based on their intensity in decreasing ",
                    "order. In case of labelling, the summed reporter ",
                    "intensity is used for ranking. ",
                    "The annotation is stored in the column name below:"
                )),
                textInput(
                    inputId = NS(id, "duplicated_rank_varnam"),
                    label = NULL,
                    value = "duplicatedRank",
                    width = "50%"
                ),
                ## Check box for add missing value numbers
                checkboxInput(
                    inputId = NS(id, "missing_val"),
                    label = "Add missing values numbers"
                ),
                p(paste0(
                    "Check this box to annotate features and samples ",
                    "with the amount and proportion of missing values.",
                    "The amount and proportion of missingness will be ",
                    "automatically stored in the 'nNA' and 'pNA' ",
                    "columns, respectively."
                )),
                actionButton(
                    inputId = NS(id, "annotate"),
                    label = "Perform annotation"
                )
            ),
            box(
                title = "Preview feature annotations",
                status = "primary",
                width = 12,
                solidHeader = FALSE,
                collapsible = TRUE,
                DT::dataTableOutput(NS(id, "rowdata_preview"))
            )
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


interface_module_annotation_box <- function(id, fun, box_title) {
    box_settings <- list(
        title = box_title,
        status = "primary",
        width = 4,
        solidHeader = TRUE,
        collapsible = TRUE
    )
    box_fun <- list(
        selectInput(
            inputId = NS(id, "function"),
            label = "Annotation functions to use",
            choices = annotators
        )
    )
    fun_args <- formals(get(fun))[-(1:2)] ## remove the "object" (1st) and "i" (2nd) args
    box_fun_params <- lapply(names(fun_args), function(i) {
        textInput(
            inputId = NS(id, i),
            label = i,
            value = "",
            placeholder = as.character(fun_args[[1]])
        )
    })
    do.call(box, c(box_settings,
                   box_desription,
                   box_fun,
                   uiOutput(NS(id, "annotation_box_arguments"))))
}

annotators <- c(
    "spotInconsistentProteinInference", "spotOneHitWonders",
    "spotRunWonders", "rankDuplicatedPsms", "spotDuplicatedPsms"
)
