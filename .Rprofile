options(
    device = function(...) {
        pdf(NULL, bg = "white", ...)
        dev.control(displaylist = "enable")
    }
)
# setHook("plot.new", new_plot)
# setHook("grid.newpage", new_plot)
# function(...) {
#     out <- .Primitive(".External.graphics")(...)
#     if (check_null_dev()) {
#         plot_updated <<- TRUE
#     }
#     out
# }

# update_plot <- function(...) {
#     tryCatch(
#         {
#             if (plot_updated) {
#                 plot_updated <<- FALSE
#                 record <- recordPlot()
#                 if (length(record[[1L]])) {
#                     png(filename = plot_file)
#                     on.exit(dev.off())
#                     replayPlot(record)
#                 }
#             }
#         },
#         error = message
#     )
#     TRUE
# }
# addTaskCallback(update_plot)

if (interactive() && Sys.getenv("TERM_PROGRAM") == "vscode") {
    if ("httpgd" %in% .packages(all.available = TRUE)) {
        options(vsc.plot = FALSE)
        options(device = function(...) {
            httpgd::hgd(silent = TRUE)
            .vsc.browser(httpgd::hgd_url(), viewer = "Beside")
        })
    }
}