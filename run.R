# The shiny app to identify the visual breakpoints. Comment out
# when you want to perform the manual identification.
# shiny::runApp("Scripts/ManualRating/manual_rating_app.R")

targets::tar_visnetwork()

targets::tar_make()

print(targets::tar_meta(fields = error), n = Inf)
print(targets::tar_meta(fields = warnings, complete_only = TRUE), n = Inf)
