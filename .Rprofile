# Still load the user's Rprofile before this one
if (file.exists("~/.Rprofile")) {
  source("~/.Rprofile")
}

# Need three packages
if (
  requireNamespace('later', quietly = TRUE) &&
    requireNamespace('rstudioapi', quietly = TRUE) &&
    requireNamespace('svMisc', quietly = TRUE)
) {
  later::later(
    function() {
      save_all <- rstudioapi::documentSaveAll
      register <- rstudioapi::registerCommandCallback
      assign_temp <- svMisc::assign_temp

      # Save files before diff, commit, pull or push
      if (rstudioapi::isAvailable() && !svMisc::exists_temp('.handle_diff')) {
        assign_temp('.handle_diff', register("vcsDiff", save_all))
        assign_temp('.handle_commit', register("vcsCommit", save_all))
        assign_temp('.handle_pull', register("vcsPull", save_all))
        assign_temp('.handle_push', register("vcsPush", save_all))
      }

      # Check if one should do a pull because there are changes in the remote repo
      res <- try(
        suppressWarnings(system2(
          "git",
          c("fetch", "--dry-run"),
          stdout = TRUE,
          stderr = TRUE
        )),
        silent = TRUE
      )
      if (
        !inherits(res, "try-error") &&
          is.null(attr(res, 'status')) &&
          any(grepl("origin/", res, fixed = TRUE))
      ) {
        rstudioapi::showDialog(
          "Science des donn\u00e9es biologiques",
          paste0(
            "<b>Attention:</b> ",
            "des modifications dans le d\u00e9p\u00f4t GitHub ",
            "ne sont pas r\u00e9percut\u00e9es localement.\n\n",
            "<i>Faites un pull (après avoir committ\u00e9 vos changements ",
            "\u00e9ventuels) pour synchroniser votre d\u00e9p\u00f4t.</i>"
          )
        )
      }
    },
    3
  )
}
