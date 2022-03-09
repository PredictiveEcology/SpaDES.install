utils::globalVariables(c(
  "Account", "destFile", "filepath", "GitSubFolder", "hasSubFolder", "Repo", "repoLocation"
))

#' non-exported objects and functions from other packages
#'
#' @importFrom utils getFromNamespace
#' @keywords internal
#' @rdname imports
DESCRIPTIONFileDeps <- utils::getFromNamespace("DESCRIPTIONFileDeps", "Require")

#' @rdname imports
DESCRIPTIONFileVersionV <- utils::getFromNamespace("DESCRIPTIONFileVersionV", "Require")

#' @rdname imports
getGitHubFile <- utils::getFromNamespace("getGitHubFile", "Require")

#' @rdname imports
isWindows <- utils::getFromNamespace("isWindows", "Require")

#' @rdname imports
messageDF <- utils::getFromNamespace("messageDF", "Require")
