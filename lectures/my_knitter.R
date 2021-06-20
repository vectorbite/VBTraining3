## knitter functions for changing colors, etc

colorize <- function(x, color) {
  if (knitr::is_latex_output()) {
    sprintf("\\textcolor{%s}{%s}", color, x)
  } else if (knitr::is_html_output()) {
    sprintf("<span style='color: %s;'>%s</span>", color, x)
  } else x
}

sk2 <- function() {
  if (knitr::is_latex_output()) {
    sprintf("\\bigskip")
  } else if (knitr::is_html_output()) {
    sprintf("<br><br>")
  } else NULL
}

sk1 <- function() {
  if (knitr::is_latex_output()) {
    sprintf("\\medskip")
  } else if (knitr::is_html_output()) {
    sprintf("<br>")
  } else NULL
}


myblue <- function(x) {
  if (knitr::is_latex_output()) {
    sprintf("\\textcolor{%s}{%s}", "blue", x)
  } else if (knitr::is_html_output()) {
    sprintf("<span style='color: %s;'>%s</span>", "dodgerblue", x)
  } else x
}

myred <- function(x) {
  if (knitr::is_latex_output()) {
    sprintf("\\textcolor{%s}{%s}", "red", x)
  } else if (knitr::is_html_output()) {
    sprintf("<span style='color: %s;'>%s</span>", "red", x)
  } else x
}


mygrn <- function(x) {
  if (knitr::is_latex_output()) {
    sprintf("\\textcolor{%s}{%s}", "FG", x)
  } else if (knitr::is_html_output()) {
    sprintf("<span style='color: %s;'>%s</span>", "ForestGreen", x)
  } else x
}


