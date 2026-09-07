#' Retrieve theme values from a Plotly theme configuration
#' Extracts colourway, background colours, and font colour from a
#' JSON‑based Plotly theme definition. The function returns a named list
#' containing the theme components so they can be applied to Plotly figures.
#'
#' @param theme_name Character string indicating the theme to extract. The default value is phs-dark but you can choose phs-light.
#' @return A named list with theme elements
get_theme_values <- function(theme_name = "phs-dark") {
  theme_json <- fromJSON("./custom-plotly.js", simplifyVector = FALSE)

  base_theme <- theme_json[[theme_name]]
  theme_colors <- unlist(base_theme$layout$colorway)
  theme_paper <- base_theme$layout$paper_bgcolor
  theme_bg <- base_theme$layout$plot_bgcolor
  theme_fontcolor <- base_theme$layout$font$color

  return(list(
    theme_colors = theme_colors,
    theme_paper = theme_paper,
    theme_bg = theme_bg,
    theme_fontcolor = theme_fontcolor
  ))
}

#' Apply a Plotly theme to a Plotly figure
#'
#' Adds layout settings such as colourway, background colours, and font
#' styling to an existing Plotly object. The theme options must be supplied
#' as a named list containing elements compatible with `plotly::layout()`.
#' @param p A Plotly object.
#' @param list_theme_options A named list of theme settings, typically
#'   including elements such as theme_colors for colorway, theme_paper for paper_bgcolor,
#'   theme_bg for plot_bgcolor and theme_fontcolor for font.
#' @return The Plotly object with the theme applied.
apply_plotly_theme <- function(p, list_theme_options) {
  p |> 
    layout(
      colorway = list_theme_options$theme_colors,
      paper_bgcolor = list_theme_options$theme_paper,
      plot_bgcolor = list_theme_options$theme_bg,
      font = list(color = list_theme_options$theme_fontcolor)
    )
}

# function to calculate reduction
#' @param micro a dataframe object with benchmark info.
#' @return a concatenated char value
calcpecentage <- function(micro){
  v <- aggregate(time ~ expr, micro, mean) |>
    arrange(time)

  i <- 1
  j <- nrow(v)

  reduction <- ((v$time[j] - v$time[i])*100)/v$time[j]
  reduction <- reduction |> round(digits = 2)

  return(glue("{reduction}% reduction using {v$expr[i]} compared to {v$expr[j]}"))
}

#' Creates a plotly box plot chart based on the benchmark data
#' It also applies customised theme
#' @param data a dataframe object with benchmark info.
#' @param my_scale char value (e.g. miliseconds, microseconds).
#' @param list_theme_options A named list of theme settings
#' @return The Plotly box plot with the theme applied
fbox_plot <- function(data, my_scale, list_theme_options){
  data <- as.data.table(data)
  data$expr <- factor(data$expr)

  my_fig <- plot_ly(data, x = ~log(time), y = ~expr, color = ~expr, split = ~expr, 
                    type = "box", orientation = "h",
                    colors = list_theme_options$theme_colors) |>
    layout(
      title = calcpecentage(data),
      xaxis = list(title = paste0("Time in ", my_scale)),
      yaxis = list(title = "Expression")
    ) |> 
    apply_plotly_theme(list_theme_options)
  return(my_fig)
}
