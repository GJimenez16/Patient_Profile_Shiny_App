# Patient Profile plots

# Distribution plot for age 
agedist_plot <- plot_ly(
  data$adsl,
  x = ~AGE,
  color = ~SEX,
  colors = c("#588157", "#344E41"),
  type = "histogram"
) %>% 
  layout(
    xaxis = list(title = "Age"),
    yaxis = list(title = "Count"),
    barmode = "stack",
    color = "#344E41"
  )

# Distribution plot for race
racedist_plot <- plot_ly(
  data$adsl,
  y = ~RACE,
  type = "histogram",
  orientation = "h"
) %>% 
  layout(
    xaxis = list(title = ""),
    yaxis = list(title = "")
  )

# Gantt plot for ae coloured by severity 
ae_gantt_plot <- function(adae_data) {
  
  data_plot <- adae_data %>% 
    mutate(ASTDTM = as.Date(ASTDTM, format = "%m/%d/%Y"),
           AENDTM = as.Date(AENDTM, format = "%m/%d/%Y"))
  
  if(nrow(data_plot) == 0) {
    
    return(
      plot_ly() %>% 
        add_annotations(
          text = "No adverse events data available for this subject",
          x = 0.5,
          y = 0.5,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          font = list(size = 20)
        ) %>%
        layout(
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
        )
    )
    
  } else {
    
    fig <- plot_ly()
    
    color_mapping <- c("1" = "green",
                       "2" = "yellow",
                       "3" = "orange",
                       "4" = "red",
                       "5" = "darkred")
    
    fig <- plot_ly()
    
    for(i in 1:(nrow(data_plot) - 1)) {
      fig <- add_trace(fig,
                       x = c(data_plot$ASTDTM[i], data_plot$AENDTM[i]),
                       y = c(i, i),
                       mode = "lines",
                       line = list(width = 50,
                                   color = color_mapping[as.character(data_plot$AETOXGR[i])]),
                       showlegend = FALSE,
                       hoverinfo = "text",
                       # Custom hover text
                       text = paste("Adverse Event:", data_plot$AETERM[i], "<br>",
                                    "Severity:", data_plot$AESEV[i], "<br>",
                                    "Grade:", data_plot$AETOXGR[i], "<br>",
                                    "Duration:", data_plot$AENDTM[i] - data_plot$ASTDTM[i]),
                       evaluate = TRUE
      )
    }
    
    fig <- layout(
      fig,
      yaxis = (list(showgrid = FALSE,
                    tickmode = "array",
                    tickvals = 1:nrow(data_plot),
                    ticktext = unique(data_plot$AETERM)))
    )
    
    fig
  } 
}


# Timeline for selected labs
labs_plot <- function(adlb_data) {
  plot_ly(adlb_data, 
          x = ~AVISITN,
          y = ~AVAL,
          color = ~paste(LBTEST, " (", AVALU, ")", sep = ""),
          type = "scatter",
          mode = "lines") %>% 
    layout(
      xaxis = list(title = "Day of Visit"),
      yaxis = list(title = "Result",
                   type = "log")
    )
}
 