
---
  title: "Incredible Journey of Tesla Candle stick analysis"
author: "Biswa Pujarini"
date: "26/07/2020"
output:
  html_document
---

library(plotly)
library(quantmod)


getSymbols("TSLA",src='yahoo')
df <- data.frame(Date=index(TSLA),coredata(TSLA))

# create Bollinger Bands
bbands <- BBands(TSLA[,c("TSLA.High","TSLA.Low","TSLA.Close")])

# join and subset data
df <- subset(cbind(df, data.frame(bbands[,1:3])), Date >= "2018-02-14")

# colors column for increasing and decreasing
for (i in 1:length(df[,1])) {
  if (df$TSLA.Close[i] >= df$TSLA.Open[i]) {
    df$direction[i] = 'Increasing'
  } else {
    df$direction[i] = 'Decreasing'
  }
}

i <- list(line = list(color = '#17BECF'))
d <- list(line = list(color = '#7F7F7F'))

# plot candlestick chart

fig <- df %>% plot_ly(x = ~Date, type="candlestick",
                      open = ~TSLA.Open, close = ~TSLA.Close,
                      high = ~TSLA.High, low = ~TSLA.Low, name = "TSLA",
                      increasing = i, decreasing = d) 
fig <- fig %>% add_lines(x = ~Date, y = ~up , name = "B Bands",
                         line = list(color = '#ccc', width = 0.5),
                         legendgroup = "Bollinger Bands",
                         hoverinfo = "none", inherit = F) 
fig <- fig %>% add_lines(x = ~Date, y = ~dn, name = "B Bands",
                         line = list(color = '#ccc', width = 0.5),
                         legendgroup = "Bollinger Bands", inherit = F,
                         showlegend = FALSE, hoverinfo = "none") 
fig <- fig %>% add_lines(x = ~Date, y = ~mavg, name = "Mv Avg",
                         line = list(color = '#E377C2', width = 0.5),
                         hoverinfo = "none", inherit = F) 
fig <- fig %>% layout(yaxis = list(title = "Price"))

# plot volume bar chart
fig2 <- df 
fig2 <- fig2 %>% plot_ly(x=~Date, y=~TSLA.Volume, type='bar', name = "TSLA Volume",
                         color = ~direction, colors = c('#17BECF','#7F7F7F')) 
fig2 <- fig2 %>% layout(yaxis = list(title = "Volume"))

# create rangeselector buttons
rs <- list(visible = TRUE, x = 0.5, y = -0.055,
           xanchor = 'center', yref = 'paper',
           font = list(size = 9),
           buttons = list(
             list(count=1,
                  label='RESET',
                  step='all'),
             list(count=1,
                  label='1 YR',
                  step='year',
                  stepmode='backward'),
             list(count=3,
                  label='3 MO',
                  step='month',
                  stepmode='backward'),
             list(count=1,
                  label='1 MO',
                  step='month',
                  stepmode='backward')
           ))

# subplot with shared x axis
fig <- subplot(fig, fig2, heights = c(0.7,0.2), nrows=2,
               shareX = TRUE, titleY = TRUE)
fig <- fig %>% layout(title = paste("Tesla: 2018-02-14 -",Sys.Date()),
                      xaxis = list(rangeselector = rs),
                      legend = list(orientation = 'h', x = 0.5, y = 1,
                                    xanchor = 'center', yref = 'paper',
                                    font = list(size = 10),
                                    bgcolor = 'transparent'))

fig