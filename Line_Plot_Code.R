library(plotly)
library(readr)
library(tidyr)
library(dplyr)
library(viridis)
library(htmlwidgets)

selected_countries <- c("China", "India", "United Arab Emirates", "Egypt",
                        "Iran (Islamic Republic of)", "Brazil", "South Africa",
                        "Russian Federation", "Ethiopia")
hdi_line <- clean_HDI_data %>%
  filter(country %in% selected_countries)
hdi_line_long <- hdi_line %>%
  gather(key = "year", value = "HDI", `1990`, `2000`, `2010`, `2015`, `2019`, `2020`, `2021`, `2022`)
colnames(hdi_line_long)
colnames(hdi_line_long)[which(names(hdi_line_long) == "year")] <- "Year"
hdi_line_long$HDI <- as.numeric(as.character(hdi_line_long$HDI))
glimpse(hdi_line_long)

fig <- plot_ly(hdi_line_long, y = ~HDI,
               x = ~Year,
               color = ~country,
               colors = viridis_pal(option = "D")(length(unique(hdi_line_long$country))),
               text = ~paste0(country, '<br>Year: ', Year, '<br>HDI Value: ', HDI),
               type = 'scatter', mode = 'lines+markers') %>%
  layout(legend = list(orientation = 'h'),
         title = 'Human Development Index Trends in BRICS Countries (1990-2022)')
fig
saveWidget(fig, "Visuals/hdi_plot.html", selfcontained = F, libdir = "lib")
