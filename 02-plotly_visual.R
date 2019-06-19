library(plotly)

sorted_adverse <- adverse %>% 
  group_by(term) %>% 
  summarise(count = sum(count))

plotly::plot_ly(sorted_adverse, x = ~reorder(term, count), y = ~count, type = "bar") %>%
  layout(title = "Adverse Events - Tylenol",
         xaxis = list(title = ""),
         yaxis = list(title = ""))


gender_events <- full_join(male, female, by = 'term')

gender_events %>% 
  plot_ly() %>%
  add_trace(x = ~reorder(term, count.x), y = ~count.x, name = 'male', text = 'male', type = "bar") %>%
  add_trace(x = ~reorder(term, count.y), y = ~count.y, name = 'female', text = 'female', type = "bar")