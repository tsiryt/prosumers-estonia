# draws a comparison between actuals and prev
draw_target_vs_prev <- function(data) {
  ggplot(data = data) +
  geom_line(aes(x = datetime, y = puis_MW, col = type)) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b %d %Y")
}
