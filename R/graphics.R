# draws a comparison between actuals and prev
draw_target_vs_prev <- function(data) {
  ggplot(data = data) +
  geom_line(aes(x = datetime, y = puis_MW, col = type)) +
  facet_grid(vars(is_consumption), scales = "free_y")
}
