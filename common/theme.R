# Shared plotting theme, palette, and helpers for blog figures
library(ggplot2)

blog_cols <- c(
  red    = "#E74C3C",
  blue   = "#3498DB",
  green  = "#2ECC71",
  purple = "#9B59B6",
  dark   = "#2C3E50",
  grey   = "grey40"
)

theme_blog <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background = element_rect(fill = "white", colour = NA),
      legend.position = "top",
      legend.title = element_blank()
    )
}

# Blank theme for hand-drawn diagrams
theme_diagram <- function() {
  theme_void() +
    theme(
      plot.margin = margin(10, 10, 10, 10),
      plot.background = element_rect(fill = "white", colour = NA)
    )
}

theme_set(theme_blog())

arrow_closed <- function(size = 0.15) {
  arrow(length = unit(size, "inches"), type = "closed")
}

# x-axis (with arrow) and y-axis lines for hand-drawn diagrams
diagram_axes <- function(x0, x1, y0, y1, y_arrow = FALSE) {
  list(
    annotate("segment", x = x0, xend = x1, y = y0, yend = y0,
             arrow = arrow_closed(), linewidth = 0.5),
    annotate("segment", x = x0, xend = x0, y = y0, yend = y1,
             arrow = if (y_arrow) arrow_closed() else NULL, linewidth = 0.5)
  )
}

# Underlying two-state process observed at intermittent times
process_plot <- function(change_times, obs_times, missed_times = NULL, end = 12) {
  edges <- c(0, change_times, end)
  n <- length(edges) - 1
  seg <- data.frame(
    x = edges[-(n + 1)], xend = edges[-1],
    state = rep(c(1, 2), length.out = n)
  )
  jump <- data.frame(
    x = change_times,
    from = rep(c(1, 2), length.out = length(change_times)),
    to = rep(c(2, 1), length.out = length(change_times))
  )
  key <- data.frame(
    label = c("Underlying process", "Observation times", "Missed observations"),
    y = c(2.2, 1.9, 1.6),
    colour = c(blog_cols[["red"]], "gray50", "gray75"),
    lty = c("solid", "dashed", "dotted"),
    lw = c(1.2, 0.6, 0.6)
  )
  if (is.null(missed_times)) key <- key[1:2, ]

  p <- ggplot() +
    annotate("segment", x = obs_times, xend = obs_times, y = 0.5, yend = 2.3,
             linetype = "dashed", colour = "gray50", linewidth = 0.6)
  if (!is.null(missed_times)) {
    p <- p + annotate("segment", x = missed_times, xend = missed_times,
                      y = 0.5, yend = 2.3, linetype = "dotted",
                      colour = "gray75", linewidth = 0.6)
  }
  p +
    geom_segment(data = seg, aes(x = x, xend = xend, y = state, yend = state),
                 colour = blog_cols[["red"]], linewidth = 1.2) +
    geom_segment(data = jump, aes(x = x, xend = x, y = from, yend = to),
                 colour = blog_cols[["red"]], linewidth = 1.2) +
    geom_segment(data = key, aes(x = 13, xend = 13.5, y = y, yend = y,
                                 colour = colour, linetype = lty, linewidth = lw)) +
    geom_text(data = key, aes(x = 14.5, y = y, label = label),
              hjust = 0, size = 3.5) +
    scale_colour_identity() + scale_linetype_identity() +
    scale_linewidth_identity() +
    annotate("text", x = -0.5, y = c(1, 2), label = c("State 1", "State 2"),
             hjust = 1, size = 3.5) +
    annotate("segment", x = -0.1, xend = 0, y = c(1, 2), yend = c(1, 2),
             linewidth = 0.5) +
    annotate("text", x = obs_times, y = 0.3, label = obs_times, size = 3.5) +
    annotate("text", x = end / 2, y = 0.05, label = "Follow-up time (weeks)",
             size = 3.5) +
    diagram_axes(0, end + 0.5, 0.5, 2.3) +
    scale_x_continuous(limits = c(-2, 19), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 2.5), expand = c(0, 0)) +
    theme_diagram()
}

# Boxes and arrows for state diagrams (competing risks and multi-state models)
state_box <- function(x, y, label, w = 1.5, h = 0.6, parse = FALSE) {
  list(
    annotate("rect", xmin = x - w / 2, xmax = x + w / 2,
             ymin = y - h / 2, ymax = y + h / 2,
             fill = NA, colour = "grey20", linewidth = 0.5),
    annotate("text", x = x, y = y, label = label, size = 4.2, parse = parse)
  )
}

state_arrow <- function(x, y, xend, yend, curvature = 0, linetype = "solid") {
  annotate("curve", x = x, y = y, xend = xend, yend = yend,
           curvature = curvature, linetype = linetype,
           arrow = arrow_closed(0.12), linewidth = 0.5)
}

# Simple DAG drawn with ggplot2
# nodes: data.frame(name, x, y, label); edges: data.frame(from, to) with
# optional curvature, pad (gap left at each end), and arrow (FALSE gives a
# dashed association line); `boxed` names the conditioned-on nodes.
dag_plot <- function(nodes, edges, boxed = character(0), circles = TRUE,
                     xlim = NULL, ylim = NULL) {
  if (is.null(edges$curvature)) edges$curvature <- 0
  if (is.null(edges$pad)) edges$pad <- 0.18
  if (is.null(edges$arrow)) edges$arrow <- TRUE
  i <- match(edges$from, nodes$name)
  j <- match(edges$to, nodes$name)
  dx <- nodes$x[j] - nodes$x[i]
  dy <- nodes$y[j] - nodes$y[i]
  len <- sqrt(dx^2 + dy^2)
  edges$x <- nodes$x[i] + dx / len * edges$pad
  edges$y <- nodes$y[i] + dy / len * edges$pad
  edges$xend <- nodes$x[j] - dx / len * edges$pad
  edges$yend <- nodes$y[j] - dy / len * edges$pad

  p <- ggplot()
  for (k in seq_len(nrow(edges))) {
    p <- p + geom_curve(
      data = edges[k, ],
      aes(x = x, y = y, xend = xend, yend = yend),
      curvature = edges$curvature[k],
      colour = "grey40", linewidth = 0.6,
      linetype = if (edges$arrow[k]) "solid" else "dashed",
      arrow = if (edges$arrow[k]) arrow_closed(0.1) else NULL
    )
  }
  if (circles) {
    p <- p +
      geom_point(data = nodes, aes(x = x, y = y), size = 14,
                 colour = blog_cols[["blue"]]) +
      geom_text(data = nodes, aes(x = x, y = y, label = label),
                colour = "white", size = 5)
  } else {
    p <- p +
      geom_label(data = nodes[!nodes$name %in% boxed, ],
                 aes(x = x, y = y, label = label), linewidth = 0, size = 5,
                 lineheight = 0.9) +
      geom_label(data = nodes[nodes$name %in% boxed, ],
                 aes(x = x, y = y, label = label), linewidth = 0.6, size = 5,
                 lineheight = 0.9)
  }
  p +
    scale_x_continuous(limits = xlim, expand = expansion(0.2)) +
    scale_y_continuous(limits = ylim, expand = expansion(0.2)) +
    theme_diagram()
}
