library(tidyverse)
library(ggplot2)
requireNamespace('gridExtra', quietly = TRUE)

COLORPAL <- c(gray = '#666666',
              blue = '#0072B2',
              green = '#009E73',
              orange = '#D55E00',
              lightblue = '#56B4E9',
              magenta = '#CC79A7')

plottheme <- function(base_size = 12, base_family = '') {
  theme_bw(base_size = base_size, base_family = base_family) +
    theme(plot.title = element_text(size = rel(1), hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(size = rel(0.8), hjust = 0.5),
          plot.margin = margin(0.2, 0.4, 0.5, 0.2, 'lines'),
          panel.background = element_rect(fill = 'transparent', color = NA),
          plot.background = element_rect(fill = 'transparent', color = NA),
          legend.title = element_text(size = rel(0.7)),
          legend.text = element_text(size = rel(0.7)),
          axis.title = element_text(size = rel(0.75)),
          axis.text = element_text(size = rel(0.7)),
          axis.title.y = element_text(vjust = 1),
          axis.title.x = element_text(vjust = 0),
          panel.grid.major = element_line(
            color = 'gray30', linewidth = rel(0.5), linetype='dotted'),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(
            fill = '#ffffff', color = 'gray50', linewidth = 0.3),
          strip.text = element_text(size = rel(0.75)),
          panel.border = element_rect(color = 'gray50', linewidth = 0.3),
          legend.background = element_blank())
}


plot_distances <- function (...,
                            x = c("lambda", "lambdaind", "l0norm", "l1norm", "l2norm"),
                            y = c("l0norm", "l1norm", "l2norm"),
                            std_x = FALSE) {
  dist <- function (cv_res) {
    b <- coef(cv_res, lambda = min(cv_res$lambda[[1]]))
    tibble(lambda = cv_res$lambda[[1]],
           lambdaind = seq_along(cv_res$lambda[[1]]),
           l0_norm = map_dbl(cv_res$lambda[[1]], \ (l) {
             a <- coef(cv_res, lambda = l)
             sum(abs(a) > .Machine$double.eps) - 1
           }),
           l1_norm = map_dbl(cv_res$lambda[[1]], \ (l) {
             a <- coef(cv_res, lambda = l)
             sum(abs(a[-1]))
           }),
           l2_norm = map_dbl(cv_res$lambda[[1]], \ (l) {
             a <- coef(cv_res, lambda = l)
             (sum(a[-1]^2))
           }))
  }

  objs <- list(...)
  x <- match.arg(x)
  y <- match.arg(y)

  gd <- guide_legend(title = NULL)

  pd <- objs %>%
    map(dist) %>%
    list_rbind(names_to = 'method')

  if (identical(x, "lambda")) {
    pd <- rename(pd, x = lambda)
    x_scale <- scale_x_log10(name = expression(lambda))
  } else if (identical(x, "lambdaind")) {
    pd <- rename(pd, x = lambdaind)
    x_scale <- scale_x_continuous(transform = "reverse", name = expression(lambda-"index"))
  } else if (identical(x, "l0norm")) {
    pd <- mutate(pd, x = l0_norm)
    x_scale <- scale_x_continuous(name = "Nr. of non-zero coefficients")
  } else if (identical(x, "l1norm")) {
    pd <- mutate(pd, x = l1_norm)
    x_scale <- scale_x_continuous(name = expression(paste(L[1], " norm")))
  } else if (identical(x, "l2norm")) {
    pd <- mutate(pd, x = l2_norm)
    x_scale <- scale_x_continuous(name = expression(paste(L[2], " norm")))
  }

  if (identical(y, "l0norm")) {
    pd <- rename(pd, y = l0_norm)
    y_lab <- "Nr. of non-zero coefficients"
  } else if (identical(y, "l1norm")) {
    pd <- rename(pd, y = l1_norm)
    y_lab <- expression(paste(L[1], " norm"))
  } else if (identical(y, "l2norm")) {
    pd <- rename(pd, y = l2_norm)
    y_lab <- expression(paste(L[2], " norm"))
  }

  if (isTRUE(std_x)) {
    pd <- pd |>
      mutate(min_x = min(x),
             max_x = max(x)) |>
      group_by(method) |>
      mutate(x = min_x + (max_x - min_x) * (x - min(x)) / diff(range(x)))
  }

  pd %>%
    ggplot(aes(x = x,
               y = y,
               color = method,
               linetype = method)) +
    geom_point(aes(shape = method), size = 2) +
    geom_path(linewidth = 0.5) +
    x_scale +
    scale_shape_manual(guide = gd, values = c(1, 16)) +
    scale_linetype_manual(guide = gd, values = c('solid', '42')) +
    scale_color_manual(guide = gd, values = unname(COLORPAL[c('green', 'blue', 'orange')])) +
    labs(y = y_lab) +
    plottheme()
}

log10_labeller <- function (x, max_digits = 3) {
  \ (x) {
    e <- floor(log10(x))
    m <- x / 10^e

    expr <- if (any(abs(e) > max_digits, na.rm = TRUE)) {
      map2(m, e, \(m, e) {
        if (is.na(m)) {
          expression(paste(""))
        } else if (abs(m - 1) > .Machine$double.eps) {
          as.expression(bquote(.(m) %*% 10^.(e)))
        } else {
          as.expression(bquote(10^.(e)))
        }
      })
    } else {
      map2(x, e, \(x, e) {
        if (is.na(x)) {
          expression(paste(""))
        } else {
          x <- formatC(x, digits = abs(min(e, 0)), format = "f")
          bquote(paste(.(x)))
        }
      })
    }

    do.call(c, expr)
  }
}
