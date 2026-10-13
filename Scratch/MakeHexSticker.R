
library(ggplot2)
library(hexSticker)
source("Scratch/get_example_bssd_fit.R")
fit <- get_example_bssd_fit()
fit

# Use plot of bssd_binary object ----
p <- plot(fit)
p <- p + theme_void() + theme_transparent()
p <- p + theme(legend.position = c(0.9, 0.6))
p <- p + theme(legend.key.size = unit(0.3, 'cm'))
p

sticker(
  p,
  package = "bssd",
  p_size = 20,
  s_x = 1,
  s_y = 0.75,
  s_width = 1.0,
  s_height = 0.9,
  filename="Scratch/logo1.png",
  h_fill="#f9690e", h_color="#f39c12"
)


# Change colours ----
library(tidyverse)
# library(viridis)
plot.bssd_binary <- function(x, ...) {
  pi1 <- pi2 <- value <- name <- NULL

  as.data.frame(x$fit) %>%
    select(pi1, pi2) %>%
    pivot_longer(cols = c("pi1", "pi2")) %>%
    mutate(name = fct_recode(name, `Arm 1` = "pi1", `Arm 2` = "pi2")) %>%
    ggplot(aes(x = value, fill = name, colour = name)) +
    geom_density(alpha = 0.7) +
    scale_colour_viridis_d() +
    scale_fill_viridis_d() +
    labs(x = "Prob(event)", fill = "", colour = "")
}

p <- plot(fit)
p <- p + theme_void() + theme_transparent()
p <- p + theme(legend.position = c(0.9, 0.6))
p <- p + theme(legend.key.size = unit(0.3, 'cm'))
p

sticker(
  p,
  package = "bssd",
  p_size = 20,
  s_x = 1,
  s_y = 0.75,
  s_width = 1.0,
  s_height = 0.9,
  filename="Scratch/logo2.png",
  h_fill="#f9690e", h_color="#f39c12"
)


# Use one ----
usethis::use_logo(img = "Scratch/logo1.png", geometry = "240x278", retina = TRUE)
