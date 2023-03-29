library(tidyverse)
library(cowplot)

functions <- c("dQ", "invQ", "qfgld-original-parametrisation", "quad")
purrr::walk(functions, ~ source(paste0("functions/", .x, ".R")))

ylims <- c(0, 0.25)

d <- c(0, 0.1, 0.25, 0.75, 0.9, 1)
theta <- purrr::map(d, \(d) c(5, 2, d, 1))

out <- purrr::map(theta, ~ ggplot() +
                    xlim(c(-10, 20)) +
                    ylim(ylims)+
                    stat_function(fun = dQ, n = 10^4,
                                  args = list(theta = .x, Q = qfgld_original, Qprime = qfgld_original_deriv)) +
                    ggtitle(label = substitute(paste(delta," = ",d), env = base::list(d = .x[3])))+
                    theme_minimal()+
                    theme(axis.title = element_blank(),
                          plot.title = element_text(hjust = 0.5,size = 10)))

cowplot::plot_grid(plotlist = out, nrow = 2)

ggsave(filename = "plots/fgld-varying-delta.pdf", width = 6, height = 4)
  
  
k <- c(0, 0.5, 1, 2, 5, 10)
theta <- purrr::map(k, \(k) c(5, 2, 0.5, k))

out <- purrr::map(theta, ~ ggplot() +
                    xlim(c(-10, 40)) +
                    ylim(ylims)+
                    stat_function(fun = dQ, n = 10^4,
                                  args = list(theta = .x, Q = qfgld_original, Qprime = qfgld_original_deriv))+
                    ggtitle(label = substitute(expr = paste(kappa," = ",k),env = base::list(k = .x[4])))+
                    theme_minimal()+
                    theme(axis.title = element_blank(),
                          plot.title = element_text(hjust = 0.5,size = 10)))

cowplot::plot_grid(plotlist = out, nrow = 2)

ggsave(filename = "plots/fgld-varying-kappa.pdf",width = 6,height = 4)




ggplot() +
  xlim(c(0, 1)) +
  stat_function(fun = log, n = 10^4)+
  theme_minimal()+
  theme(axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5,size = 10))


ggplot() +
  xlim(c(0, 1)) +
  stat_function(fun = \(x) -exp(-x), n = 10^4)+
  theme_minimal()+
  theme(axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5,size = 10))


ggplot() +
  xlim(c(-3, 3)) +
  stat_function(fun = \(x) sinh(x*100), n = 10^4)+
  theme_minimal()+
  theme(axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5,size = 10))


ggplot() +
  xlim(c(1e-2, 1-1e-2)) +
  stat_function(fun = \(x) tan(pi/2 + pi*x), n = 10^4)+
  theme_minimal()+
  theme(axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5,size = 10))



ggplot() +
  xlim(c(0, 1)) +
  stat_function(fun = \(x) -log(1-x), n = 10^4)+
  theme_minimal()+
  theme(axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5,size = 10))



theta <- c(0, rnorm(2))
ggplot() +
  xlim(c(-5, 5)) +
  stat_function(fun = dQ, n = 10^4,
                args = list(theta = theta, Q = qquad, Qprime = qquad_deriv))+
  theme_minimal()+
  theme(axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5,size = 10))



ggplot() +
  xlim(c(0, 1)) +
  stat_function(fun = qquad, n = 10^4,
                args = list(theta = theta))+
  theme_minimal()+
  theme(axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5,size = 10))


2*theta[2] > 3*theta[3]


ggplot() +
  xlim(c(0, 1)) +
  stat_function(fun = \(x) theta[1]*x + theta[2]*x^2 + theta[3]*x^3, n = 10^4)+
  theme_minimal()+
  theme(axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5,size = 10))





