.onLoad <- function(libname, pkgname) {
  # Memoise openFDA HTTP activity
  openFDA <<- memoise::memoise(openFDA)

  # Set default options
  rlang::push_options(openFDA.paging = "ask",
                      openFDA.paging_verbosity = "verbose",
                      openFDA.handle_http_errors = "warn")
}

.onUnload <- function(libname, pkgname) {
  rlang::push_options(openFDA.paging = NULL,
                      openFDA.paging_verbosity = NULL,
                      openFDA.handle_http_errors = NULL)
}

# To allow mocking of interactive() in tests, see:
# https://testthat.r-lib.org/reference/local_mocked_bindings.html
interactive <- NULL

if (FALSE) {
  devtools::load_all()
  letters <- c("a", "h", "s", "t")
  total_n_iterations <- 10
  bp <- purrr::map(
    letters,
    function(letter) {
      iterations <- seq_len(total_n_iterations)
      time <- map_dbl(
        iterations,
        function(iteration) {
          bench::bench_time(
            openFDA(search = glue::glue("openfda.generic_name:{letter}*"),
                    paging = "always",
                    paging_verbosity = "quiet")) |>
            purrr::pluck("real")
        }
      )
      data.frame(time = time, iteration = iterations, letter = letter)
    }
  ) |>
    purrr::list_rbind()

  bp$time <- as.numeric(bp$time)
  max_time <- ceiling(max(bp$time))
  ggplot2::ggplot(bp, ggplot2::aes(x = iteration, y = time, colour = letter)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous(breaks = seq_len(total_n_iterations)) +
    #ggplot2::scale_y_continuous(breaks = seq_len(max_time),
    #                            limits = c(0, max_time + 0.2),
    #                            expand = c(0, 0)) +
    ggplot2::scale_colour_discrete(
      labels = glue::glue("\"openfda.generic_name:{letters}\\*\"")) +
    ggplot2::labs(x = "Iteration", y = "Time (secs)",
                  colour = "`search` string") +
    ggpubr::theme_pubr() +
    ggplot2::theme(legend.position = "inside",
                   legend.position.inside = c(0.8, 0.85),
                   legend.text = ggtext::element_markdown())
}


