#' @param data a data frame with at least columns x and y
#' @param buffer neighborhood radius in whatever units x and y are in
make_nei <- function(data, buffer) {
  k_list <- 
    purrr::map2(data$x, data$y, #for every row...
         \(x.i, y.i) { 
           data |> 
             tibble::rownames_to_column("i") |>
             dplyr::mutate(i = as.integer(i)) |> 
             #filter to include points within a range of each point
             dplyr::filter(x > x.i - buffer, 
                    x < x.i + buffer,
                    y > y.i - buffer, 
                    y < y.i + buffer) |> 
             dplyr::pull(i) #and get the indexes
         }
    )
  k <- list_c(k_list) #combine into a single vector
  #index of k that is the end of each neighborhood
  m <-  k_list |> purrr::map_dbl(length) |> cumsum()
  
  # center of each neighborhood for all years
  # so just the indexes of each *unique* pixel
  i_list <- #hmm something like 
    purrr::map2(data$x, data$y, \(x.i, y.i) {
      data |> 
        tibble::rownames_to_column("i") |> 
        dplyr::mutate(i = as.integer(i)) |> 
        dplyr::filter(x == x.i, y == y.i) |>
        dplyr::pull(i)
    })
  i <- list_c(i_list)
  
  mi <- i_list |> purrr::map_dbl(length) |> cumsum()
  
  nei <- list(k = k, m = m, i = i, mi = mi)
  nei
}
