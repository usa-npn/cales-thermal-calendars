check_k <- function(gam) {
  name <- deparse(substitute(gam))
  safe_k_check <- purrr::possibly(mgcv::k.check, tibble())
  safe_k_check(gam) |> 
    as_tibble(rownames = "term") |> 
    mutate(gam = name, .before = 1) |> 
    separate(gam, into = c("trash", "res_m", "k")) |> 
    select(-trash)
}
# check_k(gam_50000_100)
