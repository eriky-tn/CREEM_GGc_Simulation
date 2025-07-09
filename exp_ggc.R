################################################################################
# This script simulates G/G/c queues by discrete events
#
# Programmed by:
# Rafael M. Andrade
# Gustavo M. Andrade
# Eriky S. Gomes
# Frederico R. B. Cruz
# 
# Universidade Federal de Minas Gerais
# Pontificia Universidade Catolica de Minas Gerais
#
# E-mail: 
# gustavo.mariz.andrade@gmail.com,
# eriky-tn@ufmg.br
# fcruz@est.ufmg.br
#
# © 2025 Andrade et al.
# v.2025-07-09
################################################################################

rm(list = ls())

library('queuecomputer')
library('microbenchmark')

# remove all variables, except functions
rem_var <- function() {
  all_objects <- ls(envir = .GlobalEnv)
  functions <- all_objects[sapply(all_objects,
                                  function(x) is.function(get(x, envir = .GlobalEnv)))]
  rm(list = setdiff(all_objects, functions), envir = .GlobalEnv)
}

################################################################################
# random g-g-c queue generator
################################################################################


#' random g-g-c queue generator
#' @param size_max queue capacity
#' @param nserv number of servers
#' @param farr arrival function
#' @param fdep departure function
#' @param narr_sim number of arrivals simulated
#' @return a simulated queue data-frame
rggc <- function(size_max, nserv, farr, fdep, narr_sim){
  # todo: implement finite queues
  
  # initialize variables
  arr <- cumsum(sapply(1:narr_sim, farr))
  dep <- sapply(1:narr_sim, fdep)
  serv_free <- rep(0, nserv)
  arr_start_serv <- rep(0, narr_sim)
  arr_end_serv <- rep(0, narr_sim)
  
  # simulate end of service times
  for(i in 1:narr_sim){
    k_next <- which.min(serv_free)
    arr_start_serv[i] <- max(arr[i], serv_free[k_next])
    arr_end_serv[i] <- arr_start_serv[i] + dep[i]
    serv_free[k_next] <- arr_end_serv[i] 
  }
  
  # update queue
  queue_tab <- data.frame(
    time = c(arr, arr_end_serv),
    type = c(rep('a', length(arr)), rep('d', length(arr_end_serv)))
  )
  queue_tab <- queue_tab[order(queue_tab$time), ]
  queue_tab$size <- cumsum(ifelse(queue_tab$type == 'a', 1, -1))
  
  return(queue_tab)
}


mmc_Ls_rho <- function(rho, c) {
  a <- c * rho
  
  P0 <- 1 / (
    sum(sapply(0:(c-1), function(k) a^k / factorial(k))) +
      (a^c) / (factorial(c) * (1 - rho))
  )
  
  Lq <- P0 * (a^c) / factorial(c) * rho / (1 - rho)^2
  Ls <- Lq + a
  
  return(Ls)
}

nserv <- 15
rho <- 0.85
repet <- 100
erro <- numeric(repet)
for(i in 1:repet){
  sim_queue <- rggc(
    size_max = 1000,
    nserv = nserv,
    farr = function(x) rexp(n = 1, rate = 1),
    fdep = function(x) rexp(n = 1, rate = 1/(nserv * rho)),
    narr_sim = 10e3
  )
  sim_queue$gap <- c(diff(sim_queue$time), 0)
  Ls_teo <- mmc_Ls_rho(rho, c = nserv)
  Ls_est <- sum(sim_queue$size * sim_queue$gap) / sum(sim_queue$gap)
  erro[i] <- Ls_est - Ls_teo
}
mean(erro)

#rem_var()
