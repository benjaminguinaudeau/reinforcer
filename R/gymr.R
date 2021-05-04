#' gymr_step
#' @export
gymr_step <- function(state, actions, cur_step){

  terminal <- cur_step == nrow(mem$input)
  state <- ifelse(terminal, c(0),  mem$input$features[[cur_step + 1]])
  mem$last_reward <- reward <- mem$reward_fun(actions, cur_step)

  return(list(state = state, reward = reward, terminal = terminal))
}

#' gymr_reset
#' @export
gymr_reset <- function(){
  cli::cli_alert("Reseting")
  mem$meta_memory <- bind_rows(mem$meta_memory, tibble::tibble(episode = max(mem$meta_memory$episode) + 1, memory = list(mem$memory)))
  mem$memory <- c()
  return(list(state = array(rep(0, sum(mem$dim_state) - length(mem$dim_state) + 1), dim = mem$dim_state)))
}

#' gymr_init
#' @export
gymr_init <- function(low = -1, high = 1, action_len = 1){
  dim_state <- dim(prices$features[[1]])
  state <- array(rep(0, sum(dim_state) - length(dim_state) + 1), dim = dim_state)
  low <- low
  high <- high
  action_len <- as.integer(action_len)
  return(list(state = state, low = low, high = high, action_len = action_len, dim_state = dim_state))
}

#' init_mem
#' @export
init_mem <- function(input, reward_fun){
  # reticulate::source_python(system.file("py", "gymr.py", package = "reinforcer"))
  load_py_fun()

  mem <- rlang::new_environment()
  mem$memory <- c()
  mem$meta_memory <- tibble(episode = 0, memory = NA)
  mem$input <- input
  mem$last_reward <- 1
  mem$reward_fun <- reward_fun
  mem$dim_state <- dim(input$features[[1]])

  return(mem)
}
