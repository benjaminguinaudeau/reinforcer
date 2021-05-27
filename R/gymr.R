#' gymr_step
#' @export
gymr_step <- function(actions){
  # glimpse(actions)


  mem$dt_index <- mem$dt_index + 1
  mem$ep_index <- mem$ep_index + 1
  mem$cur_index <- mem$cur_index + 1
  mem$memory[mem$ep_index] <- actions

  returns <- get_ep_dt() %>%
    mem$reward_fun() %>%
    mutate(dt_i = mem$dt_index,
           ep_i = mem$ep_index,
           cur_i = mem$cur_index) %>%
    append_overall_memory

  if(exists("verbose", envir = .GlobalEnv)){
    msg <- case_when(
      actions > 0 & returns$reward > 0 ~ crayon::green("-"),
      actions <= 0 & returns$reward > 0 ~ crayon::green("_"),
      actions > 0 & returns$reward < 0 ~ crayon::red("~"),
      actions <= 0 & returns$reward < 0 ~ crayon::red(".")
    )
    if(returns$reward != 0) cat(msg)
  }

  if(exists("verbose", envir = .GlobalEnv)) .GlobalEnv$saf_turns <- returns

  message_agent()

  terminal <- mem$terminal_fun()

  out <- list(
    state = get_next_state(terminal),
    reward = returns$reward,
    terminal = terminal
  )
  # if(exists("verbose", envir = .GlobalEnv)) cat(".")
  if(exists("verbose", envir = .GlobalEnv)) .GlobalEnv$saf_state <- out

  return(out)
}

#' message_agent
#' @export
message_agent <- function(){
  if(mem$dt_index %% 1000 == 0){
    msg_dt <- mem$overall_memory %>%
      tail(1000) %>%
      summarise(
        prop_buy = 100*mean(action),
        sum = sum(100*return),
        sum_buy = sum(100*return[action]),
        sum_sell = sum(100*return[!action]),
        n_ep = last(cumsum(ep_i == 1)),
        step = max(dt_i)
      ) %>%
      mutate_all(round)

    msg <- glue::glue("{msg_dt$step}\t T {msg_dt$sum}\t({msg_dt$sum_buy}|{msg_dt$sum_sell}) \tP {msg_dt$prop_buy}\t N {msg_dt$n_ep}")

    if(sum(msg_dt$sum) > 0) cli::cli_alert_success(msg) else cli::cli_alert_danger(msg)

  }
}

#' append_overall_memory
#' @export
append_overall_memory <- function(.tbl){
  n_rows <- nrow(mem$overall_memory)
  if(n_rows == 0){
    mem$overall_memory <- .tbl
  } else {
    mem$overall_memory[n_rows + 1, ] <- .tbl
  }
  return(.tbl)
}

#' get_next_state
#' @export
get_ep_dt <- function(){
  tmp <- tibble(action = mem$memory) %>%
    mutate(n_step = (1 + mem$cur_index - mem$ep_index):mem$cur_index)
  # Take lead price because execution happens at the end of a minute
  tmp$price <- mem$input$price[tmp$n_step + 1]
  return(tmp)
}

#' get_next_state
#' @export
get_next_state <- function(terminal){
  if(terminal) return(as_tibble(array(rep(0, prod(mem$dim_state)), dim = mem$dim_state)))
  state <- mem$input$features[[mem$cur_index + 1]]
  if(mem$add_action) state <- cbind(state, tibble(act = tail(mem$memory, 1)))
  if(mem$add_reward) state <- cbind(state, tibble(rew = tail(mem$overall_memory$reward, 1)))
  return(state)
}

#' gymr_reset
#' @export
gymr_reset <- function(){
  if(exists("verbose", envir = .GlobalEnv) & rlang::has_name(mem$overall_memory, "reward")) cat(tail(mem$overall_memory$reward, 1))
  if(exists("verbose", envir = .GlobalEnv)) cat("\nR")

  mem$memory <- c(0)
  mem$ep_index <- 0
  mem$cur_index <- sample(1:nrow(mem$input), 1)
  mem$dt_index <- if(mem$dt_index >= nrow(mem$input)) 0 else mem$dt_index
  if(mem$dt_index == 0) mem$epoch <- mem$epoch + 1

  out <- list(state = as_tibble(array(rep(0, prod(mem$dim_state)), dim = mem$dim_state)))

  if(exists("verbose", envir = .GlobalEnv)) cat(".")
  if(exists("verbose", envir = .GlobalEnv)) .GlobalEnv$saf_reset <- out

  return(out)
}

#' gymr_init
#' @export
gymr_init <- function(){

  if(exists("verbose", envir = .GlobalEnv)) cat("init")
  state <- as_tibble(array(rep(0, prod(mem$dim_state)), dim = mem$dim_state))

  return(list(state = state, action_len = mem$action_len, dim_state = mem$dim_state))
}

#' init_mem
#' @export
init_mem <- function(input, reward_fun, terminal_fun, add_action = F, add_reward = F, action_len = 1){
  # reticulate::source_python(system.file("py", "gymr.py", package = "reinforcer"))
  load_py_fun()

  mem <- rlang::new_environment()

  # mem$meta_memory <- tibble(episode = 0, memory = NA, rew = NA)
  # mem$reward <- c()
  mem$memory <- c()
  mem$overall_memory <- tibble::tibble()

  mem$add_action <- add_action
  mem$add_reward <- add_reward
  mem$action_len <- as.integer(action_len)
  mem$terminal_fun <- terminal_fun
  mem$reward_fun <- reward_fun

  mem$input <- input
  mem$dim_state <- dim(input$features[[1]])
  if(add_action) mem$dim_state[2] <- as.integer(mem$dim_state[2] + mem$action_len)
  if(add_reward) mem$dim_state[2] <- as.integer(mem$dim_state[2] + 1)

  mem$dt_index <- 0
  mem$ep_index <- 0
  mem$cur_index <- sample(1:nrow(mem$input), 1)
  mem$dt_round <- 1

  return(mem)
}
