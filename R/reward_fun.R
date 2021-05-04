#' get_reward
#' @export
get_reward <- function(actions, cur_step){
  mem$memory[cur_step] <- actions

  if(cur_step > 20){
    reward <- tibble(action = mem$memory) %>%
      mutate(n_step = 1:n()) %>%
      bind_rows(tibble(action = -1, n_step = cur_step)) %>%
      left_join(select(mem$input, -features), by = "n_step") %>%
      mutate(action = case_when(
        action < -.5 ~ "sell",
        action > .5 ~ "buy",
        T ~ NA_character_)) %>%
      tidyr::fill(action, .direction = "down") %>%
      filter(action != lag(action, default = "sell")) %>%
      mutate(trade_index = cumsum(action == "buy")) %>%
      group_by(trade_index) %>%
      filter(trade_index > 0 & n() == 2) %>%
      summarise(return = price_diff(price[1], price[2]), .groups = "drop") %>%
      summarise(total_return = sum(return)) %>%
      pull(total_return)
  } else {
    reward <- 0
  }

  if(cur_step %% 100 == 0){ cli::cli_alert_success("{cur_step} - {reward}") }

  return(reward)

}
