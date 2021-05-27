
#' get_reward
#' @export
get_reward <- function(.tbl){

  .tbl %>%
    mutate(action = action > 0,
           l_price = lag(price, 1, default = first(price)),
           first_trade = action != lag(action, default = !first(action)),
           trade_index = cumsum(first_trade),
           fees = ifelse(first_trade, 0, 1),
           return = case_when(
             # Sell
             !action ~ 100*((l_price/price * (1 - (0.001*fees))^2) - 1),
             # Buy
             action ~ 100*((price/l_price * (1 - (0.001*fees))^2) - 1)
           ),
           # return = case_when(
           #   # Sell Wrong
           #   !action & return < 0 ~ return,
           #   # Sell Right
           #   !action & return >= 0 ~ return,
           #   # Buy Wrong
           #   action & return < 0 ~ return,
           #   # Buy Right
           #   action & return >= 0 ~ return
           # ),
           reward = cumsum(return)) %>%
    tail(1)
}

#' get_reward_q
#' @export
get_reward_q <- function(.tbl){

  .tbl %>%
    mutate(action = action > 0,
           l_price = lag(price, 1, default = first(price)),
           first_trade = action != lag(action, default = !first(action)),
           last_trade = action != lead(action, default = !first(action)),
           trade_index = cumsum(first_trade)) %>%
    group_by(trade_index) %>%
    mutate(return = case_when(
             !first_trade ~ 0,
             # Sell
             # !action ~ 100*((first(l_price)/last(price) * (1 - (0.001))^2) - 1),
             # Buy
             action ~ 100*((last(price)/first(l_price) * (1 - (0.001))^2) - 1),
             T ~ 0
           )) %>%
    ungroup %>%
    mutate(reward = ifelse(first_trade, cumsum(return), 0)) %>%
    tail(1)
}


#' terminal_fun
#' @export
terminal_fun <- function(){


  end_of_dt <- mem$cur_index >= (nrow(mem$input) - 1)
  if(end_of_dt) return(T)
  if(mem$ep_index > 50) return(T)

  if(length(mem$memory) < 5) return(F)

  return(F)
  # current_sell <- tail(mem$memory, 1) <= 0
  # last_buy <- head(tail(mem$memory, 2), 1) > 0
  #
  # return(current_sell & last_buy)
}



#' get_reward2
#' @export
get_reward2 <- function(actions, cur_step){

  mem$memory[cur_step] <- actions

  if(cur_step > 20){

    last_value <- tibble(action = mem$memory) %>%
      dplyr::mutate(n_step = 1:dplyr::n()) %>%
      dplyr::left_join(dplyr::select(mem$input, -features), by = "n_step") %>%
      #dplyr::bind_rows(dplyr::tibble(action = -1, price = last(mem$input$price), n_step = cur_step + 1)) %>%
      dplyr::mutate(action = ifelse(action > 0, "buy", "sell")) %>%
      dplyr::filter(action != dplyr::lag(action, default = "sell")) %>%
      dplyr::mutate(trade_index = cumsum(action == "buy")) %>%
      dplyr::group_by(trade_index) %>%
      dplyr::filter(trade_index > 0 & n() == 2) %>%
      dplyr::summarise(return = price_diff(price[1], price[2]), n_step = last(n_step)) %>% #, .groups = "drop"
      dplyr::ungroup() %>%
      dplyr::summarise(
        total_return = sum(return, na.rm = T),
        return = last(return),
        n_step = last(n_step)
      ) %>%
      dplyr::mutate_all(~ifelse(is.na(.x), 0, .x))

    if(actions > 0){
      reward <- 0
      total <- last_value$total_return
    } else {
      if(last_value$n_step == cur_step){
        reward <- last_value$return
        total <- last_value$total_return
      } else {
        reward <- 0
        total <- last_value$total_return
      }
    }
  } else {
    reward <- 0
    total <- 0
  }

  if(cur_step %% 100 == 0){ cli::cli_alert_success("{cur_step} - {reward} {total}") }

  #print(reward)

  return(reward)
}
