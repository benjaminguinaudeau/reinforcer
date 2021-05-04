
#' train_rl
#' @export
train_rl <- function(env_train, type, model_name = NULL, steps = 25000L){

  if(type == "DDPG"){
    n_actions = tail(env_train$action_space$shape, -1)
    param_noise = None
    # action_noise = OrnsteinUhlenbeckActionNoise(mean=np.zeros(n_actions), sigma=float(0.5) * np.ones(n_actions))
  }
  if(type == "GAIL"){
    # model = SAC('MLpPolicy', env_train, verbose=1)
    # generate_expert_traj(model, 'expert_model_gail', n_timesteps=100, n_episodes=10)
    # dataset = ExpertDataset(expert_path='expert_model_gail.npz', traj_limitation=10, verbose=1)
  }

  model <- switch(
    type,
    "A2C" = py$A2C('MlpPolicy', env_train, verbose=0),
    "ACER" = py$ACER('MlpPolicy', env_train, verbose=0),
    "DDPG" = py$DDPG('MlpPolicy', env_train, param_noise = param_noise, action_noise = action_noise),
    "PPO" = py$PPO2('MlpPolicy', env_train, ent_coef = 0.005, nminibatches = 8),
    "GAIL" = py$GAIL('MLpPolicy', env_train, dataset, verbose=1)
  )

  model$learn(total_timesteps = steps)
  if(!is.null(model_name)) model$save(glue::glue("{TRAINED_MODEL_DIR}/{model_name}"))

  return(model)

}

#' retrain_rl
#' @export
retrain_rl <- function(model, steps){
  model$learn(total_timesteps = steps)
  return(model)
}
