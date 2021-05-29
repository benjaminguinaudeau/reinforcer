import numpy as np
import pandas as pd 

import gym
from gym import spaces


class gymr(gym.Env):
    metadata = {'render.modes': ['human']}
    def __init__(self, init, step, reset):
       out = r[init]()
       self.state = out["state"]
       self.dim_state = out["dim_state"]
       self.episode_index = 0
       self.overall_index = 0
       self.step_fun = step
       self.reward = 0
       self.reset_fun = reset
       self.action_space = spaces.Discrete(out["action_len"]) 
       self.observation_space = spaces.Box(low = np.inf, high = np.inf, shape = self.dim_state)

    def step(self, actions):
        self.episode_index += 1 
        self.overall_index += 1
        out = r[self.step_fun](state = self.state, actions = actions, episode_index = self.episode_index, overall_index = self.overall_index, prev_reward = self.reward)
        self.state = out["state"]
        if(np.isnan(out["reward"])):
          raise ValueError('Reward cannot be missing')
        self.reward = out["reward"]
        return out["state"], out["reward"], out["terminal"],{}

    def reset(self):
      out = r[self.reset_fun]()
      self.episode_index = 0
      self.reward = 0
      self.state = out["state"]
      return self.state
