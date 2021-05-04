
from stable_baselines import GAIL, SAC
from stable_baselines import ACER
from stable_baselines import PPO2
from stable_baselines import A2C
from stable_baselines import DDPG
from stable_baselines import TD3
from stable_baselines.common.vec_env import DummyVecEnv


import numpy as np
import pandas as pd 

import gym
from gym import spaces

def dummy_gymr(init, step, reset, action_len):
  return DummyVecEnv([lambda: gymr(init, step, reset, action_len)])

class gymr(gym.Env):
    metadata = {'render.modes': ['human']}
    def __init__(self, init, step, reset, action_len):
       out = r[init](action_len = action_len)
       self.dim_state = out["dim_state"]
       self.state = out["state"]
       self.row = 0
       self.step_fun = step
       self.reset_fun = reset
       self.action_space = spaces.Box(low = out["low"], high = out["high"], shape = (out["action_len"], )) 
       self.observation_space = spaces.Box(low = 0, high = np.inf, shape = self.state.shape)

    def step(self, actions):
        self.row += 1
        out = r[self.step_fun](state = self.state, actions = actions, cur_step = self.row)
        # print(out)
        return out["state"], out["reward"], out["terminal"],{}

    def reset(self):
      out = r[self.reset_fun]()
      self.row = 0
      self.state = out["state"]
      return self.state
