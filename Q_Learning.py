##########################################################################

# Copyright (c) 2017 Nandini Khanwalkar 
# nandini2@pdx.edu

##########################################################################

import os
import numpy as np
import matplotlib.pyplot as plt

reward = [-5, -1, 10]
actions = [0, 1, 2, 3, 4]		# actions = ['up', 'down', 'right', 'left', 'pick']
dry_spell = 0

def generate_grid(style):
	grid = np.zeros((12,10))	# wall = 0, empty = 1, can = 2
	if style=='field':																				# If grid style is field 
		grid[np.arange(1,11), :] = np.random.choice([1, 2], (10, 10), replace=True, p=[0.5, 0.5])	# Randomly spread cans across 10 x 10 grid with 0.5 probability
	elif style=='maze':																						# Else if grid style is maze
		grid[np.arange(1,11), :] = np.random.choice([0, 1, 2], (10, 10), replace=True, p=[0.2, 0.4, 0.4])	# Randomly spread cans and walls across 10 x 10 grid with probabilities 0.4 and 0.2 respectively.
	grid = np.concatenate((np.zeros((12, 1)), grid, np.zeros((12, 1))), axis=1)
	loc_R = tuple(list((np.random.choice(np.arange(1, 11), 2))))	# Randomly select a starting location for Robby.
	return grid, loc_R	

def observe_state(loc_R, grid):		# state is observed as [NORTH, SOUTH, EAST, WEST, HERE]
	base3 = np.array([81, 27, 9, 3, 1])
	x, y = loc_R
	sensor_data = [grid[x-1,y], grid[x+1,y], grid[x,y+1], grid[x,y-1], grid[x,y]]	# Observe the contents of visible locations
	state = np.sum(base3*sensor_data)	# Compute state based on sensor data
	return state

def choose_action(q_matrix, state, epsilon):
	best_action = np.random.choice(np.nonzero(q_matrix[int(state),:] == np.amax(q_matrix[int(state),:]))[0], 1)		# Find the action with max Q value, if more than one then choose any one at random
	action = np.random.choice([best_action, np.random.choice(5, 1)[0]], 1, p=[(1-epsilon), epsilon])	# Select action; selecting a random action has probability epsilon and selecting best action has probability (1-epsilon) 
	return action

def perform_action(state, action, gamma, eta, step_cost, relocate, grid, loc_R, q_matrix, mode):
	if action==4:									# If action is 'pick'
		r = reward[int(grid[loc_R])] + step_cost	# Assign reward based on value for 'HERE'
		grid[loc_R] = 1								# Assign value of 'HERE' as empty
		if relocate==True and r==-1:
			global dry_spell
			dry_spell += 1
			if dry_spell>=20:
				loc_R = tuple(list((np.random.choice(np.arange(1, 11), 2))))
				dry_spell = 0 
		new_state = observe_state(loc_R, grid)		# Observe state to compute new state
		if mode=='train':							# Update Q value if training
			q_matrix[int(state), action[0]] += eta*(r + gamma*(np.amax(q_matrix[int(new_state),:])) - q_matrix[int(state), action[0]])
		state = new_state							# Update current state as new state, location remains unchanged
	else:											# Else if action is move, compute new location
		if action==0:
			new_loc = tuple(list(np.asarray(loc_R)+np.array([-1, 0])))
		elif action==1:
			new_loc = tuple(list(np.asarray(loc_R)+np.array([1, 0])))
		elif action==2:
			new_loc = tuple(list(np.asarray(loc_R)+np.array([0, 1])))
		elif action==3:
			new_loc = tuple(list(np.asarray(loc_R)+np.array([0, -1])))

		if grid[new_loc]==0:								# If Robby crashes into wall
			r = reward[int(grid[new_loc])] + step_cost		# Assign negative reward 
			if mode=='train':								# Update Q value if training, location & state remains unchanged
				q_matrix[int(state), action[0]] += eta*(r + gamma*(np.amax(q_matrix[int(state),:])) - q_matrix[int(state), action[0]])
		else:											# Else if no crash occurs
			r = step_cost								# Assign reward
			loc_R = new_loc								# Update current lcation as new location
			new_state = observe_state(loc_R, grid)		# Observe state to compute new state
			if mode=='train':							# Update Q values if training
				q_matrix[int(state), action[0]] += eta*(r + gamma*(np.amax(q_matrix[int(new_state),:])) - q_matrix[int(state), action[0]])
			state = new_state							# Update current state as new state
	return state, grid, loc_R, q_matrix, r

def plot_rewards(episode_reward_list, num_episodes, exp):
	plt.figure(figsize=(12, 7.5), dpi=100)
	plt.plot(num_episodes, episode_reward_list, color='blue', label='Sum of Rewards per Episode' ,lw=2)
	plt.xlabel('\nEpisodes\n', size=16)
	plt.ylabel('\nSum of Rewards\n', size=16)
	plt.title('Q-Learning: Training Robby-The Robot\n', size=18)
	plt.legend(loc='lower right')
	plt.savefig(exp)

def Q_Learning(q_matrix, mode, exp='Exp', gamma=0.9, eta=0.2, l_mode='const', step_cost=0, relocate=False, epsilon=1, e_mode='var', style='field'):
	episode_reward_list = []
	for episode in range(5000):									# For 5000 episodes
		if episode%50==0 and episode>0:							# Update parameters if condition holds
			if epsilon>0.1 and e_mode=='var':
				epsilon -= 0.01
			if eta>0.1 and l_mode=='decay':
				eta -= 0.04
		grid, loc_R = generate_grid(style)						# Generate a new grid
		episode_reward = 0										# Initialize episode reward to 0
		for step in range(200):									# For 200 steps in the episode
			state = observe_state(loc_R, grid)					# Observe current state
			action = choose_action(q_matrix, state, epsilon)	# Choose an action and perform it
			state, grid, loc_R, q_matrix, r = perform_action(state, action, gamma, eta, step_cost, relocate, grid, loc_R, q_matrix, 'train')
			episode_reward += r 								# Update episode reward
		if mode=='test' or (mode=='train' and (episode%100==0 or episode==4999)):
			episode_reward_list.append(episode_reward)			# Update episode reward list 
	if mode=='train':
		num_episodes = np.concatenate((np.arange(0, 5000, 100), [4999]), axis=0)
		plot_rewards(episode_reward_list, num_episodes, exp)	# Plot rewards per episode graph if training
	elif mode=='test':
		print('\tTest Average =', np.mean(episode_reward_list), '\tTest Standard Deviation =', np.std(episode_reward_list)) 	# Compute mean reward sum and standard deviation if testing
	return q_matrix

# Experiment 1
print('\n  Experiment 1:')
q_matrix = np.zeros((3**5, 5))
q_matrix = Q_Learning(q_matrix, mode='train', exp='Exp_1.png')
Q_Learning(q_matrix, mode='test', epsilon=0.1, e_mode='const')

# Experiment 2
print('\n  Experiment 2:')
for eta in [0.8, 0.6, 0.4, 0.2]:
	q_matrix = np.zeros((3**5, 5))
	q_matrix = Q_Learning(q_matrix, mode='train', exp='Exp_2_LR-'+str(eta)+'.png', eta=eta)
	Q_Learning(q_matrix, mode='test', eta=eta, epsilon=0.1, e_mode='const')

# Experiment 3
print('\n  Experiment 3:')
for epsilon in [0.5, 0.4, 0.3, 0.2, 0.1]:
	q_matrix = np.zeros((3**5, 5))
	q_matrix = Q_Learning(q_matrix, mode='train', exp='Exp_3_epsilon-'+str(epsilon)+'.png', epsilon=epsilon, e_mode='const')
	Q_Learning(q_matrix, mode='test', epsilon=epsilon, e_mode='const')

# Experiment 4
print('\n  Experiment 4:')
q_matrix = np.zeros((3**5, 5))
q_matrix = Q_Learning(q_matrix, mode='train', exp='Exp_4.png', step_cost=-0.5)
Q_Learning(q_matrix, mode='test', step_cost=-0.5, epsilon=0.1, e_mode='const')

# Experiment 5
print('\n  Experiment 5:')
q_matrix = np.zeros((3**5, 5))
q_matrix = Q_Learning(q_matrix, mode='train', exp='Exp_5-decaying_LR.png', eta=1, l_mode='decay')
Q_Learning(q_matrix, mode='test', epsilon=0.1, e_mode='const')

q_matrix = np.zeros((3**5, 5))
q_matrix = Q_Learning(q_matrix, mode='train', exp='Exp_5-Maze.png', style='maze')
Q_Learning(q_matrix, mode='test', epsilon=0.1, e_mode='const', style='maze')

q_matrix = np.zeros((3**5, 5))
q_matrix = Q_Learning(q_matrix, mode='train', exp='Exp_5-Maze_with_decaying_LR.png', eta=1, l_mode='decay', style='maze')
Q_Learning(q_matrix, mode='test', epsilon=0.1, e_mode='const', style='maze')

# Experiment 6
q_matrix = np.zeros((3**5, 5))
q_matrix = Q_Learning(q_matrix, mode='train', eta=0.6, relocate=True, exp='Exp.png', gamma=0.5)
Q_Learning(q_matrix, mode='test', eta=0.6, relocate=True, epsilon=0.1, e_mode='const', gamma=0.5)