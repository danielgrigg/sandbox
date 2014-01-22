import random
import matplotlib
import matplotlib.pyplot as plt

N = 5000
mu = 100.

def sample(distribution, N=N, mu=mu):
  "Samples from distribution N times"
  return normalize([distribution() for _ in range(N)], mu * N)

def constant(mu=mu):  return mu
def uniform(mu=mu, width=mu): return random.uniform(mu-width/2, mu+width/2)
def gauss(mu=mu, sigma=mu/3): return random.gauss(mu, sigma)
def beta(alpha=2, beta=3): return random.betavariate(alpha, beta)
def pareto(alpha=4): return random.paretovariate(alpha)

def normalize(numbers, total):
  "Scale numbers so they add up to total"
  factor = total / float(sum(numbers))
  return [x * factor for x in numbers]

def random_split(X, Y):
  "Take all money in the pot and divide it randomly between X and Y."
  pot = X + Y
  m = random.uniform(0, pot)
  return m, pot - m

def winner_take_most(X, Y, most=3/4.):
  "Give most of the money in the pot to one of the parties"
  pot = X + Y
  m = random.choice((most * pot, (1 - most) * pot))
  return m, pot - m

def winner_take_all(X, Y):
  "Give all to one of the actors"
  return winner_take_most(X, Y, 1.0)

def redistribute(X, Y):
  return winner_take_most(X, Y, 0.55)

def split_half_min(X, Y):
  """poorer actor only wants to risk half his wealth;
  the other actor matches this; then randomly split the pot"""
  pot = min(X, Y)
  m = random.uniform(0, pot)
  return X - pot/2. + m, Y + pot/2. - m

