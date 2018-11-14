import numpy as np
from sklearn.metrics import mean_squared_error
import sys



A = [ [3, 5, 1, 0]
    , [3, 4, 3, 1]
    , [0, 1, 2, 5]
    , [2, 4, 0, 5]]

A_ = [ [1, 1, 1, 0]
     , [1, 1, 1, 1]
     , [0, 1, 1, 1]
     , [1, 1, 0, 1]]
Q = np.ones((4,10))
U = np.ones((6,4))

group_answers= [ [3, 4, 3, 1, 1, 5, 5, 2, 3, 3] # Tessa
               , [3, 2, 3, 1, 4, 5, 5, 3, 2, 1] # Jonatan
               , [1, 1, 3, 1, 3, 1, 4, 3, 2, 2] # Jacob
               , [4, 1, 3, 1, 2, 4, 5, 3, 1, 2] # Anton
               , [2, 4, 2, 1, 2, 5, 5, 2, 1, 1] # Henrik
               , [3, 4, 2, 1, 2, 5, 5, 4, 1, 1] # Kasper
               ]

test_answers= [  [0, 4, 3, 1, 1, 5, 5, 0, 3, 3] # Tessa
               , [3, 2, 3, 0, 4, 5, 5, 3, 2, 1] # Jonatan
               , [1, 1, 3, 1, 3, 1, 4, 3, 2, 2] # Jacob
               , [4, 1, 3, 1, 2, 0, 5, 0, 1, 2] # Anton
               , [2, 4, 0, 1, 2, 5, 0, 2, 1, 1] # Henrik
               , [3, 4, 2, 1, 2, 5, 5, 4, 0, 1] # Kasper
               ]

toOneOrZero = lambda x: 0 if x == 0 else 1

def applyToMatrix(f, M):
  M_ = np.copy(M)
  for x in np.nditer(M_, op_flags=['readwrite']):
    x[...] = f(x)
  return M_

alpha = 0.001
# U = 6 * 4
# Q = 4 * 10
def gradient_descent(A, A_, U, Q):
  guess = np.matmul(U, Q) # 6 * 10
  guess_ = np.multiply(guess, A_) # 6 * 10
  mse = mean_squared_error(A, guess_)
  print(mse)
  if mse >= 0.59:
    E = guess_ - A # 6 * 10
    #print(f"E:{E}")
    U_ = U - np.transpose(np.matmul(Q, np.transpose(E))) * alpha
    #print(f"U_: {U_}")

    Q_ = Q - np.matmul(np.transpose(U), E) * alpha
    #print(f"Q_: {Q_}")
    return gradient_descent(A, A_, U_, Q_)
  else:
    return U, Q

sys.setrecursionlimit(1000000)
test_answers_ = applyToMatrix(toOneOrZero, test_answers)
U1, Q1 = gradient_descent(test_answers, test_answers_, U, Q)
result = np.matmul(U1, Q1)
print(result - group_answers)
print(mean_squared_error(result, group_answers))





  

#print(f"sq error: {mean_squared_error(A, UQ)}")