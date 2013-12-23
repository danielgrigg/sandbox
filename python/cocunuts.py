import math

def Cn_up(Cn1):
    return (5.0 * Cn1 / 4.0) + 1

def isInt(f):
    return math.floor(f) == f
   
for k in range(1, 10000):
    C5 = 5.0 * k + 1.0
    C4 = Cn_up(C5)
    C3 = Cn_up(C4)
    C2 = Cn_up(C3)
    C1 = Cn_up(C2)
    C0 = Cn_up(C1)
    x = (C5,C4,C3,C2,C1,C0)

    if isInt(C5) and isInt(C4) and isInt(C3) and isInt(C2) and isInt(C1) and isInt(C0):
        print(x)

    
