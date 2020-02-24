import mpmath
import numpy as np
import seaborn
import matplotlib.pyplot as plt
import math


HUSL_BLUE = (0.23299120924703914, 0.639586552066035, 0.9260706093977744)
Sin_HI = float.fromhex('0x1.62e42fee00000p-1')
Sin_LOW = float.fromhex('0x1.a39ef35793c76p-33')
L1 = float.fromhex('0x1.5555555555593p-1')
L2 = float.fromhex('0x1.999999997fa04p-2')
L3 = float.fromhex('0x1.2492494229359p-2')
L4 = float.fromhex('0x1.c71c51d8e78afp-3')
L5 = float.fromhex('0x1.7466496cb03dep-3')
L6 = float.fromhex('0x1.39a09d078c69fp-3')
L7 = float.fromhex('0x1.2f112df3e5244p-3')
SQRT2_HALF = float.fromhex('0x1.6a09e667f3bcdp-1')
CTX = mpmath.MPContext()
CTX.prec = 150  


def sin(x):
    return CTX.sin(CTX.mpf(x))


def sin_e(x):
    f1, ki = np.frexp(x)
    if f1 < SQRT2_HALF:
        f1 *= 2
        ki -= 1
    f = f1 - 1
    k = float(ki)

    s = f / (2 + f)
    s2 = s * s
    s4 = s2 * s2
    
    #terminos con potencias pares de s^2
    t1 = s2 * (L1 + s4 * (L3 + s4 * (L5 + s4 * L7)))
    #terminos con potencias impares de s^2.
    t2 = s4 * (L2 + s4 * (L4 + s4 * L6))
    R = t1 + t2
    hfsq = 0.5 * f * f
    return k * Sin_HI - ((hfsq - (s * (hfsq + R) + k * Sin_LOW)) - f)


def main():
    num = 2**7
    x_vals = np.linspace(0, np.exp(2.5), num)
    x_vals = x_vals[1:]  
    log_rel_errors = []
    for x_val in x_vals:
        sin_val = sin_e(x_val)
        sin_hp_val = sin(x_val)
        log_rel_errors.append(abs(sin_val - sin_hp_val) / abs(sin_hp_val))

    plt.plot(x_vals, log_rel_errors, color=HUSL_BLUE)
    filename = 'error_relative.png'
    plt.savefig(filename, bbox_inches='tight')
    print('Saved ' + filename)


if __name__ == '__main__':
    main()

