# hask-num

Real numbers as Cauchy sequences of rationals.

# Design

Arithmetic operations preserve the Cauchy property with explicit moduli of convergence $N(\varepsilon)$:

| Operation | Modulus |
|---|---|
| $-a$ | $N_a(\varepsilon)$ |
| $\|a\|$ | $N_a(\varepsilon)$ |
| $a + b$ | $\max(N_a(\varepsilon/2),\ N_b(\varepsilon/2))$ |
| $a \times b$ | $\max(N_a(\varepsilon/2M),\ N_b(\varepsilon/2M))$ |

etc.

## Roadmap

- [ ] Division
- [ ] Integer powers
- [ ] `sqrt`, `exp`, `log`
- [ ] Trigonometric functions
