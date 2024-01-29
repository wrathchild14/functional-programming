def Mandelbrot(c, maxstep):
    Zn = 0 + 0j
    for n in range(maxstep):
        if abs(Zn) > 2:
            break
        yield Zn
        Zn = Zn**2 + c

def curry3(func):
    def curried(*args):
        if len(args) < 3:
            print(len(args))
            return lambda *args2: curried(*(args + args2))
        else:
            return func(*args)
    return curried

@curry3
def f(a, b, c):
    return a + b + c

# class Mandelbrot:
#     def __init__(self, c, maxstep):
#         self.c = c
#         self.maxstep = maxstep
#         self.n = 0
#         self.Zn = 0 + 0j

#     def __iter__(self):
#         return self

#     def __next__(self):
#         if self.n < self.maxstep and abs(self.Zn) <= 2:
#             current = self.Zn
#             self.Zn = self.Zn**2 + self.c
#             self.n += 1
#             return current
#         else:
#             raise StopIteration

if __name__ == "__main__":
    for i, Zn in enumerate(Mandelbrot(0.2 + 0.7j, 10)):
        print(f"Zap.št. {i} Rezultat {Zn} Abs. vrednost {abs(Zn)}")
    print(f(1,2,3))
    print(f(1)(2,3))
    print(f(1,2)(3))
    print(f(1)(2)(3))