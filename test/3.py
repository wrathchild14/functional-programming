def decorator1(f):
    def wrapper1():
        print("Decorator 1 before call")
        return f()
    return wrapper1

def decorator2(f):
    def wrapper2():
        print("Decorator 2 before call")
        return f()
    return wrapper2

@decorator1
@decorator2
def hello():
    print("Hello, world!")

hello()