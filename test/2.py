def flatten(func):
    def wrapper():
        def flatten_list(nested_list):
            result = []
            for i in nested_list:
                if isinstance(i, list):
                    result.extend(flatten_list(i))
                else:
                    result.append(i)
            return result

        return flatten_list(func())
    return wrapper

def enkrat(f):
  cache = set()
  def wrapper(*args):
    if args in cache:
      raise ValueError(f"Function {f.__name__} has already been called with these arguments: {args}")
    else:
      cache.add(args)
      return f(*args)
  return wrapper

@enkrat
def add(x, y):
    return x + y

@flatten
def test():
    return [1, ['b', 'c'], [[1], [2]]]

if __name__ == "__main__":
    # print(test()) # [1, 'b', 'c', 1, 2]
    print(add(1, 2)) # 3
    print(add(1, 2)) # ValueError: Function add has already been called with these arguments: (1, 2)
