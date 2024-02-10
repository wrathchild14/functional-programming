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

@flatten
def test():
    return [1, ['b', 'c'], [[1], [2]]]

if __name__ == "__main__":
    print(test()) # [1, 'b', 'c', 1, 2]
