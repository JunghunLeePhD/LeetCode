def longestCommonPrefix(strs: list[str]) -> str:
    if not strs: return ""
    prefix = strs[0]
    for i in range(len(prefix)):
       for j in range(1, len(strs)):
            if i >= len(strs[j]) or strs[j][i] != prefix[i]:
                return strs[0][:i]
    return prefix


def main ():
    test: list[list[str]] = [["flower", "flow", "flight"],
        ["interstate", "interstellar", "interior"],
        ["dog", "racecar", "car"],
        ["apple", "application", "apply"],
        ["flow", "flower", "flowing"],
        ["hello", "hello", "hello"],
        [],
        ["automation"],
        ["start", "", "starter"],
        ["", "begin", "below"],
        ["ca", "car", "carbon"],
        ["a", "a", "a", "a", "a", "a", "a"],
        ["Apple", "Application", "Apply"],
        ["Apple", "apple", "APPLY"],
        ["100", "1000", "10"],
        ["_internal", "_interrupt", "_interval"]]

    for v in test:
        print(longestCommonPrefix(v))

main()