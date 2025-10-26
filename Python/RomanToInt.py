def helper(c: str) -> int:
    if c == "I": return 1
    if c == "V": return 5
    if c == "X": return 10
    if c == "L": return 50
    if c == "C": return 100
    if c == "D": return 500
    if c == "M": return 1000
    else: return 0

def romanToInt(s: str) -> int:
    indice = {
        "I" : 1,
        "V" : 5,
        "X" : 10,
        "L" : 50,
        "C" : 100,
        "D" : 500,
        "M" : 1000
    }
    acc = 0
    for i in range(len(s)-1):
       if indice[s[i+1]] > indice[s[i]]: 
          acc -= indice[s[i]]
       else: 
          acc += indice[s[i]]
    acc += indice[s[-1]]
    return acc

def main():
    print(romanToInt("III"))
    print(romanToInt("LVIII"))
    print(romanToInt("MCMXCIV"))
main()