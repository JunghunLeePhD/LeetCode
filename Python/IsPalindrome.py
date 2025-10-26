def main():
    for i in range(1,1000000):
        if isPalindrome(i):
            print(i)

def isPalindrome(x: int) -> bool:
    if x < 0:
        return False
    xString: str = f"{x}"
    return xString == xString[::-1]

if __name__ == "__main__":  
    main()