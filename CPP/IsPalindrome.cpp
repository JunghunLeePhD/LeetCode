// g++ -O2 IsPalindrome.cpp -o IsPalindrome
#include <string> 
#include <iostream>

using namespace std;
bool isPalindrome(int x);

int main(){
    for(int i = 1; i<=1000000; ++i){
        if(isPalindrome(i)){
            cout << i << endl;
        }
    }
}

// This is the fast mathematical version
bool isPalindrome(int x) {
    if (x < 0 || (x % 10 == 0 && x != 0)) {
        return false;
    }
    int reversed_half = 0;
    while (x > reversed_half) {
        reversed_half = reversed_half * 10 + x % 10;
        x /= 10;
    }
    return x == reversed_half || x == reversed_half / 10;
}