#include <string> 
#include <map>
#include <iostream>

using namespace std;
int romanToInt(string s);
int indice(char c);

int main(){
    cout << romanToInt("III") << endl;
    cout << romanToInt("LVIII") << endl;
    cout << romanToInt("MCMXCIV") << endl;
    return 1;
}

int romanToInt(string s){
    if (s.empty()) {return 0;}
    int acc = 0;
    int prev_val = 0;
    for(int i = s.length() - 1; i >= 0; --i){
        int current_val = indice(s[i]);
        prev_val > current_val ? acc -= current_val : acc += current_val;
        prev_val = current_val;
    }
    return acc;
}

int indice(char c){
    switch (c) {
        case 'I': return 1;
        case 'V': return 5;
        case 'X': return 10;
        case 'L': return 50;
        case 'C': return 100;
        case 'D': return 500;
        case 'M': return 1000;
        default:  return 0;
    }
}