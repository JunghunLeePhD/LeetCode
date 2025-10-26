#include <iostream>
#include<vector>

using namespace std;

string longestCommonPrefix(vector<string> strs);

int main(){
    vector<vector<string>> test = 
        {
            {"flower", "flow", "flight"},
            {"interstate", "interstellar", "interior"},
            {"dog", "racecar", "car"},
            {"apple", "application", "apply"},
            {"flow", "flower", "flowing"},
            {"hello", "hello", "hello"},
            {},
            {"automation"},
            {"start", "", "starter"},
            {"", "begin", "below"},
            {"ca", "car", "carbon"},
            {"a", "a", "a", "a", "a", "a", "a"},
            {"Apple", "Application", "Apply"},
            {"Apple", "apple", "APPLY"},
            {"100", "1000", "10"},
            {"_internal", "_interrupt", "_interval"},
        };
    for(int i = 0; i < test.size(); ++i){
        cout << longestCommonPrefix(test[i]) << endl;
    }
    return 1;
}

string longestCommonPrefix(vector<string> strs){
    if(strs.empty()){
        return "";
    }
    string prefix = strs[0];
    for(int i = 0; i < prefix.size(); ++i){
        for(int j = 0; j < strs.size(); ++j){
            if (i >= strs[j].size() || strs[j][i] != prefix[i]){
                return strs[0].substr(0,i);
            }
        }
    }
    return prefix;
}