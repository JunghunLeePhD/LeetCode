#include <vector>
#include <iostream>

using namespace std;
vector<int> twoSum(vector<int>& nums, int target);


int main(){
    vector<int> mynumbs = {2, 7, 11, 15};
    int mytarget = 9;

    vector<int> result = twoSum(mynumbs, mytarget);

    if (result.size() == 2) {
        cout << "Indices found: " << result[0] << ", " << result[1] << endl;
    } else {
        cout << "No solution found." << endl;
    }

    return 0;
}

vector<int> twoSum(vector<int>& nums, int target){
    for(int i = 0; i < nums.size(); i++){
        for(int j = i+1; j < nums.size(); j++){
            if(nums[i]+nums[j] == target){
                return {i, j};
            }
        }
    }
    return {-1};
}