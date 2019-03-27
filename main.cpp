#include <iostream>
#include <bits/stdc++.h>
#include <stdbool.h>
#include <math.h>

using namespace std;

struct minterm {pair<short,vector<short> > values ; bool picked = false; string express; short cost;};
struct minValue {short scalar; vector <short> cover; string canon;};
bool equalVectors(vector <short> v1,vector <short> v2){
    if(v1.size()==v2.size()){
        for(int i=0;i<v1.size();i++){
            if(v1[i]!=v2[i])
                return false;
        }
        return true;
    }
    else return false;
}
short countOnes(short m){
    multiset <short> ones;
    while(m){
        ones.insert(m%2);
        m/=2;
    }
    return ones.count(1);
}
vector < vector <minterm> > tabularPrep(short m, vector < vector <minterm> > v){
    short ones = countOnes(m);
    while(ones>=v.size())
        v.push_back(vector<minterm>());
    minterm temp;
    temp.values.first=m;
    v[ones].push_back(temp);
    return v;
}
bool isPowerOfTwo(int n){
   return (ceil(log2(n)) == floor(log2(n)));
}
vector < vector <minterm> > falsify(vector < vector <minterm> > v){
    for(int i=0;i<v.size();i++){
        for(int j=0;j<v[i].size();j++){
            v[i][j].picked=false;
        }
    }
    return v;
}
bool repetition (vector <minterm> v, minterm m){
    for(int i=0;i<v.size();i++){
        if((v[i].values.first==m.values.first)&&(equalVectors(v[i].values.second,m.values.second)))
            return true;
    }
    return false;
}
vector <minterm> tabularMethod(vector < vector <minterm> > v1,vector < vector <minterm> > v2,vector <minterm> v3){
    v2.clear();
    v1 = falsify(v1);
    for(int i=0;i<v1.size()-1;i++){
            v2.push_back(vector<minterm>());
        for(int j=0;j<v1[i].size();j++){
            for(int k=0;k<v1[i+1].size();k++){
                short diff = v1[i+1][k].values.first - v1[i][j].values.first;
                if(equalVectors(v1[i][j].values.second,v1[i+1][k].values.second)&&diff>0&&isPowerOfTwo(diff)){
                    v1[i+1][k].picked=v1[i][j].picked=true;
                    v1[i][j].values.second.push_back(diff);
                    v2[i].push_back(v1[i][j]);
                    v1[i][j].values.second.pop_back();
                }
            }
        }
    }
    for(int i=0;i<v2.size();i++){
        for(int j=0;j<v2[i].size();j++){
            sort(v2[i][j].values.second.begin(),v2[i][j].values.second.end());
        }
    }
    bool flag = false;
    for(int i=0;i<v1.size();i++){
        for(int j=0;j<v1[i].size();j++){
            if(v1[i][j].picked==true){
                flag = true;
            }
            else{
                if(!repetition(v3,v1[i][j]))
                    v3.push_back(v1[i][j]);
            }
        }
    }
    /*for(int i=0;i<v1.size();i++){
        for(int j=0;j<v1[i].size();j++){
            cout << v1[i][j].values.first << " ";
            for(int k=0;k<v1[i][j].values.second.size();k++)
                cout << v1[i][j].values.second[k] << " ";
            cout << endl;
        }
    }
    for(int i=0;i<v2.size();i++){
        for(int j=0;j<v2[i].size();j++){
            cout << v2[i][j].values.first << " ";
            for(int k=0;k<v2[i][j].values.second.size();k++)
                cout << v2[i][j].values.second[k] << " ";
            cout << endl;
        }
    }
    for(int i=0;i<v3.size();i++){
            cout << v3[i].values.first << " ";
            for(int k=0;k<v3[i].values.second.size();k++)
                cout << v3[i].values.second[k] << " ";
            cout << endl;
    }*/
    if (!flag) return v3;
    return tabularMethod(v2,v1,v3);
}
vector <minterm> minToExp(string s,vector <minterm> v,short m){
    for(int i=0;i<v.size();i++){
        string eliminate;
        for(int j=0;j<v[i].values.second.size();j++){
            eliminate.push_back(s[s.size()-floor(log2(v[i].values.second[j]))-1]);
        }
        short temp = v[i].values.first;
        for(int j=m-1;j>=0;j--){
            if(eliminate.find(s[j])==-1){
                string temps;
                temps.push_back(s[j]);
                temps+=(temp%2?"":"'");
                v[i].express=temps+v[i].express;
            }
            temp/=2;
        }
        v[i].cost=v[i].express.size()+(v[i].express.size()==1?0:1);
    }
    return v;
}
vector <minValue> minCanon(vector <minValue> v, string s, short m){
    for(int i=0;i<v.size();i++){
        short temp=v[i].scalar;
        for(int j=m-1;j>=0;j--){
            string temps;
            temps.push_back(s[j]);
            temps+=(temp%2?"":"'");
            v[i].canon=temps+v[i].canon;
            temp/=2;
        }
    }
    return v;
}
vector <minValue> PiTablePrep(vector <minValue> v, vector <minterm> pi){
    for(int i=0;i<v.size();i++){
        for(int j=0;j<pi.size();j++){
            bool flag = false;
            for(int k=0;k<pi[j].express.size();k++){
                string compare = pi[j].express.substr(k,1);
                if(pi[j].express[k+1]=='\''){
                    compare+="'";
                    k++;
                }
                if(v[i].canon.find(compare)==-1){
                    flag =  true;
                    break;
                }
                else if(v[i].canon[v[i].canon.find(compare)+1]=='\''&&compare.size()==1){
                    flag =  true;
                    break;
                }
            }
        if(!flag)
            v[i].cover.push_back(j);
        }
    }
    return v;
}
int main()
{
    cout << "Enter the number of variables" << endl;
    short vars;
    cin >> vars;
    string variables;
    for(int i=0;i<vars;i++)
        variables.push_back('A'+i);
    vector < vector <minterm> > minterms,aux;
    vector <minterm> primeImplicants;
    vector <minValue> minOnly;
    cout << "Please enter the minterms\nEnter -1 to end" << endl;
    short mintemp;
    cin >> mintemp;
    while(mintemp!=-1){
        if(mintemp>pow(2.0,(double)vars)-1)
            cout << "Can't be represented using " << vars << " variables\nEnter another minterm" << endl;
        else
        minterms=tabularPrep(mintemp,minterms);
        minValue minTemp;
        minTemp.scalar=mintemp;
        minOnly.push_back(minTemp);
        cin >> mintemp;
    }

    cout << "Please enter the don't care\nEnter -1 to end" << endl;
    cin >> mintemp;
    while(mintemp!=-1){
        if(mintemp>pow(2.0,(double)vars)-1)
            cout << "Can't be represented using " << vars << " variables\nEnter another don't care" << endl;
        else{
        minterms=tabularPrep(mintemp,minterms);
        }
        cin >> mintemp;
    }
    primeImplicants=tabularMethod(minterms,aux,primeImplicants);
    primeImplicants=minToExp(variables,primeImplicants,vars);
    minOnly=minCanon(minOnly,variables,vars);
    minOnly=PiTablePrep(minOnly,primeImplicants);
    for(int i=0;i<minOnly.size();i++){
        cout << minOnly[i].scalar << " " << minOnly[i].canon << " ";
        for(int j=0;j<minOnly[i].cover.size();j++)
            cout << minOnly[i].cover[j];
        cout << endl;
    }
    for(int i=0;i<primeImplicants.size();i++){
            cout << primeImplicants[i].values.first << " ";
            for(int k=0;k<primeImplicants[i].values.second.size();k++)
                cout << primeImplicants[i].values.second[k] << " ";
            cout << primeImplicants[i].express << " " << primeImplicants[i].cost ;
            cout << endl;
    }
    return 0;
}
