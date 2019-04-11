#include <iostream>
#include <bits/stdc++.h>
#include <stdbool.h>
#include <math.h>

using namespace std;

struct minValue
{
    short scalar;
    vector <short> cover;
    string canon;
};
struct minterm
{
    pair<short,vector<short> > values ;
    bool picked = false;
    string express;
    short cost;
    vector<short> covers;
    string complements;
};
bool equalVectors(vector <short> v1,vector <short> v2)
{
    if(v1.size()==v2.size())
    {
        for(int i=0; i<v1.size(); i++)
        {
            if(v1[i]!=v2[i])
                return false;
        }
        return true;
    }
    else
        return false;
}
short countOnes(short m)
{
    multiset <short> ones;
    while(m)
    {
        ones.insert(m%2);
        m/=2;
    }
    return ones.count(1);
}
vector < vector <minterm> > tabularPrep(short m, vector < vector <minterm> > v)
{
    short ones = countOnes(m);
    while(ones>=v.size())
        v.push_back(vector<minterm>());
    minterm temp;
    temp.values.first=m;
    v[ones].push_back(temp);
    return v;
}
bool isPowerOfTwo(int n)
{
    return (ceil(log2(n)) == floor(log2(n)));
}
vector < vector <minterm> > falsify(vector < vector <minterm> > v)
{
    for(int i=0; i<v.size(); i++)
    {
        for(int j=0; j<v[i].size(); j++)
        {
            v[i][j].picked=false;
        }
    }
    return v;
}
bool repetition (vector <minterm> v, minterm m)
{
    for(int i=0; i<v.size(); i++)
    {
        if((v[i].values.first==m.values.first)&&(equalVectors(v[i].values.second,m.values.second)))
            return true;
    }
    return false;
}
vector <minterm> tabularMethod(vector < vector <minterm> > v1,vector < vector <minterm> > v2,vector <minterm> v3)
{
    v2.clear();
    v1 = falsify(v1);
    for(int i=0; i<v1.size()-1; i++)
    {
        v2.push_back(vector<minterm>());
        for(int j=0; j<v1[i].size(); j++)
        {
            for(int k=0; k<v1[i+1].size(); k++)
            {
                short diff = v1[i+1][k].values.first - v1[i][j].values.first;
                if(equalVectors(v1[i][j].values.second,v1[i+1][k].values.second)&&diff>0&&isPowerOfTwo(diff))
                {
                    v1[i+1][k].picked=v1[i][j].picked=true;
                    v1[i][j].values.second.push_back(diff);
                    v2[i].push_back(v1[i][j]);
                    v1[i][j].values.second.pop_back();
                }
            }
        }
    }
    for(int i=0; i<v2.size(); i++)
    {
        for(int j=0; j<v2[i].size(); j++)
        {
            sort(v2[i][j].values.second.begin(),v2[i][j].values.second.end());
        }
    }
    bool flag = false;
    for(int i=0; i<v1.size(); i++)
    {
        for(int j=0; j<v1[i].size(); j++)
        {
            if(v1[i][j].picked==true)
            {
                flag = true;
            }
            else
            {
                if(!repetition(v3,v1[i][j]))
                    v3.push_back(v1[i][j]);
            }
        }
    }
    if (!flag)
        return v3;
    return tabularMethod(v2,v1,v3);
}
vector <minterm> minToExp(string s,vector <minterm> v,short m)
{
    for(int i=0; i<v.size(); i++)
    {
        string eliminate;
        for(int j=0; j<v[i].values.second.size(); j++)
        {
            eliminate.push_back(s[s.size()-floor(log2(v[i].values.second[j]))-1]);
        }
        short temp = v[i].values.first;
        for(int j=m-1; j>=0; j--)
        {
            if(eliminate.find(s[j])==-1)
            {
                string temps;
                temps.push_back(s[j]);
                temps+=(temp%2?"":"'");
                v[i].express=temps+v[i].express;
            }
            temp/=2;
        }
        v[i].cost=v[i].express.size()+(v[i].express.size()==1?0:1);
        if(v[i].express.size()==2&&v[i].express[1]=='\'')
            v[i].cost=2;
    }
    return v;
}
vector <minValue> minCanon(vector <minValue> v, string s, short m)
{
    for(int i=0; i<v.size(); i++)
    {
        short temp=v[i].scalar;
        for(int j=m-1; j>=0; j--)
        {
            string temps;
            temps.push_back(s[j]);
            temps+=(temp%2?"":"'");
            v[i].canon=temps+v[i].canon;
            temp/=2;
        }
    }
    return v;
}
vector <minValue> PiTablePrep(vector <minValue> v, vector <minterm> pi)
{
    for(int i=0; i<v.size(); i++)
    {
        v[i].cover.clear();
        for(int j=0; j<pi.size(); j++)
        {
            bool flag = false;
            for(int k=0; k<pi[j].express.size(); k++)
            {
                string compare = pi[j].express.substr(k,1);
                if(pi[j].express[k+1]=='\'')
                {
                    compare+="'";
                    k++;
                }
                if(v[i].canon.find(compare)==-1)
                {
                    flag =  true;
                    break;
                }
                else if(v[i].canon[v[i].canon.find(compare)+1]=='\''&&compare.size()==1)
                {
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
void getComplements (vector <minterm> &pi)
{
    for(int i=0; i<pi.size(); i++)
    {
        string comp;
        for(int j=0; j<pi[i].express.size(); j++)
        {
            if(pi[i].express[j]=='\'')
                comp+=pi[i].express.substr(j-1,2);
        }
        pi[i].complements=comp;
    }
}
void essentials(vector <minValue> &v, vector <minterm> &pi, vector <string> &epi)
{
    for(int i=0; i<v.size(); i++)
    {
        if(v[i].cover.size()==1)
        {
            int index = v[i].cover[0];
            epi.push_back(pi[index].express);
            for(int j=0; j<v.size(); j++)
            {
                for(int k=0; k<v[j].cover.size(); k++)
                {
                    if(v[j].cover[k]==index)
                    {
                        v.erase(v.begin()+j--);
                        i=-1;
                        break;
                    }
                }
            }
            for(int j=0; j<pi.size(); j++)
            {
                bool fnd = false;
                for(int k=0; k<epi.size(); k++)
                {
                    if(epi[k]==pi[j].express)
                        fnd = true;
                }
                if(!fnd)
                {
                    for(int k=0; k<pi[j].complements.size(); k+=2)
                    {
                        if(pi[index].complements.find(pi[j].complements.substr(k,2))!=-1)
                        {
                            pi[j].cost--;
                            pi[j].complements.erase(pi[j].complements.begin()+k);
                            pi[j].complements.erase(pi[j].complements.begin()+k);
                            k-=2;
                        }
                    }
                }
            }
        }
    }
}
void rowDominancePrep(vector <minValue> &v, vector <minterm> &pi)
{
    for(int i=0; i<pi.size(); i++)
    {
        pi[i].covers.clear();
        for(int j=0; j<v.size(); j++)
        {
            for(int k=0; k<v[j].cover.size(); k++)
            {
                if(v[j].cover[k]==i)
                {
                    pi[i].covers.push_back(v[j].scalar);
                    sort(pi[i].covers.begin(),pi[i].covers.end());
                }
            }
        }
    }
}
void rowDominance(vector <minterm> &pi)
{
    for(int i=0; i<pi.size(); i++)
    {
        for(int j=0; j<pi.size(); j++)
        {
            if(pi[i].covers.size() >= pi[j].covers.size() && pi[j].cost >= pi[i].cost && i!=j)
            {
                string temp1;
                for(int k=0; k<pi[i].covers.size(); k++)
                {
                    temp1+=(char)(pi[i].covers[k]);
                }
                string temp2;
                for(int k=0; k<pi[j].covers.size(); k++)
                {
                    temp2+=(char)(pi[j].covers[k]);
                }
                if(temp1.find(temp2)!=-1)
                {
                    pi.erase(pi.begin()+j);
                    j--;
                    if(i)
                    {
                        i--;
                    }

                }
            }
        }
    }
}
void columnDominance(vector <minValue> &v)
{
    for(int i=0; i<v.size(); i++)
    {
        for(int j=0; j<v.size(); j++)
        {
            if(v[i].cover.size() <= v[j].cover.size() && i!=j)
            {
                string temp1;
                for(int k=0; k<v[i].cover.size(); k++)
                {
                    temp1+=(char)(v[i].cover[k]);
                }
                string temp2;
                for(int k=0; k<v[j].cover.size(); k++)
                {
                    temp2+=(char)(v[j].cover[k]);
                }
                if(temp2.find(temp1)!=-1)
                {
                    v.erase(v.begin()+j);
                    j--;
                    if(i)
                    {
                        i--;
                    }
                }
            }
        }
    }
}
void petrick(vector <minValue> &v, vector <minterm> &pi, vector <string> &epi)
{
    short cost = 5000;
    short counter = 0;
    vector <short> trial;
    vector <string> minCost;
    short divisor;
    while(1)
    {
        trial.clear();
        trial.push_back(v[0].cover[counter-(counter/v[0].cover.size())*v[0].cover.size()]);
        for(int i=1; i<v.size(); i++)
        {
            divisor = 1;
            for(int j=0; j<i; j++)
            {
                divisor*=v[j].cover.size();
            }
            short counterX = counter/divisor;
            short temp = v[i].cover[counterX-(counterX/v[i].cover.size())*v[i].cover.size()];
            bool fnd = false;
            for(int j=0; j<trial.size(); j++)
            {
                if(temp == trial[j])
                {
                    fnd = true;
                    break;
                }
            }
            if(!fnd)
                trial.push_back(temp);
        }
        int trialCost=0;
        string trialComp;
        for(int i=0; i<trial.size(); i++)
        {
            trialCost+=pi[trial[i]].cost;
            trialComp+=pi[trial[i]].complements;
        }
        sort(trialComp.begin(),trialComp.end());
        for(int i=1; i< trialComp.size(); i++)
        {
            if(trialComp[i]==trialComp[i-1] && trial[i]!='\'')
                trialCost--;
        }
        if(trialCost<cost)
        {
            cost=trialCost;
            minCost.clear();
            for(int i=0; i<trial.size(); i++)
            {
                minCost.push_back(pi[trial[i]].express);
            }
        }
        counter++;
        divisor = 1;
        for(int j=0; j<v.size(); j++)
        {
            divisor*=v[j].cover.size();
        }
        if(counter == divisor)
            break;
    }
    for(int i=0; i<minCost.size(); i++)
        epi.push_back(minCost[i]);
}
void piTable(vector <minValue> &v, vector <minterm> &pi, vector <string> &epi)
{
    int sizeV = v.size();
    int sizeP = pi.size();
    v=PiTablePrep(v,pi);
    essentials(v,pi,epi);
    rowDominancePrep(v,pi);
    rowDominance(pi);
    columnDominance(v);
    if(sizeV-v.size() || sizeP-pi.size())
    {
        piTable(v,pi,epi);
    }
    else if(v.size())
    {
        petrick(v,pi,epi);
    }
}
int main()
{
    cout << "Enter the number of variables" << endl;
    short vars;
    cin >> vars;
    string variables;
    for(int i=0; i<vars; i++)
        variables.push_back('A'+i);
    vector < vector <minterm> > minterms,aux;
    vector <minterm> primeImplicants;
    vector <string> essentialPIs;
    vector <minValue> minOnly;
    cout << "Please enter the minterms separated by a space or a new line\nEnter -1 to end" << endl;
    short mintemp;
    cin >> mintemp;
    while(mintemp!=-1)
    {
        if(mintemp>pow(2.0,(double)vars)-1)
        {
            cout << mintemp << " can't be represented using " << vars << " variables\nEnter another minterm" << endl;
            cin >> mintemp;
        }
        else
        {
            minterms=tabularPrep(mintemp,minterms);
            minValue minTemp;
            minTemp.scalar=mintemp;
            minOnly.push_back(minTemp);
            cin >> mintemp;
        }
    }
    cout << "Please enter the don't care separated by a space or a new line\nEnter -1 to end" << endl;
    cin >> mintemp;
    while(mintemp!=-1)
    {
        if(mintemp>pow(2.0,(double)vars)-1)
            cout << "Can't be represented using " << vars << " variables\nEnter another don't care" << endl;
        else
        {
            minterms=tabularPrep(mintemp,minterms);
        }
        cin >> mintemp;
    }
    primeImplicants=tabularMethod(minterms,aux,primeImplicants);
    primeImplicants=minToExp(variables,primeImplicants,vars);
    minOnly=minCanon(minOnly,variables,vars);
    getComplements(primeImplicants);
    piTable(minOnly,primeImplicants,essentialPIs);
    cout << "F(" <<variables[0];
    for(int i=1; i<variables.size(); i++)
        cout << "," << variables[i];
    cout << ") = " << essentialPIs[0];
    for(int i=1; i<essentialPIs.size(); i++)
        cout << " + " << essentialPIs[i];
    return 0;
}
