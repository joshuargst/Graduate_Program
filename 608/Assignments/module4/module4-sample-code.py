#!/usr/bin/env python
# coding: utf-8

# In[1]:


import pandas as pd
import numpy as np

# This module we'll be looking at the New York City tree census. This data was provided by a volunteer driven census in 2015, and we'll be accessing it via the socrata API. The main site for the data is [here](https://data.cityofnewyork.us/Environment/2015-Street-Tree-Census-Tree-Data/uvpi-gqnh), and on the upper right hand side you'll be able to see the link to the API.
# 
# The data is conveniently available in json format, so we should be able to just read it directly in to Pandas:

# In[2]:


url = 'https://data.cityofnewyork.us/resource/nwxe-4ae8.json'
trees = pd.read_json(url)
trees.head(10)


# Looks good, but lets take a look at the shape of this data:

# In[3]:


trees.shape


# 1000 seems like too few trees for a city like New York, and a suspiciously round number. What's going on?
# 
# Socrata places a 1000 row limit on their API. Raw data is meant to be "paged" through for applications, with the expectation that a UX wouldn't be able to handle a full dataset. 
# 
# As a simple example, if we had a mobile app with limited space that only displayed trees 5 at a time, we could view the first 5 trees in the dataset with the url below:

# In[4]:


firstfive_url = 'https://data.cityofnewyork.us/resource/nwxe-4ae8.json?$limit=5&$offset=0'
firstfive_trees = pd.read_json(firstfive_url)
firstfive_trees


# If we wanted the next 5, we would use this url:

# In[5]:


nextfive_url = 'https://data.cityofnewyork.us/resource/nwxe-4ae8.json?$limit=5&$offset=5'
nextfive_trees = pd.read_json(nextfive_url)
nextfive_trees


# You can read more about paging using the Socrata API [here](https://dev.socrata.com/docs/paging.html)
# 
# In these docs, you'll also see more advanced functions (called `SoQL`) under the "filtering and query" section. These functions should be reminding you of SQL.
# 
# Think about the shape you want your data to be in before querying it. Using `SoQL` is a good way to avoid the limits of the API. For example, using the below query I can easily obtain the count of each species of tree in the Bronx:


for x in range(0, 5000, 1000):
    soql_url = ('https://data.cityofnewyork.us/resource/nwxe-4ae8.json?$limit=1000&$offset=' + str(x) +\
        '&$select=boroname,spc_common,health,steward,count(tree_id)' +\
        '&$group=boroname,spc_common,health,steward').replace(' ', '%20')
    soql_trees = pd.read_json(soql_url)
    if(x==0):
        df = pd.DataFrame(columns=list(soql_trees.columns.values))
    df = df.append(soql_trees)


df = df.reset_index(drop=True)



df_totals = df.groupby(['boroname', 'spc_common'])['count_tree_id'].sum()
df_total_by_boroname_specie_health = df.groupby(['boroname', 'spc_common', 'health'])['count_tree_id'].sum()
df_totals = df_totals.reset_index(drop=False)
df_total_by_boroname_specie_health = df_total_by_boroname_specie_health.reset_index(drop=False)
df_totals.columns = ['boroname', 'spc_common', 'total_for_specie_in_borough']
df_total_by_boroname_specie_health.columns = ['boroname', 'spc_common', 'health', 'total']
tree_proportions = pd.merge(df_total_by_boroname_specie_health, df_totals, on=['boroname', 'spc_common'])
tree_proportions['ratio'] = tree_proportions['total']/ tree_proportions['total_for_specie_in_borough']
tree_proportions['spc_common'] = tree_proportions['spc_common'].apply(lambda x: x.title())

species = np.sort(tree_proportions.spc_common.unique())


df_total_by_steward = df.groupby(['boroname', 'spc_common', 'steward'])['count_tree_id'].sum()
df_total_by_steward = df_total_by_steward.reset_index(drop=False)
df_total_by_steward.columns = ['boroname', 'spc_common', 'steward', 'steward_total']
df_steward = pd.merge(df, df_total_by_steward, on=['boroname', 'spc_common', 'steward'])
di = {'Poor':1, 'Fair':2, 'Good':3}
df_steward['health_level'] = df_steward['health'].map(di)
#df_steward.sort_values(by=['boroname', 'spc_common', 'steward']).head(10)
df_steward['health_index'] = (df_steward['count_tree_id']/df_steward['steward_total']) * df_steward['health_level']
#df_steward.sort_values(by=['boroname', 'spc_common', 'steward']).head(10)
df_overall_health_index = df_steward.groupby(['boroname', 'spc_common', 'steward'])['health_index'].sum()
df_overall_health_index = df_overall_health_index.reset_index(drop=False)
df_overall_health_index.columns = ['boroname', 'spc_common', 'steward', 'overall_health_index']
di2 = {'3or4':3, '4orMore':4, 'None':1, '1or2':2}
df_overall_health_index['steward_level'] = df_overall_health_index['steward'].map(di2)
df_overall_health_index['spc_common'] = df_overall_health_index['spc_common'].apply(lambda x: x.title())



# This behavior is very common with web APIs, and I think this is useful when thinking about building interactive data products. When in a Jupyter Notebook or RStudio, there's an expectation that (unless you're dealing with truly large datasets) the data you want can be brought in memory and manipulated.
# 
# Dash and Shiny abstract away the need to distinguish between client side and server side to make web development more accessible to data scientists. This can lead to some unintentional design mistakes if you don't think about how costly your callback functions are (for example: nothing will stop you in dash from running a costly model triggered whenever a dropdown is called.)
# 
# The goal of using the Socrata is to force you to think about where your data operations are happening, and not resort to pulling in the data and performing all operations in local memory.
# 
# ----------
# 
# **NOTE**: One tip in dealing with URLs: you may need to replace spaces with `'%20'`. I personally just write out the url and then follow the string with a replace:



'https://api-url.com/?query with spaces'.replace(' ', '%20')


'''
DashApp
'''

import dash
import dash_core_components as dcc
import dash_html_components as html
from dash.dependencies import Input, Output

import plotly.graph_objs as go



external_stylesheets = ['https://codepen.io/chriddyp/pen/bWLwgP.css']

app = dash.Dash(__name__, external_stylesheets=external_stylesheets)

app.layout = html.Div([
    html.H1("Joshua Registe - Module 4"),
    html.H6('The following figure answers the question "what proportion of trees are in good, fair, or poor health according to the health variable? The proportions below are plotted by borough and you can check this by species type via the filter. For example, Black Maples show that most of them are in healthy condition in the Bronx while a greater proportion of them are in fair or poor condition in Brooklyn and Staten Island, but still most of them are in good health'),
        html.H6('Use drop-down to select tree species of interest'),
    
    dcc.Dropdown(
        id='specie', 
        options=[{'label': i, 'value': i} for i in species],
        value="Black Maple",
        style={'height': 'auto', 'width': '300px'}
    ),

    dcc.Graph(id='graph-ratio'),
    html.H6('The following figure answers the question "Are Stewards (steward activity measured by the steward variable) having an impact on the health of trees?" You can now assess whether or not the number of stewards have an effect on the health index of each plant. For Example, you can see that for the Black Maple, there is essentially no benefit for each additional steward'),

    dcc.Graph(id='graph-health')

], style={'columnCount': 1})

#Display Proportion Graph 
@app.callback(
    Output('graph-ratio', 'figure'),
    [Input('specie', 'value')])
def update_figure(selected_specie):

    filtered_df = tree_proportions[tree_proportions.spc_common == selected_specie]
    #borocode: 1 (Manhattan), 2 (Bronx), 3 (Brooklyn), 4 (Queens), 5 (Staten Island)

    good = filtered_df[filtered_df.health=="Good"]
    poor = filtered_df[filtered_df.health=="Poor"]
    fair = filtered_df[filtered_df.health=="Fair"]
    traces = []
    
    traces.append(go.Bar(
    x=good['boroname'],
    y=good['ratio'],
    name='Good',
    opacity=0.5
    ))
    
    traces.append(go.Bar(
    x=poor['boroname'],
    y=poor['ratio'],
    name='Poor',
    opacity=0.5
    ))
        
    traces.append(go.Bar(
    x=fair['boroname'],
    y=fair['ratio'],
    name='Fair',
    opacity=0.5
    ))
            
    return {
        'data': traces,
        'layout': go.Layout(
            xaxis={'title': 'Borough'},
            yaxis={'title': 'Proportion of Trees in Borough'},
            margin={'l': 40, 'b': 40, 't': 10, 'r': 10},
            legend=dict(x=-.1, y=1.2)
        )
    }


#Display Steward-Health Graph for Question 2
@app.callback(
    Output('graph-health', 'figure'),
    [Input('specie', 'value')])
def update_figure2(selected_specie):
    #print('here: ' + selected_specie)
    filtered_df = df_overall_health_index[df_overall_health_index.spc_common == selected_specie]
    
    oneortwo = filtered_df[filtered_df.steward=="1or2"]
    threeorfour = filtered_df[filtered_df.steward=="3or4"]
    fourormore = filtered_df[filtered_df.steward=="4orMore"]
    exactlynone = filtered_df[filtered_df.steward=="None"]
    
    traces2 = []
        
    
    traces2.append(go.Bar(
    x=exactlynone['boroname'],
    y=exactlynone['overall_health_index'],
    name='No Stewards',
    opacity=0.5
    ))
        
    traces2.append(go.Bar(
    x=oneortwo['boroname'],
    y=oneortwo['overall_health_index'],
    name='One-Two Stewards',
    opacity=0.5
    ))
    
    traces2.append(go.Bar(
    x=threeorfour['boroname'],
    y=threeorfour['overall_health_index'],
    name='Three-Four Stewards',
    opacity=0.5
    ))
        
    traces2.append(go.Bar(
    x=fourormore['boroname'],
    y=fourormore['overall_health_index'],
    name='Four + Stewards',
    opacity=0.5
    ))
        
    
    return {
        'data': traces2,
        'layout': go.Layout(
            xaxis={'title': 'Borough'},
            yaxis={'title': 'Overall Health Index'},
            margin={'l': 40, 'b': 40, 't': 10, 'r': 10},
            legend=dict(x=-.1, y=1.2)
        )
    }


if __name__ == '__main__':
    app.run_server(debug=False)




