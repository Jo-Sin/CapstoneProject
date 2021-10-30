import dash_core_components as dcc
from dash_core_components.Dropdown import Dropdown
import dash_html_components as html
from dash.dependencies import Input, Output
import plotly.graph_objs as go
from django_plotly_dash import DjangoDash
import pandas as pd
import plotly.express as px

external_stylesheets = ['https://codepen.io/chriddyp/pen/bWLwgP.css']

app = DjangoDash('VisualApp', external_stylesheets=external_stylesheets)



data = pd.read_csv("static/final.csv")
features = data.columns.to_list()

fig = px.histogram(data, x=features[1])

bar1 = px.bar(data,
    x="Country",
    y=["Yearly Pre-Intervention Rate","Yearly Post Intervention Rate"],
    title="Yearly Suicide Counts by Country",
    barmode="group")
bar2 = px.bar(data,
    x="Country",
    y=["Pre-Intervention Suicide","Post-Intervention Suicide"],
    title="Total Suicide Counts by Country",
    barmode="group")
bar3 = px.bar(data,
    x="Phones",
    y=["Yearly Pre-Intervention Rate","Yearly Post Intervention Rate"],
    title="Phones")
bar4 = px.bar(data,
    x="Signs with Emergency Contact",
    y=["Yearly Pre-Intervention Rate","Yearly Post Intervention Rate"],
    title="Signs with Emergency Contact")
bar5 = px.bar(data,
    x="CCTV",
    y=["Yearly Pre-Intervention Rate","Yearly Post Intervention Rate"],
    title="CCTV")
bar6 = px.bar(data,
    x="Safety Staff",
    y=["Yearly Pre-Intervention Rate","Yearly Post Intervention Rate"],
    title="Safety Staff")
bar7 = px.bar(data,
    x="Blue Lights",
    y=["Yearly Pre-Intervention Rate","Yearly Post Intervention Rate"],
    title="Blue Lights")

pie1 = px.pie(data,
    title="Yearly Pre-Intervention Suicides by Location",
    names="Location Type",
    values="Yearly Pre-Intervention Rate")
pie2 = px.pie(data,
    title="Yearly Post-Intervention Suicides by Location",
    names="Location Type",
    values="Yearly Post Intervention Rate")
pie3 = px.pie(data,
    title="Yearly Pre-Intervention Suicides by Method",
    names="Suicide Method",
    values="Yearly Pre-Intervention Rate")
pie4 = px.pie(data,
    title="Yearly Post-Intervention Suicides by Method",
    names="Suicide Method",
    values="Yearly Post Intervention Rate")


app.layout = html.Div(children=[
    html.Div(
        children=[
            html.Div(
                children=[       
                    dcc.Graph(
                        id="main-graph",
                        figure=fig
                    ),
                ],
                className="nine columns"
            ),
            html.Div(
                children=[
                    html.Br(),
                    html.Br(),
                    html.Label("Select Feature"),
                    dcc.Dropdown(
                        id="feature-select",
                        options=[
                            {'label':x, 'value': x} for x in features
                        ],
                        value=features[1],
                        clearable=False
                    ),
                    html.Br(),
                    html.Label("Select Graph type"),
                    dcc.Dropdown(
                        id="graph-select",
                        options=[
                            {'label':'Histogram', 'value': 'hist'},
                            {'label':'Scatterplot', 'value': 'scatter'},
                            {'label':'Boxplot', 'value':'box'}
                        ],
                        value='hist',
                        clearable=False
                    ),
                ],
                className="three columns"
            ),
        ],
        className="row"
    ),
    html.Hr(),
    html.Div(
        children=[
            html.Div(
                children=[
                    dcc.Graph(
                        id="pie1",
                        figure=pie1
                    ),
                ],
                className="six columns"
            ),
            html.Div(
                children=[
                    dcc.Graph(
                        id="pie2",
                        figure=pie2
                    ),
                ],
                className="six columns"
            ),
        ],
        className="row"
    ),
    html.Div(
        children=[
            html.Div(
                children=[
                    dcc.Graph(
                        id="pie3",
                        figure=pie3
                    ),
                ],
                className="six columns"
            ),
            html.Div(
                children=[
                    dcc.Graph(
                        id="pie4",
                        figure=pie4
                    ),
                ],
                className="six columns"
            ),
        ],
        className="row"
    ),
    html.Hr(),
    dcc.Graph(
        id="bar1",
        figure=bar1
    ),
    dcc.Graph(
        id="bar2",
        figure=bar2
    ),
    html.Hr(),
    html.Div(
        children=[
            html.Div(
                children=[
                    dcc.Graph(
                        id="bar3",
                        figure=bar3
                    ),
                ],
                className="six columns"
            ),
            html.Div(
                children=[
                    dcc.Graph(
                        id="bar4",
                        figure=bar4
                    ),
                ],
                className="six columns"
            ),
        ],
        className="row"
    ),
    html.Div(
        children=[
            html.Div(
                children=[
                    dcc.Graph(
                        id="bar5",
                        figure=bar5
                    ),
                ],
                className="six columns"
            ),
            html.Div(
                children=[
                    dcc.Graph(
                        id="bar6",
                        figure=bar6
                    ),
                ],
                className="six columns"
            ),
        ],
        className="row"
    ),
    html.Div(
        children=[
            html.Div(
                children=[
                    dcc.Graph(
                        id="bar7",
                        figure=bar7
                    ),
                ],
                className="six columns"
            ),
        ],
        className="row"
    ),
])


@app.callback(
    Output("main-graph","figure"),
    [Input("feature-select", "value"), Input("graph-select","value")]
)
def UpdateGraph(feature, graph):
    if graph == 'hist':
        fig2 = px.histogram(data, x=feature)
    elif graph == 'scatter':
        fig2 = px.scatter(data, x=feature)
    else:
        fig2 = px.box(data, x=feature)
    fig2.update_layout(
        title="Graph: "+feature
    )
    return fig2