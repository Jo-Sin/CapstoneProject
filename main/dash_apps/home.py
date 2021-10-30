import math
import dash_core_components as dcc
import dash_html_components as html
from dash.dependencies import Input, Output, State
import plotly.graph_objs as go
from django_plotly_dash import DjangoDash
import pandas as pd
import plotly.express as px

external_stylesheets = ['https://codepen.io/chriddyp/pen/bWLwgP.css']

app = DjangoDash('HomeApp', external_stylesheets=external_stylesheets)


LOCATION_OPTIONS = {
    'height': [
        {'label':'All', 'value': 'all'},
        {'label':'Bridge', 'value': 'bridge'},
    ],
}

INTERVENTIONS = {
    'height-all': [
        {'label':'Physical barriers', 'value': 'barrier'},
        {'label':'Physical barriers and Help Seeking', 'value': 'barrier+help'},
    ],
    'height-bridge': [
        {'label':'Physical barriers', 'value': 'barrier'},
        {'label':'Physical barriers and Help Seeking', 'value': 'barrier+help'},
    ],
}

yearlyNumber = 1.0

data = pd.read_csv("static/height-all-barrier.csv")
postData = data.multiply(yearlyNumber)

fig1 = px.histogram(data, x="IRR_mean", histnorm='probability density', title="Histogram of IRRs")
fig2 = px.histogram(postData, x="IRR_mean",
        title="Suicides post-intervention",
        histnorm='probability density',
        color_discrete_sequence=['indianred'])

app.layout = html.Div(children=[
    html.H2("Bootstrapping"),
    html.Div(
        children=[
            html.Div(
                children=[
                    html.Label("Suicide Methods"),
                    dcc.Dropdown(
                        id="suicide-method",
                        options=[
                            {'label':'Jumping from height', 'value': 'height'},
                        ],
                        value="height",
                        clearable=False
                    )
                ],
                className="four columns"
            ),
            html.Div(
                children=[
                    html.Label("Location Type"),
                    dcc.Dropdown(
                        id="location-type",
                        options=[
                            {'label':'All', 'value': 'all'},
                            {'label':'Bridge', 'value': 'bridge'},
                        ],
                        value="all",
                        clearable=False
                    )
                ],
                className="four columns"
            ),
            html.Div(
                children=[
                    html.Label("Intervention Type"),
                    dcc.Dropdown(
                        id="intervention-type",
                        options=[
                            {'label':'Physical barriers', 'value': 'barrier'},
                        ],
                        value="barrier",
                        clearable=False
                    )
                ],
                className="four columns"
            ),
        ],
        className="row"
    ),
    html.Br(),
    html.Div(
        children=[
            html.Div(
                children=[
                    html.Label("Average yearly number of suicides pre-intervention"),
                    dcc.Input(
                        id="avg-number",
                        type="number",
                        value="1.0"
                    )
                ],
                className="eight columns"
            ),
        ],
        className="row"
    ),
    html.Div(
        children=[
            html.Div(
                children=[
                    dcc.Graph(
                        id="graph1",
                        figure=fig1
                    )
                ],
                className="six columns"
            ),
            html.Div(
                children=[
                    dcc.Graph(
                        id="graph2",
                        figure=fig2
                    )
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
                    html.Center(id="irr-mean",children=""),
                    html.Center(id="irr-ci",children=""),
                    html.Center(id="irr-text",children="IRR represents the ratio of post-intervention to pre-intervention yearly suicide rates"),
                ],
                className="six columns"
            ),
            html.Div(
                children=[
                    html.Center(id="post-irr-mean",children=""),
                    html.Center(id="post-irr-ci",children="")
                ],
                className="six columns"
            ),
        ],
        className="row"
    ),
    html.Hr(),
    # html.Div(children="blah blah", id="sample-text")
])



@app.callback(
    [Output("graph2","figure"),Output("post-irr-mean","children"),Output("post-irr-ci","children")],
    [Input("avg-number", "value"), Input("graph1","figure")],
    [State("intervention-type", "value"),State("location-type","value"),State("suicide-method","value")]
)
def UpdatePostData(avgNum, img, intervention,location,method):
    refkey = method+"-"+location+"-"+intervention

    data = pd.read_csv("static/"+refkey+".csv")
    yearlyNumber = float(avgNum)

    postData = data.multiply(yearlyNumber)
    
    fig2 = px.histogram(postData, x="IRR_mean",
        title="Post-intervention suicide prediction",
        histnorm='probability density',
        color_discrete_sequence=['indianred'])
    fig2.update_layout(xaxis_title="Yearly number of suicides")

    m, c, s = postData["IRR_mean"].agg(['mean','count','std'])
    hi = m + 1.95*s/math.sqrt(c)
    lo = m - 1.95*s/math.sqrt(c)

    postMean = round(m,5)
    hi = round(hi,3)
    lo = round(lo,3)

    meanText = "Mean: {}".format(postMean)
    ciText = "Confidence Interval: ({} - {})".format(lo,hi)
    return fig2, meanText, ciText


@app.callback(
    [Output("location-type","options"), Output("location-type", "value")],
    Input("suicide-method","value")
)
def UpdateSuicide(method):
    return LOCATION_OPTIONS[method], LOCATION_OPTIONS[method][0]["value"]

@app.callback(
    [Output("intervention-type","options"), Output("intervention-type", "value")],
    Input("location-type","value"),
    State("suicide-method","value")
)
def UpdateSuicide(location, method):
    refkey = method+"-"+location
    return INTERVENTIONS[refkey], INTERVENTIONS[refkey][0]["value"]

# @app.callback(
#     Output("graph1","figure"),
#     [Input("intervention-type", "value"),Input("location-type","value"),Input("suicide-method","value")]
# )
# def UpdateIntervention(intervention,location,method):
#     refkey = method+"-"+location+"-"+intervention

#     data = pd.read_csv("static/"+refkey+".csv")

#     fig1 = px.histogram(data, x="IRR_mean", histnorm='probability density', title="Histogram of IRRs")
#     return fig1

@app.callback(
    [Output("irr-mean","children"), Output("irr-ci","children"), Output("graph1","figure")],
    [Input("intervention-type", "value"),Input("location-type","value"),Input("suicide-method","value")]
)
def UpdateIntervention(intervention,location,method):
    refkey = method+"-"+location+"-"+intervention

    data = pd.read_csv("static/"+refkey+".csv")

    m, c, s = data["IRR_mean"].agg(['mean','count','std'])
    hi = m + 1.95*s/math.sqrt(c)
    lo = m - 1.95*s/math.sqrt(c)

    postMean = round(m,5)
    hi = round(hi,3)
    lo = round(lo,3)

    fig1 = px.histogram(data, x="IRR_mean", histnorm='probability density', title="Histogram of IRRs")
    fig1.update_layout(xaxis_title="IRR")

    meanText = "Mean: {}".format(postMean)
    ciText = "Confidence Interval: ({} - {})".format(lo,hi)
    return meanText, ciText, fig1