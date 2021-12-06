from flask import Flask, render_template, request
import pandas as pd
import numpy as np
import pathlib

app = Flask(__name__, template_folder='template')


@app.route('/')
def home():
    return render_template('home.html')


@app.route('/visualizacion',
           methods=["GET", "POST"])  ##{{ url_for('viasializacion')}}
def viasializacion():

    df = pd.read_csv(str(pathlib.Path().absolute()) + "/db/db-acc-med.csv",
                     sep=";",
                     encoding='unicode_escape')
    clases = df["CLASE"].sort_values()
    clases = clases.drop_duplicates().values
    rows = df.head(n=10).values

    if request.method == 'POST':
        Fini = request.form["Dstart"]
        Fend = request.form["Dend"]
        typeacc = request.form["typeAcc"]

        df["FECHA2"] = pd.to_datetime(df["FECHA"], format="%d/%m/%Y")

        date1 = pd.to_datetime(Fini, format="%Y/%m/%d")
        date2 = pd.to_datetime(Fend, format="%Y/%m/%d")
        filtro1 = df["FECHA2"] >= date1
        filtro2 = df["FECHA2"] <= date2
        filtro = filtro1 & filtro2

        if (typeacc == "NA"):
            rows = df[filtro]
        else:
            tacc = df["CLASE"] == typeacc
            filtro = filtro & tacc
            rows = df[filtro]

        rows = rows.values

    return render_template('/visual.html', datos=rows, op=clases)


@app.route('/prediccion', methods=["GET",
                                   "POST"])  ##{{ url_for('prediccion')}}
def prediccion():
    return render_template('predic.html')


@app.route('/referencias')
def referencias():
    return render_template('ref.html')


@app.route('/agrupacion', methods=["GET", "POST"])
def agrupacion():

    df_freq = pd.read_csv(str(pathlib.Path().absolute()) + "/db/freq.csv",
                          sep=",",
                          encoding='unicode_escape')

    df2 = pd.read_csv(str(pathlib.Path().absolute()) +
                               "/db/freq2.csv",
                               sep=";",
                               encoding='unicode_escape')

    taball = df_freq.head(n=10).values
    TGravedad = df2.head(n=10).values

    op2 = df2["Var1"].sort_values()
    op2 = op2.drop_duplicates().values

    op1 = df2["Var2"].sort_values()
    op1 = op1.drop_duplicates().values

    TGravedad = df2.head(n=10).values

    img = "/static/src/Atropello Con heridos.svg"


    if request.method == "POST":
        clase = request.form["Tclass2"]
        gravedad = request.form["gravedad"]

        TGravedad = df2[df2["Var2"] == clase]
        TGravedad = TGravedad[TGravedad["Var1"] == gravedad]
        TGravedad = TGravedad.values

        img = "/static/src/"+clase + " " + gravedad+".svg"
        

    return render_template('agrup.html', all=taball, op1=op1, op2=op2, com1 =TGravedad, src=img)

if __name__ == '__main__':
    app.run(debug=True)
