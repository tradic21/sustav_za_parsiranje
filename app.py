from flask import Flask, render_template, request
import subprocess
import json

app = Flask(__name__)

def izvrsi_upit(upit):
    try:
        prolog_query = f"consult('relacijska_algebra.pl'), izvrsi_upit({json.dumps(upit)}, Rezultat), halt."
        cmd = ['swipl', '-q', '-g', prolog_query]
        rezultat = subprocess.run(
            cmd,
            capture_output=True, text=True
        )
        print("RAW OUTPUT:", rezultat.stdout.strip())
        if not rezultat.stdout.strip():
            return []
        try:
            podaci = json.loads(rezultat.stdout.strip().replace("'", '"'))
            return podaci
        except Exception as e:
            print("Greška pri parsiranju JSON-a:", e)
            return []
    except Exception as e:
        return str(e)

def get_tablice():
    studenti = izvrsi_upit("STUDENT")
    prijave = izvrsi_upit("PRIJAVA")
    return studenti, prijave

@app.route('/', methods=['GET', 'POST'])
def index():
    rezultat = None
    studenti, prijave = get_tablice()  
    if request.method == 'POST':
        upit = request.form.get('upit')
        rezultat = izvrsi_upit(upit)
    return render_template('index.html', rezultat=rezultat, studenti=studenti, prijave=prijave)

if __name__ == '__main__':
    app.run(debug=True)
