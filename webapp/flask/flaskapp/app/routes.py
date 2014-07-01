from flask import Flask, render_template
from time import sleep
import threading


def waitfn(arg):
    for i in range(arg):
        with open('foo.txt', 'a') as f:
            f.write("i {0}\n".format(i))
        sleep(2)

app = Flask(__name__)


@app.route('/')
def home():
    return render_template('home.html')


@app.route('/about')
def about():
    return render_template('about.html')


def run_server():
    thread = threading.Thread(target=waitfn, args=(10, ))
    thread.start()
    app.run(debug=True, use_reloader=False)
    thread.join()


if __name__ == '__main__':
    run_server()
