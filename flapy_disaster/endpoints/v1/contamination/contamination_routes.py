# Imports for routing
import flask as fl
from app import app
import globes as gb

@app.route('/test/contamination_test')
def contamination_test():
    print("hello contamination")
    return fl.render_template('html/HomePage.html')
