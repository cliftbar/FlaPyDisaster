"""
Generate the Anaconda and Pip requirements files for the current environment.  Use this when adding a new python library dependency.
Ensure that it can be installed in both Pip and Anaconda
"""
import os


os.system("conda list --explicit > conda_requirements.txt")
os.system("pip freeze > requirements.txt")
