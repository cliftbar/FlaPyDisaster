"""
Generate the Anaconda and Pip requirements files for the current environment.  Use this when adding a new python library dependency.
Ensure that it can be installed in both Pip and Anaconda
"""
import os
import sys

platform = sys.platform
os.system("conda list --export > conda_requirements_" + platform + ".txt")
os.system("conda list --explicit > conda_requirements_explicit_" + platform + ".txt")
#os.system("pip freeze > requirements_" + platform + ".txt")
