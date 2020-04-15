"""
Generate the Anaconda and Pip requirements files for the current environment.  Use this when adding a new python library dependency.
Ensure that it can be installed in both Pip and Anaconda
"""
import os
import sys

platform = sys.platform


def generate():
    os.system(f"conda list --export > conda_requirements_{platform}.txt")
    os.system(f"conda list --explicit > conda_requirements_explicit_{platform}.txt")
    os.system(f"conda env export --name $CONDA_DEFAULT_ENV --file conda_environment_{platform}.yml")


def install():
    os.system(f"conda install -y --file conda_requirements_{platform}.txt")


def install_explicit():
    os.system(f"conda install -y --file conda_requirements_explicit_{platform}.txt")


def install_environment():
    os.system(f"conda env create --file conda_environment_{platform}.yml")


def print_help():
    print("Usage:")
    print("\t$ python requirements.py [option]")
    print("Options:")
    print("\tgenerate")
    print("\tinstall")
    print("\tinstall-explicit")
    print("\tinstall-environment (preferred)")


def main():
    task = sys.argv[1] if len(sys.argv) > 1 else None
    print(f"task is {task}")
    if task == 'generate':
        generate()
    elif task == 'install':
        install()
    elif task == 'install-explicit':
        install_explicit()
    elif task == 'install-environment':
        install_environment()
    else:
        print_help()


if __name__ == "__name__":
    main()

