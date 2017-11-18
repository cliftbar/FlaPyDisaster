#include <Python.h>
#include <Windows.h>
#include <cmath>
#include "kernel.h"

PyObject* hello() {
    return PyFloat_FromDouble(42);
}

static PyMethodDef FlaPyPlusPlus_methods[] = {
    // The first property is the name exposed to python, the second is the C++ function name        
    { "cpp_hello", (PyCFunction)hello, METH_NOARGS, nullptr },

    // Terminate the array with an object containing nulls.
    { nullptr, nullptr, 0, nullptr }
};

static PyModuleDef FlaPyPlusPlus_module = {
    PyModuleDef_HEAD_INIT,
    "FlaPyPlusPlus",                        // Module name
    "Provides some functions, but faster",  // Module description
    0,
    FlaPyPlusPlus_methods                   // Structure that defines the methods
};

PyMODINIT_FUNC PyInit_FlaPyPlusPlus() {
    return PyModule_Create(&FlaPyPlusPlus_module);
}